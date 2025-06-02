library(shiny)
library(leaflet)
library(raster)
library(viewscape)
library(sf)
library(dplyr)
library(terra)
library(geodata)
library(tidygeocoder)
library(tmap)
library(mapview)

if (!dir.exists("data")) {
  unzip("data.zip")
}

# paths
setwd("data")

load_municipality <- function(municipality) {
  # load municipality polygon from geodata
  munic_polygon <- gadm(country = "DNK", path = ".", level=2) %>%
    st_as_sf() %>%
    filter(NAME_2==municipality) %>%
    st_transform(25832)
  return(munic_polygon)
}

load_cell_towers <- function(munic_polygon) {
  # load cell towers as vectors and filter to munic
  cell_towers <- st_read("master_dk.shp") %>%
    filter(Tjeneste_1=="Mobiltelefoni") %>%
    st_intersection(munic_polygon)
  return(cell_towers)
}

load_dem <- function(munic_polygon) {
  # load raster DEM and crop and mask to munic extent
  munic_dem <- raster::raster("dhm_aarhus_terraen_10m.tif") %>%
    crop(munic_polygon) %>%
    mask(munic_polygon)
  return(munic_dem)
}

load_buffered_dem <- function(munic_polygon, buffer_size) {
  # make a munic polygon with buffer (in meters) and load, crop and mask DEM to this
  munic_polygon_buffered <- st_buffer(munic_polygon, dist = buffer_size)
  buffered_dem <- raster::raster("dhm_aarhus_terraen_10m.tif") %>%
    crop(munic_polygon_buffered) %>%
    mask(munic_polygon_buffered)
  return(buffered_dem)
}

remove_duplicates <- function(elevated_towers) {
  # group cell towers into clusters and return one from each cluster
  tower_points <- elevated_towers %>% st_geometry()
  # find groups of points within 10 meters
  clusters <- st_is_within_distance(tower_points, dist = 10)
  # select one per group and convert list to vector of unique representative indices
  representative_indices <- unique(sapply(clusters, min))
  # subset the original towers using these representative indices
  unique_towers <- elevated_towers[representative_indices, ] %>%
    distinct()
  return(unique_towers)
}

# load data
aarhus_munic <- load_municipality("Århus")
aarhus_cell_towers <- load_cell_towers(aarhus_munic)
aarhus_dem <- load_dem(aarhus_munic)
aarhus_buffered_dem <- load_buffered_dem(aarhus_munic, 15000)

# assign elevation values from DEM to cell towers
aarhus_cell_towers$elevation <- extract(aarhus_dem, aarhus_cell_towers)

# filter towers > 80m and extract unique coordinates within 10 meters
elevated_towers <- filter(aarhus_cell_towers, elevation > 80)
unique_towers <- remove_duplicates(elevated_towers)

#___________________________________________________APP________________________________________________

# Create the Shiny UI
ui <- fluidPage(
  titlePanel("Coverage Map Generator"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Select Address
      textInput("address", "Enter Danish Address", placeholder = "e.g. Jens Christian Skous Vej 5, Aarhus"),
      
      # Select the tower
      selectInput("tower", 
                  "Select Cell Tower", 
                  choices = setNames(
                    unique_towers$MasteID, 
                    paste0(unique_towers$By, " (ID ", unique_towers$MasteID, ")")
                  ),
                  selected = unique_towers$MasteID[1]),  # default
      
      # Slider input for the start angle of FOV
      sliderInput("fov_start", 
                  "Select Antenna Direction (°)", 
                  min = 0, 
                  max = 360, 
                  value = 60), # default
      
      # Slider input for the antenna height (observer height)
      sliderInput("antenna_height", 
                  "Select Antenna Height (m)", 
                  min = 15, max = 50, value = 42),
      
      # Slider input for the maximum range
      sliderInput("max_range", 
                  "Select Maximum Range (km)", 
                  min = 2, max = 10, value = 3),
    ),
    
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Create the Shiny server function
server <- function(input, output, session) {
  
  # This will trigger when the user selects a tower
  observe({
    
    # render the map with the viewshed
    output$map <- renderLeaflet({
      
      # extract the selected tower based on input
      selected_tower <- unique_towers %>%
        filter(MasteID == input$tower)
      
      # convert the selected tower to a numeric vector of coordinates
      coords <- st_coordinates(selected_tower)
      viewpoint <- as.numeric(coords[1, ])
      
      # convert to latlong to add as leaflet map feature
      viewpoint_4326 <- selected_tower %>%
        st_transform(crs = 4326)
      
      # convert type from raster to terra package
      dhm_terra <- rast(aarhus_dem)
      dhm_buffered_terra <- rast(aarhus_buffered_dem)
      
      # convert max range from km to m
      max_range_m <- input$max_range*1000
      
      # compute viewshed
      output <- viewscape::compute_viewshed(dsm = dhm_buffered_terra, 
                                            viewpoints = viewpoint, 
                                            offset_viewpoint = input$antenna_height, # observer height
                                            r = max_range_m, # max distance
                                            method = 'los')
      
      # Calculate the start and end angles based on the slider input
      fov_start <- input$fov_start  # Use the slider value as fov_start
      fov_end <- fov_start + 120    # Always make fov_end = fov_start + 120
      
      # Apply field of view (FOV) mask using the calculated start and end angles
      angle <- viewscape::fov_mask(output, c(fov_start, fov_end))  # angle where 0 is E and 90 is N
      
      # Visualize the viewshed as raster
      output_r <- viewscape::visualize_viewshed(angle, outputtype = 'raster')
      
      # Reproject to EPSG:4326 (WGS84)
      output_r_4326 <- project(output_r, "EPSG:4326")
      
      # Convert to polygons
      viewshed_poly <- as.polygons(output_r_4326, values = TRUE)
      
      # Convert the polygons to an sf object + get bbox for map zoom
      viewshed_sf <- st_as_sf(viewshed_poly)
      bbox <- as.list(st_bbox(viewshed_sf))
      
      # Geocode the address (if provided)
      address_sf <- NULL  # default
      
      if (nzchar(input$address)) {
        address_df <- data.frame(address = input$address)
        
        geocode_result <- tryCatch({
          address_df %>%
            tidygeocoder::geocode(address = address, method = "osm", quiet = TRUE)
        }, error = function(e) NULL)
        
        if (!is.null(geocode_result) && nrow(geocode_result) > 0) {
          address_sf <- st_as_sf(geocode_result, coords = c("long", "lat"), crs = 4326)
        }
      }
      
      # create a leaflet map to visualize the viewshed
      leaflet_map <- leaflet() %>%
        
        # add base map options
        addProviderTiles("OpenStreetMap", group = "OSM") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("CartoDB.Positron", group = "Light") %>%
        addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
        
        # set zoom from viewshed extents
        fitBounds(lng1 = bbox$xmin, lat1 = bbox$ymin,
                  lng2 = bbox$xmax, lat2 = bbox$ymax) %>%
        
        # add DEM
        addRasterImage(aarhus_dem,
                       colors = terrain.colors(25), 
                       opacity = 0.5,
                       group = "Elevation") %>%
        
        # add viewshed
        addPolygons(data = viewshed_sf, 
                    fillColor = "red", 
                    fillOpacity = 0.4, 
                    color = "darkred", 
                    weight = 1,
                    group = "Viewshed") %>%
        
        # add cell tower
        addCircleMarkers(data = viewpoint_4326,
                         radius = 6,
                         color = "red",
                         stroke = TRUE,
                         fillOpacity = 1,
                         label = paste0("Tower ID: ", selected_tower$MasteID),
                         popup = paste0(
                           "<b>Tower ID:</b> ", selected_tower$MasteID, "<br>",
                           "<b>Address:</b> ", selected_tower$Vejnavn, " ", selected_tower$Husnummer, ", ", selected_tower$By, "<br>",
                           "<b>Elevation:</b> ", round(selected_tower$elevation), " m.<br>",
                           "<b>Technology:</b> ", selected_tower$Teknologi_, "<br>",
                           "<b>Installed:</b> ", substr(selected_tower$Idriftsaet, 1, 10)
                         ),
                         labelOptions = labelOptions(direction = "auto"),
                         group = "Cell Tower")
      
      # add address layer if entered
      if (!is.null(address_sf)) {
        leaflet_map <- leaflet_map %>%
          addCircleMarkers(data = address_sf,
                           radius = 6,
                           color = "blue",
                           fillOpacity = 1,
                           label = paste0("Address: ", input$address),
                           group = "Address")
      }
      
      # add layers control
      leaflet_map <- leaflet_map %>%
        addLayersControl(
          baseGroups = c("Light", "OSM", "Satellite", "Dark"),
          overlayGroups = c("Viewshed", "Cell Tower", "Address", "Elevation"),
          position = c("topleft"),
          options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(c("Elevation"))
      
      # render map
      leaflet_map
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
