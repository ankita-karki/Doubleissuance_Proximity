# Load necessary libraries
library(sf)
library(dplyr)
library(leaflet)

# Function to read and validate geometries
read_and_make_valid <- function(file) {
  sf_obj <- st_read(file, quiet = TRUE)  # Read file into an sf object
  sf_obj <- st_make_valid(sf_obj)       # Fix invalid geometries
  sf_obj$filename <- basename(file)     # Add filename as an attribute
  return(sf_obj)
}

#Proximity analysis 
###############################
# Consider 3 distance in km: 5km, 10km and 15km 
# Function to perform proximity analysis and create interactive map
create_proximity_analysis_map <- function(cookstove_sf, avoided_def_sf, buffer_distances, buffer_colors) {
  
  # Initialize the leaflet map with OpenStreetMap tiles
  buffer_map <- leaflet() %>%
    addProviderTiles(providers$OpenStreetMap)
  
  # Loop through each buffer distance to create and add buffer polygons to the map
  for (i in seq_along(buffer_distances)) {
    # Dynamically create buffer for each distance
    current_buffer <- st_buffer(cookstove_sf, dist = buffer_distances[i])
    
    # Add the buffer as a polygon layer to the map with a unique color and group
    buffer_map <- buffer_map %>%
      addPolygons(data = current_buffer, 
                  fillColor = "transparent",
                  color = buffer_colors[i],
                  fillOpacity = 0.3,
                  weight = 2,
                  opacity = 0.8,
                  popup = ~paste(buffer_distances[i] / 1000, "km buffer"),
                  group = paste0(buffer_distances[i], " m Buffer"))
  }
  
  # Add avoided deforestation project areas as polygons to the map
  buffer_map <- buffer_map %>%
    addPolygons(data = avoided_def_sf,
                fillColor = "green", # Fill color for avoided deforestation projects
                color = "#000000", # Border color for avoided deforestation projects
                weight = 2,
                fillOpacity = 0.7, # Adjusted for visibility
                popup = ~as.character(filename), # Adjust 'filename' to your specific column name if different
                group = "Avoided Deforestation")
  
  # Add layers control to toggle visibility of each buffer layer and the avoided deforestation layer
  buffer_map <- buffer_map %>%
    addLayersControl(overlayGroups = c(paste0(buffer_distances, " m Buffer"), "Avoided Deforestation"),
                     options = layersControlOptions(collapsed = FALSE, position = "topleft"))
  
  # Add legend for the map
  buffer_map <- buffer_map %>% 
    addLegend(position = "bottomright", 
              colors = c(buffer_colors, "green"), 
              labels = c(paste(buffer_distances / 1000, "km buffer"), "Avoided Deforestation Projects"), 
              opacity = 0.5)
  
  return(buffer_map)
}


####################
#ggplot for buffer analysis for specific location 

create_main_map <- function(buffers, all_avoided_def_sf, overlap, all_cookstove_sf, buffer_colors, avoided_def_colors) {
  ggplot() +
    geom_sf(data = buffers[[3]], fill = buffer_colors["15km"], color = "black", alpha = 0.2) +
    geom_sf(data = buffers[[2]], fill = buffer_colors["10km"], color = "black", alpha = 0.2) +
    geom_sf(data = buffers[[1]], fill = buffer_colors["5km"], color = "black", alpha = 0.2) +
    geom_sf(data = all_avoided_def_sf, aes(fill = filename), color = "black", alpha = 0.5) +
    scale_fill_manual(values = avoided_def_colors) +
    geom_sf(data = overlap, fill = "red", color = "black", alpha = 0.5) +
    geom_sf(data = all_cookstove_sf, fill = "white", color = "black", size = 2) +
    theme_minimal() +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")  # Remove legend
}

# Function to create inset map
create_inset_map <- function(project_name) {
  project_overlap <- overlap %>% filter(filename.1 == project_name)
  
  # Get bounding box of the overlap area
  bbox <- st_bbox(project_overlap)
  
  # Add a small buffer to the bounding box for better visualization
  buffer_size <- (bbox[3] - bbox[1]) * 0.5  # 10% of the width
  bbox_buffered <- bbox + c(-buffer_size, -buffer_size, buffer_size, buffer_size)
  
  inset_map <- ggplot() +
    geom_sf(data = buffers[[3]], fill = buffer_colors["15km"], color = "black", alpha = 0.2) +
    geom_sf(data = buffers[[2]], fill = buffer_colors["10km"], color = "black", alpha = 0.2) +
    geom_sf(data = buffers[[1]], fill = buffer_colors["5km"], color = "black", alpha = 0.2) +
    geom_sf(data = all_avoided_def_sf, aes(fill = filename), color = "black", alpha = 0.5) +
    geom_sf(data = project_overlap, fill = "red", color = "black", alpha = 0.5) +
    geom_sf(data = all_cookstove_sf, fill = "white", color = "black", size = 2) +
    scale_fill_manual(values = avoided_def_colors) +
    coord_sf(xlim = c(bbox_buffered[1], bbox_buffered[3]), 
             ylim = c(bbox_buffered[2], bbox_buffered[4])) +
    theme_void() +
    theme(legend.position = "none",  # Remove legend
          plot.title = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 1))  # Add black border
  
  return(inset_map)
}

