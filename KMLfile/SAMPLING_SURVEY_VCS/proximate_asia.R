#Loading the library 
library(sf)
library(sp)
library(dplyr)
library(leaflet)
library(terra)
library(sf)


#Creating function to read and validate geometries 
read_and_make_valid <- function(file) {
  sf_obj <- st_read(file, quiet = TRUE)  # Reads KML file into an sf object
  sf_obj <- st_make_valid(sf_obj)  # Fixes any invalid geometries
  
  # Drops Z (elevation) and M (measure) coordinates if present to simplify geometries
  if ("XYZ" %in% st_geometry_type(sf_obj) || "XYZM" %in% st_geometry_type(sf_obj)) {
    sf_obj <- st_zm(sf_obj, what = "Z")
  }
  if ("XYM" %in% st_geometry_type(sf_obj) || "XYZM" %in% st_geometry_type(sf_obj)) {
    sf_obj <- st_zm(sf_obj, what = "M")
  }
  # Add the filename as a property of the sf object
  sf_obj$filename <- basename(file)
  return(sf_obj)
}


#Loading the KML files 
###########################
avoided_def_files <- c("KML file/VCS_REDD/Asia/1748.kml")

cookstove_paths <- c("C:/Users/karki/Documents/Thesis/Samplingsurvey/Asia/2925/D_2925.kml")


##########################
avoided_def_files <- c("KML file/VCS_REDD/Asia/904.kml",
                       "KML file/VCS_REDD/Asia/1398.kml")

cookstove_paths <- c("C:/Users/karki/Documents/Thesis/Samplingsurvey/Asia/2409/2409.kml")


# Assuming the files are in the working directory, or specify the path
avoided_def_list <- lapply(avoided_def_files, read_and_make_valid)
cookstove_sf_list <- lapply(cookstove_paths, read_and_make_valid)

# Combine all cookstove sf objects into one sf object
all_cookstove_sf <- do.call(rbind, cookstove_sf_list)

# Combine all REDD+ sf objects into one sf object
all_avoided_def_sf <- do.call(rbind, avoided_def_list)


############
# Assume all_cookstove_sf and all_avoided_def_sf are already defined sf objects

buffer_distances <- c(5000, 10000, 15000) # Distances in meters
buffer_colors <- c("#800080", "orange", "black") # Colors for each buffer distance

# Initialize the leaflet map with OpenStreetMap tiles
buffer_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)

# Loop through each buffer distance to create and add buffer polygons to the map
for (i in seq_along(buffer_distances)) {
  # Dynamically create buffer for each distance
  current_buffer <- st_buffer(all_cookstove_sf, dist = buffer_distances[i])
  
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
  addPolygons(data = all_avoided_def_sf,
              fillColor = "blue", # Fill color for avoided deforestation projects
              color = "#000000", # Border color for avoided deforestation projects
              weight = 2,
              fillOpacity = 0.5, # Adjusted for visibility
              popup = ~as.character(filename), # Adjust 'filename' to your specific column name if different
              group = "Avoided Deforestation")

# Add layers control to toggle visibility of each buffer layer and the avoided deforestation layer
buffer_map <- buffer_map %>%
  addLayersControl(overlayGroups = c(paste0(buffer_distances, " m Buffer"), "Avoided Deforestation"),
                   options = layersControlOptions(collapsed = FALSE, position= "topleft"))

buffer_map <- buffer_map %>% 
  addLegend(position = "bottomright", colors = c("#800080", "orange", "black", "blue"), 
            labels = c("5km buffer", "10km buffer", "15km buffer", "Avoided Deforestation Projects"), opacity = 0.5)

# Print the map
buffer_map

#################different avoided deforestaiton 
buffer_distances <- c(5000, 10000, 15000) # Distances in meters
buffer_colors <- c("#800080", "orange", "black") # Colors for each buffer distance

# Generate a unique darker color for each avoided deforestation project
# Create a color palette
palette <- colorFactor(palette = brewer.pal(12, "Paired"), domain = all_avoided_def_sf$filename)
#project_colors <- colorFactor(viridis_pal(option = "D")(length(unique(all_avoided_def_sf$filename))), all_avoided_def_sf$filename)
# Initialize the leaflet map with OpenStreetMap tiles
buffer_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)

# Loop through each buffer distance to create and add buffer polygons to the map
for (i in seq_along(buffer_distances)) {
  # Dynamically create buffer for each distance
  current_buffer <- st_buffer(all_cookstove_sf, dist = buffer_distances[i])
  
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
  addPolygons(data = all_avoided_def_sf,
              fillColor = ~palette(filename), # Unique color for each project
              color = "#000000", # Border color for avoided deforestation projects
              weight = 2,
              fillOpacity = 0.7, # Adjusted for visibility
              popup = ~paste("Project ID:", filename), # Popup with project ID and country
              group = "Avoided Deforestation")

# Add layers control to toggle visibility of each buffer layer and the avoided deforestation layer
#buffer_map <- buffer_map %>%
#addLayersControl(overlayGroups = c(paste0(buffer_distances, " m Buffer"), "Avoided Deforestation"),
#options = layersControlOptions(collapsed = FALSE, position= "topleft"))

# Create labels for the legend
legend_labels <- c(paste0(buffer_distances / 1000, "km buffer"), unique(all_avoided_def_sf$country))

# Print the map
buffer_map


###################################
#forest cover density
forest_cover_sf <- rast("C:/Users/karki/Documents/Thesis/Forestcover/New folder/ForestCover2022Asia.tif")
plot(forest_cover_sf)

# Non-forest areas are 0, so they become NA in the mask, and forest areas are 1
forest_mask <- ifel(forest_cover_sf == 2, 1, NA)

forest_areas <- classify(forest_mask, cbind(1, 1), right=FALSE)  # Ensuring only value 1 is classified
forest_polygons <- as.polygons(forest_areas, dissolve = TRUE)  # Dissolve to merge touching regions

forest_polygons_sf <- st_as_sf(forest_polygons)

#Reducing the resolution
forest_cover_agg <- aggregate(forest_cover_sf, fact = 3, fun = mean)
##################
#settlement layer 
settlements_raster <- raster("C:/Users/karki/Documents/reporjected.tif")
plot(settlements_raster)

###Asia bounding box 
# Define the extent to which you want to crop the TIFF file
crop_extent <- extent(97.345810, 109.461943, -5.631942, 23.395829)

# Crop the raster
cropped_raster <- crop(settlements_raster, crop_extent)
plot(cropped_raster)

# Now check if the values are preserved after crop
print(unique(values(cropped_raster)))

# Reclassification matrix
class_map <- matrix(c(11, 11,
                      12, 12,
                      10, 10,
                      13, 13,
                      21, 21,
                      23, 23,
                      30, 30,
                      22, 22), byrow = TRUE, ncol = 2)


# Reclassify the raster
reclassified_raster <- reclassify(cropped_raster, class_map, right=FALSE)

# Check unique values in the reclassified raster to ensure reclassification worked
print(unique(values(reclassified_raster)))
plot(reclassified_raster)


##############
# Define a color palette for the classes, including transparency
colors <- c("red", "#1E90FF", "green")  # #00000000 is transparent
names(colors) <- c("1", "2", "3")  # Matching numeric codes

# Plot with Leaflet
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addRasterImage(reclassified_raster, colors = c("#00000000", colors), opacity = 0.7) %>%
  addLegend(pal = colorFactor(palette = colors, domain = c("1", "2", "3")),
            values = c("1", "2", "3"),
            labels = c("Rural", "Urban Cluster", "Urban"),
            opacity = 1.0,
            title = "Settlement Class")

#########works

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addRasterImage(reclassified_raster, colors = c("#00000000", colors), opacity = 0.6, group = "Settlements") %>%
  addPolygons(data = all_cookstove_sf, fillColor = "black", fillOpacity = 0.2, color = "black", weight = 2, 
              group = "Cookstove Projects") %>%
  addPolygons(data = all_avoided_def_sf, fillColor = "brown", fillOpacity = 0.6, color = "brown", weight = 1, 
              group = "Avoided Deforestation Projects") %>%
  addLayersControl(overlayGroups = c("Settlements", "Cookstove Projects", "Avoided Deforestation Projects"),
                   options = layersControlOptions(collapsed = FALSE, position="topleft")) %>%
  addLegend(position = "bottomright",colors = c("red", "blue", "yellow", "#006400","brown", "#000000"),labels = c("Rural", "Urban Cluster", "Urban Center", "Forest Cover", "Avoided Deforestation", "Cookstove"),
            title = "Legend")



#########################################


# Calculate the area of each polygon first (if not done previously)
all_avoided_def_sf <- all_avoided_def_sf %>%
  mutate(area_m2 = st_area(geometry))  # Compute the area for each polygon in all_avoided_def_sf

total_AD_areas <- all_avoided_def_sf %>%
  group_by(filename) %>%
  summarize(total_area_km2 = sum(area_m2) / 1e6)

# Print the total area of each avoided deforestation project in square kilometers
print(total_AD_areas)

######
# Assuming 'all_avoided_def_sf' and 'all_cookstove_sf' are already loaded and preprocessed

# Define buffer distances (modify as needed)
buffer_distances <- c(5000, 10000, 15000)  # in meters

# Initialize list to store results for each buffer distance and avoided deforestation project
results_list <- list()

# Loop over each buffer distance
for (dist in buffer_distances) {
  
  # Initialize list to store results for each avoided deforestation project at this buffer distance
  buffer_results <- list()
  
  # Loop over each avoided deforestation project
  for (ad_project in unique(all_avoided_def_sf$filename)) {
    single_ad <- all_avoided_def_sf[all_avoided_def_sf$filename == ad_project, ]
    
    # Initialize dataframe to store results for this avoided deforestation project
    df <- data.frame(filename = character(), 
                     buffer_distance = numeric(),
                     area_km2 = numeric(), 
                     forest_cover_density = numeric())
    
    # Loop over each cookstove project
    for (cookstove in unique(all_cookstove_sf$filename)) {
      single_cookstove <- all_cookstove_sf[all_cookstove_sf$filename == cookstove, ]
      
      # Buffer the single cookstove
      buffered_cookstove <- st_buffer(single_cookstove, dist)
      
      # Calculating intersection with avoided deforestation project
      intersection <- st_intersection(buffered_cookstove, single_ad)
      
      # Calculate the area of the intersection
      if (nrow(intersection) > 0) {
        intersection_area <- sum(st_area(intersection)) / 1e6  # Convert from m^2 to km^2
        # Clip and mask the raster to the intersection area
        intersection_vect <- vect(intersection)
        clipped_raster <- crop(forest_cover_sf, intersection_vect)
        masked_raster <- mask(clipped_raster, intersection_vect)
        
        # Calculate the mean forest cover density within the intersection
        mean_cover <- mean(values(masked_raster), na.rm = TRUE)
        
        # Store results in the dataframe
        df <- rbind(df, data.frame(filename = cookstove, 
                                   buffer_distance = dist,
                                   area_km2 = intersection_area, 
                                   forest_cover_density = mean_cover))
      } else {
        df <- rbind(df, data.frame(filename = cookstove, 
                                   buffer_distance = dist,
                                   area_km2 = 0, 
                                   forest_cover_density = NA))
      }
    }
    
    # Store the dataframe in the list under the current buffer distance for this avoided deforestation project
    buffer_results[[ad_project]] <- df
  }
  
  # Store the results for this buffer distance
  results_list[[paste(dist/1000, "km Buffer")]] <- buffer_results
}

# Print out the results
print(results_list)



##################
# Assuming 'reclassified_raster' is currently a RasterLayer
# Convert it to a SpatRaster if necessary
if (!inherits(reclassified_raster, "SpatRaster")) {
  reclassified_raster <- rast(reclassified_raster)
}

# Confirm the class
print(class(reclassified_raster))

# Assuming 'all_cookstove_sf' and 'all_avoided_def_sf' are already loaded and preprocessed
buffer_distances <- c(5000, 10000, 15000)  # Buffer distances in meters

# Prepare a list to hold the results
class_presence_results <- list()

# Loop through each buffer distance
for (dist in buffer_distances) {
  
  # Initialize list to store results for each avoided deforestation project at this buffer distance
  buffer_results <- list()
  
  # Loop through each avoided deforestation project
  for (ad_project in unique(all_avoided_def_sf$filename)) {
    single_ad <- all_avoided_def_sf[all_avoided_def_sf$filename == ad_project, ]
    
    # Initialize dataframe to store results for this avoided deforestation project
    df <- data.frame(filename = character(), most_common_class = character(), stringsAsFactors = FALSE)
    
    # Loop through each cookstove project
    for (cookstove in unique(all_cookstove_sf$filename)) {
      single_cookstove <- all_cookstove_sf[all_cookstove_sf$filename == cookstove, ]
      
      # Buffer the single cookstove proximity point
      buffered_cookstove <- st_buffer(single_cookstove, dist)
      
      # Calculate the intersection
      intersection <- st_intersection(buffered_cookstove, single_ad)
      
      # Check if there's an intersection
      if (nrow(intersection) > 0) {
        # Convert the intersection to SpatVector if necessary
        intersection_vect <- vect(intersection)
        
        # Mask the reclassified raster with the intersection vector
        masked_raster <- mask(reclassified_raster, intersection_vect)
        
        # Extract all values from the masked raster (including NA)
        values <- values(masked_raster)
        
        # Find the most frequent class (excluding NA)
        most_common_class <- names(which.max(table(values[!is.na(values)])))
        
        # If no classes found (all NA), set to NA
        if (length(most_common_class) == 0) most_common_class <- NA
        
        # Append the results to the DataFrame
        df <- rbind(df, data.frame(filename = cookstove, most_common_class = most_common_class))
      } else {
        # Append results indicating no intersection
        df <- rbind(df, data.frame(filename = cookstove, most_common_class = NA))
      }
    }
    
    # Store the dataframe in the list under the current buffer distance for this avoided deforestation project
    buffer_results[[ad_project]] <- df
  }
  
  # Store the results for this buffer distance
  class_presence_results[[paste(dist/1000, "km Buffer")]] <- buffer_results
}

# Print out the results
print(class_presence_results)

#####################
# Assume 'forest_polygons_sf', 'all_cookstove_sf', and 'all_redd_sf' are already defined sf objects.

buffer_distances <- c(5000, 10000, 15000) # Distances in meters
buffer_colors <- c("#FF0000", "#00FF00", "#0000FF") # Colors for each buffer distance

# Initialize the leaflet map with OpenStreetMap tiles
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap)

#Add settlement layer as raster image 
m <- m %>%
  addRasterImage(reclassified_raster, 
                 colors = c("#00000000", colors), 
                 opacity = 0.7,
                 group= "Reclassified raster")

# Add forest cover areas as polygons to the map
m <- m %>%
  addPolygons(data = forest_polygons_sf,
              fillColor = "#006400", # Forest green
              color = "#00000", # Dark green border, corrected from "transparent" for visibility
              weight = 1,
              fillOpacity = 0.5,
              group = "Forest Cover")

# Loop through each buffer distance to create and add buffer polygons to the map
for (i in seq_along(buffer_distances)) {
  # Dynamically create buffer for each distance
  current_buffer <- st_buffer(all_cookstove_sf, dist = buffer_distances[i])
  
  # Add the buffer as a polygon layer to the map with a unique color and group
  m <- m %>%
    addPolygons(data = current_buffer, 
                fillColor = "transparent",
                color = buffer_colors[i],
                fillOpacity = 0.3,
                weight = 2,
                opacity = 0.8,
                popup = ~paste(buffer_distances[i] / 1000, "km buffer"),
                group = paste("Buffer - ", buffer_distances[i], "m"))
}

# Add avoided deforestation project areas as polygons to the map
m <- m %>%
  addPolygons(data = all_avoided_def_sf,
              fillColor = "#000000", # Fill color for avoided deforestation projects
              color = "#000000", # Border color for avoided deforestation projects
              weight = 2,
              fillOpacity = 0.7,
              popup = ~as.character(filename), # Adjust 'filename' to your specific column name if different
              group = "Avoided Deforestation")

# Add layers control to toggle visibility of each buffer layer and the avoided deforestation layer
# Add layers control to toggle visibility of different groups
m <- m %>%
  addLayersControl(overlayGroups = c("Reclassified Raster", "Forest Cover", "Close Proximity Avoided Deforestation", "5 km Buffer"),
                   options = layersControlOptions(collapsed = FALSE))
# Print the map
print(m)

