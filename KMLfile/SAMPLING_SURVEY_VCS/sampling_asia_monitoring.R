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
  #sf_obj$filename <- basename(file)
  return(sf_obj)
}

#Loading the KML files 
avoided_def_files <- list.files(path = "KML file/VCS_REDD/Asia", pattern = "\\.kml$", full.names = TRUE)

cookstove_paths <- c("C:/Users/karki/Documents/Thesis/Samplingsurvey/Asia/2409/2409.kml",
                     "C:/Users/karki/Documents/Thesis/Samplingsurvey/Asia/2925/D_2925.kml")

# Assuming the files are in the working directory, or specify the path
avoided_def_list <- lapply(avoided_def_files, read_and_make_valid)
cookstove_sf_list <- lapply(cookstove_paths, read_and_make_valid)

# Combine all cookstove sf objects into one sf object
all_cookstove_sf <- do.call(rbind, cookstove_sf_list)

# Combine all REDD+ sf objects into one sf object
all_avoided_def_sf <- do.call(rbind, avoided_def_list)



#############
# Example of creating a lookup table
lookup_table <- data.frame(
  filename = c("All_2409.kml", "D_2925.kml"),
  country = c("Cambodia", "Cambodia")
)

# Assuming 'spatial_data' is your sf object containing a 'filename' column
all_cookstove_sf <- all_cookstove_sf %>%
  left_join(lookup_table, by = "filename")
############
#Perform the overlap analysis using st_intersection
tryCatch({
  overlap_sf <- st_intersection(all_cookstove_sf, all_avoided_def_sf)
}, error = function(e) {
  message("Error during intersection: ", e$message)
  # Use st_intersection again after making geometries valid and simplified
  all_cookstove_sf <- st_make_valid(all_cookstove_sf) %>% st_simplify(preserveTopology = TRUE)
  all_avoided_def_sf <- st_make_valid(all_avoided_def_sf) %>% st_simplify(preserveTopology = TRUE)
  overlap_sf <- st_intersection(all_cookstove_sf, all_avoided_def_sf)
})


# Create a color palette
palette <- colorFactor(palette = brewer.pal(8, "Dark2"), domain = all_avoided_def_sf$filename)

# Build the Leaflet map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = all_cookstove_sf, fillColor = "green", fillOpacity = 1, color = "black", weight = 2, 
              group = "Cookstove Projects") %>%
  addPolygons(data = all_avoided_def_sf, fillColor = ~palette(filename), fillOpacity = 1, color = "black", weight = 1, 
              group = "Avoided Deforestation Projects") %>%
  addPolygons(data = overlap_sf, fillColor = "red", fillOpacity = 0.7, color = "black", weight = 1, 
              group = "Overlapping Areas")
  #addLayersControl(overlayGroups = c("Cookstove Projects", "Avoided Deforestation Projects", "Overlapping Areas"),
                  # options = layersControlOptions(collapsed = FALSE, position="topleft"))
# Create a leaflet map with a customized scale bar
leaflet() %>%
  addTiles() %>%
  addScaleBar(position = "bottomleft", 
              options = scaleBarOptions(imperial = FALSE, metric = TRUE, updateWhenIdle = TRUE))

####################
##Area of cookstove project 
# Calculate the area of each polygon first (if not done previously)
all_cookstove_sf <- all_cookstove_sf %>%
  mutate(area_m2 = st_area(geometry))  # Compute the area for each polygon in all_avoided_def_sf

total_cookstove_areas <- all_cookstove_sf %>%
  group_by(filename) %>%
  summarize(total_area_km2 = sum(area_m2) / 1e6)

# Print the total area of each avoided deforestation project in square kilometers
print(total_cookstove_areas)


###Area of AD project
# Calculate the area of each polygon first (if not done previously)
all_avoided_def_sf <- all_avoided_def_sf %>%
  mutate(area_m2 = st_area(geometry))  # Compute the area for each polygon in all_avoided_def_sf

total_AD_areas <- all_avoided_def_sf %>%
  group_by(filename) %>%
  summarize(total_area_km2 = sum(area_m2) / 1e6)

print(total_AD_areas)

###Area of overlap
# Calculate the area of each polygon first (if not done previously)
overlap_sf <- overlap_sf %>%
  mutate(area_m2 = st_area(geometry))  # Compute the area for each polygon in all_avoided_def_sf

# Summarize total area by both filename and filename1
total_overlap_areas <- overlap_sf %>%
  group_by(filename, filename.1) %>%
  summarize(
    total_area_km2 = sum(area_m2) / 1e6,  # Convert area from square meters to square kilometers
    .groups = 'drop'  # Drop the grouping to prevent grouping from affecting further operations
  )
# Print the total area of each avoided deforestation project in square kilometers
print(total_overlap_areas)

