# Load the master function
source("master_function.R")

#mozamique
avoided_def_files <- c("KMLfile/VCS_REDD/Africa/1775.kml",
                       "KMLfile/VCS_REDD/Africa/902.kml", 
                       "KMLfile/VCS_REDD/Africa/1532.kml")

cookstove_files <- c("KMLfile/Cookstove_VCS/Africa/2351.kml",
                     "KMLfile/Cookstove_VCS/Africa/2351.kml")

# Assuming the files are in the working directory, or specify the path
avoided_def_list <- lapply(avoided_def_files, read_and_make_valid)
cookstove_sf_list <- lapply(cookstove_files, read_and_make_valid)

# Combine all cookstove sf objects into one sf object
all_cookstove_sf <- do.call(rbind, cookstove_sf_list)

# Combine all REDD+ sf objects into one sf object
all_avoided_def_sf <- do.call(rbind, avoided_def_list)

# Define Buffer Distances
buffer_distances <- c(5000, 10000, 15000)

# Create Buffers
buffers <- lapply(buffer_distances, function(dist) {
  st_buffer(all_cookstove_sf, dist = dist)
})
names(buffers) <- paste0(buffer_distances / 1000, "km")

# Define Buffer Colors
buffer_colors <- c("5km" = "#80008033", "10km" = "#FFFF0033", "15km" = "#00000033")

# Calculate Overlap
overlap <- st_intersection(buffers[[3]], all_avoided_def_sf)

# Generate Unique Colors for Avoided Deforestation Projects
avoided_def_colors <- brewer.pal(n = length(unique(all_avoided_def_sf$filename)), name = "Set3")
names(avoided_def_colors) <- unique(all_avoided_def_sf$filename)

# Create Main Map
main_map <- create_main_map(buffers, all_avoided_def_sf, overlap, all_cookstove_sf, buffer_colors, avoided_def_colors)


# Create inset maps for two specific projects
project_name1 <- unique(overlap$filename.1)[1]
project_name2 <- unique(overlap$filename.1)[2]

inset_map1 <- create_inset_map(project_name1)
inset_map2 <- create_inset_map(project_name2)

# Combine the main map and the two inset maps
combined_map <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_map1, x = 0.65, y = 0.72, width = 0.3, height = 0.3) +  # Adjust position and size as needed
  draw_plot(inset_map2, x = 0.6, y = 0.09, width = 0.3, height = 0.3)  # Adjust position and size as needed

plot(combined_map)

