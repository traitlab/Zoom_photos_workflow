#Extract waypoints for DSM points (AVUELO campaign, Agua Salud)

library(exactextractr)
library(raster)
library(sf)
library(terra)
library(tidyverse)
library(TSP)

#Set WD

#Import points
points <- st_read("avuelo_clean_points_within.shp")

#points_vec <- terra::vect(points)

#Import DSM raster
dsm_raster <- raster("20240729_aguasalud_rx1rii_dsm.tif", band=1)
#dsm <- terra::rast("20240729_aguasalud_rx1rii_dsm.tif")
crs(dsm_raster)

#dsm_valid <- as.polygons(!is.na(dsm), dissolve = TRUE) |> st_as_sf()


# Check which points are inside valid DSM areas
inside_dsm <- st_intersects(points, dsm_raster, sparse = FALSE)

# Keep only points inside non-NA areas
#points_valid <- points[inside_dsm, ]


# create 5-m buffers around points
sampled_trees_buffer <- st_buffer(points, dist = 5)
plot(st_geometry(sampled_trees_buffer))

# Extract DSM height value for each tree
sampled_trees_elev <- exact_extract(dsm_raster, sampled_trees_buffer, fun = 'max')

elev_id <- data_frame(sampled_trees_elev,sampled_trees_buffer$SampleID)
colnames(elev_id) <- c("elevation_from_dsm", "SampleID")


# Plot centroids
ggplot()+
  geom_sf(data=points)+
  theme_bw()

#AOI selection

#import polygons
aoi_polygons <- st_read("avuelo_polygons.shp")

intersections <- st_intersects(points, aoi_polygons)

# Convert intersections into a data frame
results <- data.frame(
  point_id = points$SampleID,  # Assuming points are in order
  polygon_id = sapply(intersections, function(x) ifelse(length(x) > 0, x[1], NA))  # NA if no intersection
)


# Directory to save CSV files
output_directory <- getwd()

# Loop through each AOI polygon
for (i in seq_len(nrow(aoi_polygons))) {
  aoi <- aoi_polygons[i, ]  # Select one polygon
  
  # Filter points that intersect this AOI
  points_in_aoi <- points[st_intersects(points, aoi, sparse = FALSE), ]
  
  if (nrow(points_in_aoi) == 0) next  # Skip if no points in AOI
  
  # Compute distance matrix
  dist_matrix <- st_distance(points_in_aoi)
  dist_matrix_numeric <- units::drop_units(as.matrix(dist_matrix))
  
  # Solve TSP
  tsp <- TSP(dist_matrix_numeric)
  tour <- solve_TSP(tsp, method = "repetitive_nn")
  waypoint_order <- as.integer(tour)
  
  # Reorder waypoints
  sorted_waypoints <- points_in_aoi[waypoint_order, ]
  
  # Transform coordinates
  waypoints_transformed <- st_transform(sorted_waypoints, 4326)
  
  # Prepare for export
  waypoints_transformed_renamed <- waypoints_transformed %>%
    mutate(lon_x = st_coordinates(waypoints_transformed)[, 1],
           lat_y = st_coordinates(waypoints_transformed)[, 2],
           polygon_id = SampleID,
           cluster_id = if ("cluster_id" %in% colnames(.)) cluster_id else aoi$id,
           order = 1:nrow(waypoints_transformed),
           distance_from_takeoff_point = 0,
           index = 0) %>%
    left_join(elev_id, by = "SampleID") %>%  # Merge elevation
    select(index, polygon_id, cluster_id, distance_from_takeoff_point, 
           lon_x, lat_y, elevation_from_dsm, order) %>%  # Correct column names
    st_drop_geometry()
  
  # Export CSV
  output_csv_file <- paste0(output_directory, "/waypoints_aoi_", aoi$id, ".csv")
  write_csv(waypoints_transformed_renamed, output_csv_file)
  
  print(paste("Saved:", output_csv_file))
}


