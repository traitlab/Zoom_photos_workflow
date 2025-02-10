extract_centroids_wpt_missions <- function(trees_polygon_path, #required, selected trees for waypoint missions
                                           aoi_path = "", #optional, if AOI is available
                                           aoi_index = 1, #optional, if there are more than one polygon in the AOI layer
                                           aoi_qualifier = "", #optional, to differentiate if there are more than one polygon in the AOI layer
                                           aoi_relation = "", #optional, choice between within (trees fully within AOI) or intersect (trees with at least a part inside AOI)
                                           dsm_path, #required, DSM from the mapping mission
                                           espg_code) #required, projected CRS
  {
  
  require(exactextractr)
  require(raster)
  require(sf)
  require(tidyverse)
  require(TSP)
  
  # Directory and basename
  directory_path <- dirname(trees_polygon_path)
  
  trees_polygon_file_with_extension <- basename(trees_polygon_path)
  trees_polygon_file_without_extension <- tools::file_path_sans_ext(trees_polygon_file_with_extension)
  
  # Initialize variables
  trees_aoi <- NULL
  dsm_max <- NA
  
  # Read tree polygons file
  trees <- st_read(trees_polygon_path) %>% 
    mutate(fid = as.integer(rownames(.)))
  
  # Read DSM raster file
  dsm_crs <- paste0("EPSG:", espg_code)
  dsm_raster <- raster(dsm_path, crs = dsm_crs)

  if (aoi_path != "") {
    # Import KML of AOI
    aoi <- st_read(aoi_path)
    aoi <- st_zm(aoi, drop = TRUE, what = "ZM")
    aoi_transform <- st_transform(aoi, espg_code)
    
    # Plot AOI
    ggplot() +
      geom_sf(data = aoi_transform) +
      theme_bw()

    # Define the relationship between trees and AOI (choice: "within" or "intersect")
    if (aoi_relation == "within") {
      # Use st_within() to select polygons completely within the AOI
      within_indices <- st_within(trees, aoi_transform[aoi_index, ])
    } else if (aoi_relation == "intersect") {
      # Use st_intersects() to select polygons that overlap with the AOI
      within_indices <- st_intersects(trees, aoi_transform[aoi_index, ])
    } else {
      # Throw an error if aoi_relation has an invalid value
      stop("The value of aoi_relation must be either 'within' or 'intersect'.")
    }
  
    # Keep only the polygons that satisfy the selected relationship (non-empty results)
    trees_aoi <- trees[lengths(within_indices) > 0, ]
  
    # Extract the maximum DSM value for the AOI
    dsm_max <- exact_extract(dsm_raster, aoi_transform[aoi_index, ], fun = "max") %>% 
      max(na.rm = TRUE)
  } else {
    # If no AOI, use all tree polygons
    trees_aoi <- trees
  }
    
  # create 1-m buffers around trees
  sampled_trees_buffer <- st_buffer(trees_aoi, dist = 1)
  plot(st_geometry(sampled_trees_buffer))
  
  # Extract DSM height value for each tree
  sampled_trees_elev <- exact_extract(dsm_raster, sampled_trees_buffer, fun = 'max')
  
  sampled_trees <- trees_aoi %>% 
    mutate(elev = sampled_trees_elev)
  
  #Extract centroids from sampled trees
  st_agr(sampled_trees) = "constant"
  sampled_trees_centroids <- st_centroid(sampled_trees)
  
  # Export centroids as GPKG for visualization
  output_centroids_file <- paste0(directory_path, "/", trees_polygon_file_without_extension,
                                  "_wpt", aoi_qualifier, ".gpkg")
    st_write(sampled_trees_centroids, output_centroids_file, append = FALSE)
  
  
  # Plot centroids
  ggplot()+
    geom_sf(data=sampled_trees_centroids)+
    theme_bw()
  
  # Shortest path (TSP algorithm)
  # Calculate the distance matrix between waypoints
  dist_matrix <- st_distance(sampled_trees_centroids)
  
  # Convert distance matrix to a symmetric matrix
  dist_matrix_numeric <- units::drop_units(as.matrix(dist_matrix))
  
  # Create a TSP object
  tsp <- TSP(dist_matrix_numeric)
  
  # Solve the TSP using a heuristic algorithm (e.g., repetitive nearest neighbor)
  tour <- solve_TSP(tsp, method = "repetitive_nn")
  
  # Get the order of waypoints from the solution
  waypoint_order <- as.integer(tour)
  
  # Reorder the waypoints
  sorted_waypoints <- sampled_trees_centroids[waypoint_order, ]
  
  # Plot the sorted waypoints to visualize the S-pattern
  plot(st_geometry(sorted_waypoints), type = "b", col = "blue", main = "Waypoints in S-pattern")
  text(st_coordinates(sorted_waypoints), pos = 4)
  
  
  # Transform waypoint coordinates to WGS84
  waypoints_transformed <- st_transform(sorted_waypoints, 4326)
  
  
  # Rename columns for CSV export
  cluster_column <- if ("cluster_id" %in% colnames(waypoints_transformed)) "cluster_id" else NULL
  
  waypoints_transformed_renamed <- waypoints_transformed %>% 
    mutate(lon_x = st_coordinates(waypoints_transformed)[, 1],
           lat_y = st_coordinates(waypoints_transformed)[, 2],
           polygon_id = fid,
           cluster_id = if (!is.null(cluster_column)) .data[[cluster_column]] else fid,
           order = 1:nrow(waypoints_transformed),
           distance_from_takeoff_point = 0,
           index = 0) %>% 
    dplyr::select(index,
                  polygon_id,
                  cluster_id,
                  distance_from_takeoff_point,
                  lon_x,
                  lat_y,
                  elevation_from_dsm = elev,
                  order) %>% 
    st_drop_geometry()
  
  
  # Export as CSV
  output_csv_file <- paste0(directory_path, "/", trees_polygon_file_without_extension,
                            "_wpt", aoi_qualifier, ".csv")
  
  write_csv(waypoints_transformed_renamed, output_csv_file)

  # Return paths of output files  
  if (aoi_path != "") {
  return(list(aoi = aoi_qualifier,
              centroids_gpkg = output_centroids_file, 
              waypoints_csv = output_csv_file,
              max_dsm_value = dsm_max))
  } else {
  return(list(centroids_gpkg = output_centroids_file, 
              waypoints_csv = output_csv_file))
  }
  
}


# Example usage
# source("extract_centroids_wpt_missions.R")
# 
# extract_centroids_wpt_missions(trees_polygon_path = "DJI_mapping/50haplot/50haplot_treesover100m2.gpkg",
#                                aoi_path = "DJI_mapping/50haplot/bci50haplot_aoi.gpkg",
#                                aoi_index = 1,
#                                aoi_qualifier = "so",
#                                aoi_relation = "intersect",
#                                dsm_path = "conrad/20241121_bci50haplot_m3e/20241121_bci50haplot_m3e_dsm.tif",
#                                espg_code = 32617)