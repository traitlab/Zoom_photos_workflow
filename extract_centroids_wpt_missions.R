extract_centroids_wpt_missions <- function(trees_polygon_path, #required, selected trees for waypoint missions
                                           aoi_path = "", #optional, if AOI is available
                                           aoi_index = 1, #optional, if there are more than one polygon in the AOI layer
                                           aoi_qualifier = "", #optional, to differentiate if there are more than one polygon in the AOI layer
                                           aoi_relation = "", #optional, choice between within (trees fully within AOI) or intersect (trees with at least a part inside AOI)
                                           dsm_path, #required, DSM from the mapping mission
                                           espg_code, #required, projected CRS
                                           buffer_path=10,#buffer around tree path
                                           buffer_tree=3) #buffer around tree centroid
  {
  
  require(exactextractr)
  require(raster)
  require(sf)
  require(tidyverse)
  require(TSP)
  require(purrr)
  
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
  
  # Extract tree centroids from trees_aoi
  
  tree_centroids <- st_centroid(trees_aoi)
  
  # create 1-m buffers around tree centroids
  sampled_trees_buffer <- st_buffer(tree_centroids, dist = buffer_tree)
  plot(st_geometry(sampled_trees_buffer))
  
  # Extract DSM height value for each tree
  sampled_trees_elev <- exact_extract(dsm_raster, sampled_trees_buffer, fun = function(values, coverage_fraction) {
    quantile(values, probs = 1, na.rm = TRUE) #change probs value to get elevation values per quantile (e.g. 0.99 is 99th percentile)
  })
  
  #Integrate elevation to sampled_trees df
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
  
  waypoints_transformed <- st_transform(sorted_waypoints, crs=4326)
  
  sampled_trees_buffer <- sampled_trees_buffer[waypoint_order,]
  
  # Plot the sorted waypoints to visualize the S-pattern
  plot(st_geometry(sorted_waypoints), type = "b", col = "blue", main = "Waypoints in S-pattern")
  text(st_coordinates(sorted_waypoints), pos = 4)
  
  plot(st_geometry(sampled_trees_buffer), type="b", col="red", main ="Buffers in S-pattern")
  
  
  # Rename columns for CSV export
  cluster_column <- if ("cluster_id" %in% colnames(sampled_trees_buffer)) "cluster_id" else NULL
  
  ####Adding max height between each pair of points####
  
  #Extracting coordinates points
  coords <- st_coordinates(sorted_waypoints)
  
  # Compute pairwise vectors for following points
  n <- nrow(coords)
  vector_list <- list()
  
  for (i in 1:n) {
      if (i<n) {
        # Compute vector from point i to point i+1
        dx <- coords[i + 1, 1] - coords[i, 1]
        dy <- coords[i + 1, 2] - coords[i, 2]
        vector_list[[length(vector_list) + 1]] <- data.frame(
          from = i,
          to = i + 1,
          dx = dx,
          dy = dy)
      }
    }
  
  # Combine results into a data frame
  vectors_df <- bind_rows(vector_list)
  
  # Convert as vectors and project coordinates
   #Create LINESTRING geometries between each pair
  line_geoms <- vector("list", length = nrow(vectors_df))
  
  for (i in seq_len(nrow(vectors_df))) {
    coords_pair <- rbind(
      coords[i, ],
      coords[i+1, ]
    )
    line_geoms[[i]] <- st_linestring(coords_pair)
  }
  
  #Create sf object from these geometries
  vector_lines <- st_sf(
    vectors_df,
    geometry = st_sfc(line_geoms, crs = st_crs(sorted_waypoints))
  )
  
  # Reproject to good UTM zone (with ESPG code)
  vector_lines_utm <- st_transform(vector_lines, crs = espg_code)
  
  #Add a 1m buffer for vector lines between pair of points
  
  buffered_path <- st_buffer(vector_lines_utm, dist=buffer_path)
  
  plot(buffered_path[1])
  
  #Add the centroid buffer polygon at the extremities of every vector line
  
  # Create the paired list of two geometries from df2 for each df1 row
  combined_paths_geom <- map2(
    sampled_trees_buffer$geom[1:9],
    sampled_trees_buffer$geom[2:10],
    ~ st_union(.x, .y)
  )
  
  
  # Create the output sf object
  trees_with_paths <- buffered_path %>%
    mutate(
      combined_path = st_sfc(combined_paths_geom, crs = st_crs(buffered_path)),
    geometry = map2(geometry, combined_path, st_union) %>% st_sfc(crs = st_crs(sampled_trees_buffer)))%>%
  select(-combined_path) %>%  # remove the extra column
  st_as_sf()
  
    plot(trees_with_paths$geometry[1:9])
    
  #highest value for each vector line
  highest_point <- exact_extract(dsm_raster, trees_with_paths, fun = function(values, coverage_fraction) {
    quantile(values, probs = 1, na.rm = TRUE) #change probs to change selected elevation by quantile
  })
  
  #extract central points for path
  buffer_centroids <- st_centroid(trees_with_paths)
  
  #Vector of zeros (factice polygon_ID for checkpoints)
  fid <- numeric(n-1)

  
  #dataframe with only checkpoints
  checkpoints_transformed <- cbind(buffer_centroids,fid, highest_point)%>%
    rename(elev=highest_point,
         geom=geometry)%>%
    st_transform(4326)

  #Add checkpoints between waypoints
    # Empty list to store result
  interleaved_points <- list()
  
  for (i in 1:(nrow(waypoints_transformed) - 1)) {
    
    # Add start waypoint and add type wpt
    wpt <- waypoints_transformed[i,(ncol(waypoints_transformed)-2):(ncol(waypoints_transformed))] %>% mutate(type = "wpt")
    interleaved_points[[length(interleaved_points) + 1]] <- wpt
    
    # Extract relevant checkpoint (for example, by index — adjust to your logic) and add type cpt
    checkpoint <- checkpoints_transformed[i,(ncol(checkpoints_transformed)-2):(ncol(checkpoints_transformed))] %>% mutate(type = "cpt")

    # Add checkpoint in between
    interleaved_points[[length(interleaved_points) + 1]] <- checkpoint
  }
  
  # Add the last waypoint and type
  last_wp <- waypoints_transformed[nrow(waypoints_transformed), (ncol(waypoints_transformed)-2):(ncol(waypoints_transformed)) ] %>% mutate(type = "wpt")
  interleaved_points[[length(interleaved_points) + 1]] <- last_wp
  
  # Bind into a single sf object
  combined_sf_transformed <- do.call(rbind, interleaved_points)
  

  #####Preparation for formatting CSV file####
  
  points_transformed_renamed <- combined_sf_transformed %>% 
    mutate(lon_x = st_coordinates(combined_sf_transformed)[, 1],
           lat_y = st_coordinates(combined_sf_transformed)[, 2],
           polygon_id = fid,
           cluster_id = if (!is.null(cluster_column)) .data[[cluster_column]] else fid,
           order =  if_else(type == "wpt", cumsum(type == "wpt"), 0),
           elev= replace(
             elev,
             which(type == "cpt"),
             sapply( which(type == "cpt"),
               function(i) {
                 inds <- max(1, i - 1):min(n(), i + 1)
                 max(combined_sf_transformed$elev[inds], na.rm = TRUE) #Select maximum height in path and polygons for chekpoint height
               })))%>% 
    
    dplyr::select(polygon_id,
                  cluster_id,
                  type, # replace of distance from takeoff point column
                  lon_x,
                  lat_y,
                  elevation_from_dsm = elev,
                  order) %>% 
    st_drop_geometry()
  
  
  # Export as CSV
  output_csv_file <- paste0(directory_path, "/", trees_polygon_file_without_extension,
                            "_wpt", aoi_qualifier, ".csv")
  
  write_csv(points_transformed_renamed, output_csv_file)

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


#### Example usage####
# source("extract_centroids_wpt_missions.R")
#trees <- st_read("20240701_sblz3_p1_rgb_gr0p07_infer.gpkg") %>% 
#  mutate(fid = as.integer(rownames(.)))%>%
#  rename(polygon_id=fid)

#over100 <- filter(trees, trees$area>100)

#over100_adjusted <- over100 %>%dplyr::select(index,
#                                           polygon_id,
#                                           cluster_id,
#                                           distance_from_takeoff_point,
#                                           lon_x,
#                                           lat_y,
#                                           elevation_from_dsm,
#                                           order) %>% 
#  st_drop_geometry()%>%
#  arrange(order)%>%
#  mutate(order = rank(order))
#st_write(over100,"20240701_sblz3_p1_rgb_gr0p07_subsample_infer.gpkg")

## Run the function with the gpkg subsample
#extract_centroids_wpt_missions(trees_polygon_path = "20240701_sblz3_p1_rgb_gr0p07_subsample_infer.gpkg",
#                                dsm_path = "20240701_sblz3_p1_dsm_highdis.cog.tif",
#                                espg_code = 32618)
