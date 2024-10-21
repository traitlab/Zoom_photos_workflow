#Download libraries
library(sf)
library(TSP)
library(dplyr)
library(ggplot2)
library(stars)
library(raster)

#Setwd with all spatial data in it (aoi.kml, site.kml, clusters.gpkg, DSM)

#Import kmls of AOI and site
aoi <- st_read("Fairchild-CNW.kml")#AOI polygon
site <- st_read("bcinfairchild_total.kml")#Drone mapping site
aoi <- st_zm(aoi, drop = TRUE, what = "ZM") #Dropping z dimension (height)
site <- st_zm(site, drop = TRUE, what = "ZM") #Dropping z dimension (height)
st_crs(aoi)
#<- 32617 for Panama
st_crs(site)
fairchild_aoi <- st_transform(aoi, 32617)#for Panama
fairchild <- st_transform(site, 32617)#for Panama
st_crs(fairchild_aoi)
st_crs(fairchild)

#plot
ggplot()+
  geom_sf(data= fairchild)+
  geom_sf(data= fairchild_aoi)+
  theme_bw()

####Extract coordinates from clusters####

fairchildclusters <- st_read("20240909_bcinfairchild_m3e_rgb_final_clustered_resnetQPE_clusters_umap_2_None_None_hdbscan_3_0p12.gpkg") #clusters spatial info file
st_crs(fairchildclusters) #see if it matches the aoi and site crs

#intersect aoi with drone site
fairchildclusters_aoi <- st_intersection(fairchildclusters,fairchild_aoi)
#ensure each tree crown is within aoi
fairchildclusters_aoi_filtered <- fairchildclusters_aoi[st_within(fairchildclusters_aoi,fairchild_aoi, sparse = F),]

#Cluster selection by filters
library(dplyr)
library(tidyr)

clustersize <- fairchildclusters_aoi_filtered %>%
  group_by(cluster_labels) %>%
  mutate(cluster_size=n()) #creation of variable cluster_size (number of trees by cluster)

length(unique(clustersize$cluster_labels))

largertrees_allclusters <-  subset(clustersize,clustersize$area>20) #Adding minimum area to selected trees (20 m^2 here)

length(unique(largertrees_allclusters$cluster_labels))

#remove noise (cluster -1 trees were unclassified)
largertrees_allclusters <- largertrees_allclusters[!(largertrees_allclusters$cluster_labels == -1),]

# Sample 3 trees rows per cluster (minimum of 3 trees by cluster in the aoi)
set.seed(123)
sampled_trees <- largertrees_allclusters %>%
  group_by(cluster_labels) %>%
  filter(n()>=2)%>%
  slice_sample(n = 3, replace = FALSE)

#Extract centroids from sampled trees

sampled_trees_centroids <- st_centroid(sampled_trees)
st_crs(sampled_trees)

#Extract centroids in gpkg for visaulization of tree canopies
st_write(sampled_trees_centroids, "all_centroids_bcinfairchild_CNW.gpkg")

#Visualization
ggplot()+
  geom_sf(data=fairchild)+
  geom_sf(data=sampled_trees_centroids)+
  theme_bw()

#Shortest path
#TSP algorithm

# Calculate the distance matrix between waypoints
dist_matrix <- st_distance(sampled_trees_centroids)

# Convert distance matrix to a symmetric matrix
dist_matrix <- as.matrix(dist_matrix)
dist_matrix_numeric <- units::drop_units(dist_matrix)

# Create a TSP object
tsp <- TSP(dist_matrix_numeric)

# Solve the TSP using a heuristic algorithm (e.g., nearest neighbor)
tour <- solve_TSP(tsp, method = "nn")

# Get the order of waypoints from the solution
waypoint_order <- as.integer(tour)

# Reorder the waypoints
sorted_waypoints <- sampled_trees_centroids[waypoint_order, ]

# Plot the sorted waypoints to visualize the S-pattern
plot(st_geometry(sorted_waypoints), type = "b", col = "blue", main = "Waypoints in S-pattern")
text(st_coordinates(sorted_waypoints), pos = 4)

#create_s_shape <- function(df) {
#  df <- df %>% arrange(x, y)  # Ensure data is sorted initially
#  df <- df %>%
#    group_by(x) %>%
#    mutate(y = ifelse(x %% 2 == 0, desc(y), y)) %>%
#    ungroup()
#  df <- df %>%
#    arrange(x, y)
#  return(df)
#}

# Apply the function to create the S-shaped pattern
#s_pattern <- create_s_shape(waypoints)

# Plot the waypoints to visualize the S-shape pattern
#plot(s_pattern$x, s_pattern$y, type = "n", xlab = "X", ylab = "Y", main = "S-shape Waypoints")
#points(s_pattern$x, s_pattern$y, col = "blue", pch = 19)
#text(s_pattern$x, s_pattern$y, labels = s_pattern$id, pos = 3)

# Draw lines to represent the S-shape path
#lines(s_pattern$x, s_pattern$y, col = "red")

#Visualization
#ggplot()+
#  geom_sf(data=hertelnorth)+
#  geom_sf(data=fitgrid_hertelnorth)+
#  theme_bw()

#retransform waypoint coordinates in WGS84
waypoints_transformed <- st_transform(sorted_waypoints, 4326)

#Import DSM
#fairchilddsm <- read_stars("20240909_fairchild_m3e_dsm_.tif")
#st_crs(fairchilddsm)

fairchilddsmraster <- raster("20240909_bcinfairchild_m3e_dsm_0.5m.tif")
crs(fairchilddsmraster)
#fairchilddsmraster <- projectRaster(fairchilddsmraster, crs=crs(32617))

fairchilddsmrasteraoi <- crop(fairchilddsmraster, extent(fairchild_aoi))
fairchilddsmrasteraoi2 <- mask(fairchilddsmrasteraoi, fairchild_aoi)

raster_df <- as.data.frame(fairchilddsmrasteraoi2, xy=T)

#visualization
ggplot() +
  geom_raster(data=raster_df, aes(x=x, y=y, fill=raster_df$X20240909_bcinfairchild_m3e_dsm_0.5m)) +
  scale_fill_viridis_c() +  # Example color scale (adjust as needed)
  theme_minimal() +
  geom_sf(data= sorted_waypoints)

#Buffer for elevation values (3m here)
buffers <- st_buffer(sorted_waypoints, dist = 3)

extracted_values <- lapply(1:nrow(buffers), function(i) {
  buffer <- as(buffers[i, ], "Spatial")
  values <- raster::extract(fairchilddsmrasteraoi2, buffer)
})

# Combine the highest values with the points
# Function to get the highest value for each element if it's a numeric vector
get_highest <- function(x) {
  if (is.list(x)) {
    x <- unlist(x)  # Flatten the list
  }
  if (is.numeric(x)) {
    return(max(x, na.rm = TRUE))
  } else {
    return(NA)  # Return NA if not numeric
  }
}

# Apply the function to each element in the list
highest_values <- sapply(extracted_values, get_highest)

#add highest elevation around in waypoint_mission
waypoints_mission <- cbind(waypoints_transformed,highest_values)

waypoints_mission <- waypoints_mission[,-c(1:8,10:14,16,17,18)]
waypoints_coord <- st_coordinates(waypoints_mission)
waypoints_mission <- cbind(st_drop_geometry(waypoints_mission),waypoints_coord)
n <- nrow(waypoints_mission)

#waypoints_mission <- waypoints_mission[,-c(3:4)]

#create necessary columns for csv document
waypoints_mission[,6:8] <- rep(0,n)


#relocating columns
waypoints_mission <- waypoints_mission %>% 
  relocate(V6) %>%
  relocate(V7, .after = cluster_labels)%>%
  relocate(highest_values, .after=Y)

waypoints_mission[,8] <- c(1:n)

#renaming columns
colnames(waypoints_mission) <- c("index", "polygon_id", "cluster_id","distance_from_takeoff_point", "lon_x","lat_y","elevation_from_dsm","order")

#Generate csv
write.csv(waypoints_mission,"20240909_bcinfairchild_CNW_m3e_waypoints_shortest_path.csv", row.names = F)


