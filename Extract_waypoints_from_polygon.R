#Download libraries
library(sf)
library(TSP)
library(dplyr)
library(ggplot2)

#Import kmls
hertelnorth <- st_read("hertelnorth_reduced.kml")
gault_larger <- st_read("Gault_larger_area.shp")
st_crs(hertelnorth)
st_crs(gault_larger)
hertelnorth <- st_transform(hertelnorth, 32618)
gault_larger <- st_transform(gault_larger, 32618)
st_crs(hertelnorth)
st_crs(gault_larger)
#plot
ggplot(hertelnorth)+
  geom_sf()+
  theme_bw()

#Create bounding box
bbox <- st_bbox(gault_larger)
cell_size <- 50
grid <- st_make_grid(gault_larger, cellsize = cell_size ,what = "centers")

# Convert the grid to a data frame and keep only points within the polygon
grid_sf <- st_sf(geometry = grid)
grid_sf <- grid_sf[gault_larger,]

# Define the rotation function
rotate_points <- function(grid_sf, angle, origin = c(0, 0)) {
  # Convert angle to radians 
  angle <- angle * pi / 180
  
  # Create the rotation matrix
  rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), ncol = 2)
  
  # Get coordinates from sf object
  coords <- st_coordinates(grid_sf)
  
  # Translate points to origin
  translated_coords <- sweep(coords, 2, origin, FUN = "-")
  
  # Apply the rotation matrix
  rotated_coords <- translated_coords %*% rotation_matrix
  
  # Translate points back to original location
  rotated_coords <- sweep(rotated_coords, 2, origin, FUN = "+")
  
  # Convert back to sf object
  rotated_sf <- st_as_sf(data.frame(X = rotated_coords[, 1], Y = rotated_coords[, 2]), coords = c("X", "Y"), crs = st_crs(grid_sf))
  
  return(rotated_sf)
}

# Define the angle of rotation (e.g., 45 degrees)
angle <- 55
# Calculate the centroid of the polygon as the origin of rotation
origin <- st_coordinates(st_centroid(st_union(grid_sf)))

# Apply the rotation to the grid points
rotated_grid_sf <- rotate_points(grid_sf, angle, origin)

#Fit grid within area
fitgrid_hertelnorth <- st_intersection(hertelnorth,rotated_grid_sf)[,3]

#Visualization
ggplot()+
  geom_sf(data=hertelnorth)+
  geom_sf(data=fitgrid_hertelnorth)+
  theme_bw()

#Shortest path
#TSP algorithm

# Calculate the distance matrix between waypoints
dist_matrix <- st_distance(fitgrid_hertelnorth)

# Convert distance matrix to a symmetric matrix
dist_matrix <- as.matrix(dist_matrix)
dist_matrix_numeric <- units::drop_units(dist_matrix)

# Create a TSP object
tsp <- TSP(dist_matrix_numeric)

# Solve the TSP using a heuristic algorithm (e.g., nearest neighbor)
tour <- solve_TSP(tsp, method = "identity")

# Get the order of waypoints from the solution
waypoint_order <- as.integer(tour)

# Reorder the waypoints
sorted_waypoints <- fitgrid_hertelnorth[waypoint_order, ]

# Plot the sorted waypoints to visualize the S-pattern
plot(st_geometry(sorted_waypoints), type = "b", col = "blue", main = "Waypoints in S-pattern")
text(st_coordinates(sorted_waypoints), pos = 4)

#Visualization
ggplot()+
  geom_sf(data=hertelnorth)+
  geom_sf(data=fitgrid_hertelnorth)+
  theme_bw()

#retransform waypoint coordinates in WGS84
waypoints_50 <- st_transform(sorted_waypoints, 4326)
waypoints_50 <- st_coordinates(waypoints_50)
write.csv(waypoints_50,"waypoints_coord_55deg_50m_hertelnorth.csv")