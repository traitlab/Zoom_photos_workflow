# Load necessary libraries
library(tidyverse)
library(stringr)
library(sf)

# Set directories
setwd("C:/Users/guill/OneDrive/Documents/TougasG_PhD_2024-2028/T1-A24/Lab meeting presentations/16 octobre 2024")
photo_dir <- "C:/Users/guill/OneDrive/Documents/TougasG_PhD_2024-2028/T1-A24/Lab meeting presentations/16 octobre 2024"

destination_dir <- "C:/Users/guill/OneDrive/Documents/TougasG_PhD_2024-2028/T1-A24/Lab meeting presentations/16 octobre 2024/Clusters"


# Read polygon IDs and file information
tree_polygon_ids <- read_sf("C:/Users/guill/OneDrive/Documents/TougasG_PhD_2024-2028/Voyage Panama Septembre 2024/Data/Spatial/bcinfairchild/all_centroids_bcinfairchild_E.gpkg")

photo_list <- list.files(photo_dir, pattern = "\\.JPG$", ignore.case = TRUE)
photo_list_df <- as.data.frame(photo_list)
photo_list_df$polygon_id <- regmatches(photo_list_df$photo_list, gregexpr("(?<=_V_)\\d+", photo_list_df$photo_list, perl = TRUE))
colnames(photo_list_df) <- c("file_name", "polygon_id")


df_nclusterandfiles <- merge(tree_polygon_ids, photo_list_df, by = "polygon_id")
df_nclusterandfiles$cluster_labels <- as.character(df_nclusterandfiles$cluster_labels)
df_nclusterandfiles$polygon_id <- as.character(df_nclusterandfiles$polygon_id)


summary(df_nclusterandfiles)

# Function to move files based on cluster labels
move_photos <- function(id, cluster_label, source_dir, destination_dir) {
  # List all files in the source directory
  all_files <- list.files(source_dir, pattern = "\\.JPG$", full.names = TRUE)
  
  # Extract the last 4 numeric characters from the basename of each file
  extract_last_4_digits <- function(file_name) {
    match <- regmatches(file_name, regexpr("\\d{4}(?=[^\\d]*\\.JPG$)", file_name, perl = TRUE))
    if (length(match) == 0) {
      return(NA)  # Return NA if no match is found
    } else {
      return(match)  # Return the matched digits
    }
  }
  
  # Find files that contain the polygon_id in the last 4 digits of their names
  matching_files <- all_files[sapply(basename(all_files), function(f) extract_last_4_digits(f) == id)]
  print(matching_files)
  
  # Define the destination folder
  destination_folder <- file.path(destination_dir, cluster_label)
  
  # Create destination folder if it doesn't exist
  if (!dir.exists(destination_folder)) {
    dir.create(destination_folder, recursive = TRUE)
  }
  
  # Move each matching file
  for (file in matching_files) {
    destination_file <- file.path(destination_folder, basename(file))
    success <- file.rename(file, destination_file)
    if (!success) {
      warning(paste("Failed to move", file, "to", destination_file))
    }
  }
}

# Apply the function to each row
for (i in 1:nrow(df_nclusterandfiles)) {
  row <- df_nclusterandfiles[i, ]
  move_photos(row$polygon_id, row$cluster_labels, photo_dir, destination_dir)
}

