# Zoom photos workflow from Lefolab 2024
By Guillaume Tougas, Antoine-Caron Guay, Vincent Le Falher and Etienne Laliberté

1. The extract_centroids_wpt_missions() function in "extract_centroids_wpt_missions.R" file allows the user to generate a CSV file containing waypoints corresponding to precise tree crown centroids. This CSV file will be used further as an input for the lefolab-dji-waypoints algorithm that generates a fly-ready close-up photo drone mission in a .kmz format. Also, these waypoints are placed in a certain order that optimizes the drone close-up photo missions, following a shortest path algorithm (TSP).

Example of how to use the function
```R
extract_centroids_wpt_missions(trees_polygon_path = "DJI_mapping/50haplot/50haplot_treesover100m2.gpkg",
                               aoi_path = "DJI_mapping/50haplot/bci50haplot_aoi.gpkg",
                               aoi_index = 1,
                               aoi_qualifier = "so",
                               aoi_relation = "intersect",
                               dsm_path = "conrad/20241121_bci50haplot_m3e/20241121_bci50haplot_m3e_dsm.tif",
                               espg_code = 32617)
```


2. The "photo_classification_by_cluster.R" script is a way to classify all the wide and close-up ("zoom") photos within their correct cluster. It helps visualizing the accuracy of the clustering algorithm after having uploaded the photos from the previously generated and executed close-up photo drone mission.