# Zoom photos workflow from Lefolab 2024
By Guillaume Tougas, Antoine-Caron Guay, Vincent Le Falher and Etienne Laliberté

1. The "extract_coord_all_clusters_zoom_wpt_missions.R" script allows the user to generate a CSV file containing waypoints corresponding to precise tree crown centroids. This CSV file will be used further as an input for the lefolab-dji-waypoints algorithm that generates a fly-ready close-up photo drone mission in a .kmz format. Also, these waypoints are placed in a certain order that optimizes the drone close-up photo missions, following a shortest path algorithm (TSP).

2. The "photo_classification_by_cluster.R" script is a way to classify all the wide and close-up ("zoom") photos within their correct cluster. It helps visualizing the accuracy of the clustering algorithm after having uploaded the photos from the previously generated and executed close-up photo drone mission.
