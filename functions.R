# basic functions

# 1. input a time-travel matrix produced by r5r package
# 2. input a reference (root) point
# 3. select a destination point
# 4. find the shortest path
# 5. calculate new coordinates of each point on this path

#----new points----

# new points
new_points <- function(points, time_matrix, speed = 0.5) {
  # Initialize a data frame to store new points
  new_points <- points
  new_points$x_new <- new_points$x
  new_points$y_new <- new_points$y
  
  # Iterate over each unique pair in time_matrix
  i = 1
  for (i in 1:nrow(time_matrix)) {
    from_point <- time_matrix$from[i]
    to_point <- time_matrix$to[i]
    time_value <- time_matrix$time[i]
    
    # Extract coordinates of points in the current pair
    point_from <- points[points$point == from_point, c("x", "y")]
    point_to <- points[points$point == to_point, c("x", "y")]
    
    if (nrow(point_from) == 0 | nrow(point_to) == 0) {
      next  # Skip if either point is not found
    }
    
    # Compute new coordinates for the `from_point`
    direction <- point_to - point_from
    distance <- sqrt(sum(direction^2))
    if (distance == 0) {
      next  # Skip if points are the same
    }
    new_coords <- point_from + time_value * speed * direction / distance
    
    # Update the new coordinates in new_points data frame
    new_points$x_new[new_points$point == from_point] <- new_coords[1]
    new_points$y_new[new_points$point == from_point] <- new_coords[2]
  }
  
  # Prepare the result data frame with updated coordinates
  result <- new_points[, c("point", "x_new", "y_new")]
  colnames(result) <- c("point", "x", "y")
  
  return(result)
}

# Example usage
points <- data.frame(
  point = c("R", "A", "B", "C"),
  x = c(1, 4, 5, 8),
  y = c(2, 6, 8, 10)
)

time_matrix <- data.frame(
  from = c("R", "A", "B"),
  to = c("A", "B", "C"),
  time = c(6, 10, 20)
)

new_points_df <- new_points(points, time_matrix)
print(new_points_df)


points$x <- as.numeric(points$x)
points$y <- as.numeric(points$y)
new_points_df$x <- as.numeric(new_points_df$x)
new_points_df$y <- as.numeric(new_points_df$y)

library(tidyverse)
ggplot() +
  geom_point(data = points, aes(x = x, y = y), color = "blue", size = 3, shape = 16) +
  geom_point(data = new_points_df, aes(x = x, y = y), color = "red", size = 4, shape = 17, alpha = 0.5) +
  theme_minimal()


#----real data----
options(timeout = 1200)
install.packages("usethis")

# packages
library(tidyverse)
library(sf)
library(tidytransit)
library(usethis)
library(r5r)
library(osmextract)

# OSM
# usethis::edit_r_environ()
# Add a line containing: OSMEXT_DOWNLOAD_DIRECTORY=/path/to/save/files
oe_download_directory()
osm_han = oe_get("Hannover", quiet = FALSE)
highway <- osm_han["highway"]
table(highway$highway)
bbox <- st_as_sfc(st_bbox(highway), crs = 4326)
class(bbox)

# data
setwd("/Users/valeria/GTFS")
area <- read_sf("region_hannover.geojson")
gtfs_han <- tidytransit::read_gtfs("gtfs_han.zip")

# stops
stops <- stops_as_sf(gtfs_han$stops)
stops <- st_join(stops, area, join = st_within) %>% filter(!is.na(NUTS)) %>%
  #st_drop_geometry() %>%
  select(stop_name) %>%
  group_by(stop_name) %>%
  slice(1) %>%
  ungroup()

# visual
highway2 <- highway %>% filter(highway == "primary" | highway == "secondary")
ggplot() +
  geom_sf(data = st_geometry(stops)) +
  geom_sf(data = highway2)
  #geom_sf(data = bbox, aes(col = "grey", alpha = 0.5))

# Java problems
#library(rJavaEnv)
#rJavaEnv::java_quick_install(version = 21)
#use_java(21)
#java_check_version_cmd()
#java_check_version_rjava()
#rJava::.jcall("java.lang.System", "S", "getProperty", "java.version")
# sudo R CMD javareconf JAVA_HOME=/Users/valeria/Library/Caches/org.R-project.R/R/rJavaEnv/installed/macos/x64/21

# network
options(java.parameters = '-Xmx8G')
#r5r::download_r5(force_update = TRUE)
r5r_core <- setup_r5("/Users/valeria/GTFS")



plot(st_geometry(stops))




