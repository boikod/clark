# basic functions

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
