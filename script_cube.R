rm(list=ls())

# install.packages('pracma')
library(pracma)

# Rotation function
# Input: Matrix 8x3 coordinates, Float angle in radians, String axis from ['x', 'y', 'z']
rotate <- function(coords, angle, axis) {
  
  if (axis == 'x') {
    
    rotation_matrix <- matrix(c(1, 0, 0,
                                0, cos(angle), -sin(angle),
                                0, sin(angle), cos(angle)), ncol = 3, byrow = TRUE)
    
    return(coords %*% t(rotation_matrix))
    
  } else if (axis == 'y') {
    
    rotation_matrix <- matrix(c(cos(angle), 0, sin(angle),
                                0, 1, 0,
                                -sin(angle), 0, cos(angle)), ncol = 3, byrow = TRUE)
    
    return(coords %*% t(rotation_matrix))
    
  } else if (axis == 'z') {
    
    rotation_matrix <- matrix(c(cos(angle), -sin(angle), 0,
                                sin(angle), cos(angle), 0,
                                0, 0, 1), ncol = 3, byrow = TRUE)
    
    return(coords %*% t(rotation_matrix))
  }
}

# Function to find face normals (cross product of 3 points of a face)
get_normal <- function(geometry_coords, faces_coords, face) {
  
  v1 <- geometry_coords[faces_coords[face, 3], ] - geometry_coords[faces_coords[face, 2], ]
  v2 <- geometry_coords[faces_coords[face, 1], ] - geometry_coords[faces_coords[face, 2], ]
  
  return(cross(v1, v2))
}

# Function to find visible edges
find_visible_edges <- function(geometry_coords, faces_coords, faces_edges, geometry_edges, viewer_vector) {
  
  visible_edges = matrix(nrow = 0, ncol = 2)
  
  for (face in 1:nrow(faces_coords)) {
    normal <- get_normal(geometry_coords, faces_coords, face)
    
    if (dot(viewer_vector, normal) < 0) {
      visible_edges <- rbind(visible_edges, geometry_edges[faces_edges[face, ], ])
    }
  }
  
  return(unique(visible_edges))
}

# Function to get visible faces and the dot product between their normals and light vector
get_polygons <- function(geometry_coords, faces_coords, light_vector, viewer_vector) {
  
  # polygons matrix will store the indices of faces and the value of lightness
  polygons = matrix(nrow = 0, ncol = 2)
  
  for (face in 1:nrow(faces_coords)) {
    normal <- get_normal(geometry_coords, faces_coords, face)
    
    if (dot(viewer_vector, normal) < 0) {
      # calculate lightness as a negative dot product of normal with a light vector
      lightness = floor(-dot(light_vector, normal))
      # normalize to a scale of 10 colors (can be more if more colors are added later)
      lightness <- if (lightness < 1) 1 else lightness
      lightness <- if (lightness >10 ) 10 else lightness
      
      polygons = rbind(polygons, c(face, lightness))
    }
  }
  
  return(polygons)
}

# Create an 8x3 matrix with the coordinates of the cube's vertices (initially standing straight)
geometry_coords <- matrix(c(-1, -1, -1,
                        1, -1, -1,
                        1, 1, -1,
                        -1, 1, -1,
                        -1, -1, 1,
                        1, -1, 1,
                        1, 1, 1,
                        -1, 1, 1), ncol = 3, byrow = TRUE)

# And instantiate all the cube's edges
geometry_edges <- matrix(c(1, 2, 
                       2, 3, 
                       3, 4, 
                       4, 1,
                       5, 6, 
                       6, 7, 
                       7, 8, 
                       8, 5,
                       1, 5, 
                       2, 6, 
                       3, 7, 
                       4, 8), ncol = 2, byrow = TRUE)

# Faces instantiated by their 4 vertices
faces_coords <- matrix(c(4, 3, 2, 1,
                       2, 3, 7, 6,
                       5, 8, 4, 1,
                       1, 2, 6, 5,
                       7, 3, 4, 8,
                       5, 6, 7, 8), ncol = 4, byrow = TRUE)

# Faces instantiated by their edges (the same order as in faces_coords)
faces_edges <- matrix(c(1, 2, 3, 4,
                        2, 11, 6, 10,
                        8, 12, 4, 9,
                        1, 10, 5, 9,
                        11, 3, 12, 7,
                        5, 6, 7, 8), ncol = 4, byrow = TRUE)

# Set the step angles (any) to rotate the cube
angle_x = 5 * (pi / 180) 
angle_y = 10 * (pi / 180)
angle_z = 5 * (pi / 180)

# set the vector of the viewport (z axis into the screen)
viewer_vector = c(0, 0, 1)
# set the vector of the lighting for shading (preferably unit vector)
light_vector = c(-2/sqrt(3), -2/sqrt(3), 2/sqrt(3))
# set a vector of color values (first darker then lighter)
lightness_levels <- c(
  "#770000", "#990000", "#BB0000", "#E21F1F",
  "#EF3232", "#FF5555", "#FF7777", "#FF9999",
  "#FFBBBB", "#FFDDDD"
)

# Plot the cube in a while loop to create an animation
while (TRUE) {
  
  geometry_coords <- rotate(geometry_coords, angle_x, 'x')
  geometry_coords <- rotate(geometry_coords, angle_y, 'y')
  geometrye_coords <- rotate(geometry_coords, angle_z, 'z')
  
  plot(geometry_coords[, 1], geometry_coords[, 2], type = "n", asp = 1,
       xlim = c(-2, 2), ylim = c(-2, 2), xlab = '', ylab = '')
  
  visible_edges = find_visible_edges(geometry_coords, faces_coords, faces_edges, geometry_edges, viewer_vector)
  
  for (edge in 1:nrow(visible_edges)) {
    lines(geometry_coords[visible_edges[edge, ], 1], geometry_coords[visible_edges[edge, ], 2])
  }
  
  polygons = get_polygons(geometry_coords, faces_coords, light_vector, viewer_vector)
  
  for (i in 1:nrow(polygons)) {
  
    x = geometry_coords[faces_coords[polygons[i, 1], ], 1]
    y = geometry_coords[faces_coords[polygons[i, 1], ], 2]
    polygon(x, y, col = lightness_levels[polygons[i, 2]])
    
  }
  
  # Also plot vertex indices for debug
  # for (i in 1:8) {
  #   text(geometry_coords[i, 1], geometry_coords[i, 2], labels = i, pos = 3)
  # }
  
  Sys.sleep(0.1)
}

