#==================================================================
# Build the world based on real coordinates and real conditions.
# coords: a matrix of longitude and latitude in this order.
# conditions: a vector indicating how good the conditions are for agriculture

BuildWorld <- function(coords, conditions) {
  n <- nrow(coords)
  myWorld <- matrix(ncol = 8, nrow = n)
  myWorld[, 2:3] <- coords
  myWorld[, c(1, 8)] <- 1:n
  colnames(myWorld) <- c('cellID', 'Longitude', 'Latitude', 
                         "Parent", "BirthT", "Trait", 
                         "Environment", "TipLabel")
  return(myWorld)
}