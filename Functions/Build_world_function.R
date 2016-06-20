#==================================================================
# Build an hexagonal grid with a radius of R cells over which to simulate 
# cultural evolution. Assume a proportion P of the world is appropriate for foraging
# (env = 1; trait = 1) and the rest is good for domestication (env = 2; trait = 2)

BuildWorld <- function (R, P) {
  # R is the radius of cells of the hexagonal world
  # P is the distribution of habitas type 1 over type 2
  # Cube coordinates for hexagonal grid systems 
  # (see http://www.redblobgames.com/grids/hexagons/)
  
  # Calculate matrix size
  n <- (3 + (2 * (R - 1)))
  nrow.world <- sum(((n - 1):(n - R)) * 2, n)
  # Calculate loop size
  n2 <- (3 + (2 * (R - 2)))
  loop <- sum(((n2 - 1):(n2 - (R - 1))) * 2, n2)
  # Empty Matrix
  myWorld <- matrix(NA, ncol = 8, nrow = nrow.world)
  myWorld[1, 1:3] <- 0 
  myWorld[, 7] <- sample(1:2, nrow.world, TRUE, prob = c(1 - P, P))
  x <- 1 # Counter
  for (j in 1:loop) {
    x <- x + 1
    myHex <- myWorld[j, 1:3]
    myneighbors <- neighbors(myHex, inside = FALSE, myWorld)
    x2 <- (nrow(myneighbors) + x) - 1
    myWorld[x:x2, 1:3]  <- myneighbors
    x <- x2
  }
  colnames(myWorld) <- c('x', 'y', 'z', "Parent", "BirthT", "Trait", "Environment",
                         "TipLabel")
  myWorld[, 8] <- 1:nrow.world
  return(myWorld)
}