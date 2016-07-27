# Primer for simulation of cultural phylogenies in space
#
# 7 Jun 2016 
# Carlos A. Botero
# Washington University in Saint Louis

library(gtools)
library(ape)
require(data.table)

rm(list=ls())

############################################
# first some simple hexagonal grid functions

# function to check whether a neighbor is out of bounds in the
checkNeighbor <- function (myHex, direction) {
  Neigh <- as.numeric(myHex + direction)
  if (length(which(myWorld$x == Neigh[1] & 
                   myWorld$y == Neigh[2] &
                   myWorld$z == Neigh[3])) > 0) {
    return (Neigh)
  }
  else { return (c(NA, NA, NA)) }
}

# returns the coordinates of all possible neighbors (NA if neighbor is outside of desired grid system)
neighbors <- function(myHex, check = TRUE) {
  if (check) {
    myNeighbors <- as.data.frame(rbind(
      checkNeighbor(myHex, c(0, 1, -1)), 
      checkNeighbor(myHex, c(1,  0, -1)), 
      checkNeighbor(myHex, c(-1, 1,  0)), 
      checkNeighbor(myHex, c(1, -1,  0)), 
      checkNeighbor(myHex, c(-1,  0, 1)), 
      checkNeighbor(myHex, c(0, -1, 1))))
  } else {
    myNeighbors <- as.data.frame(rbind(
      myHex + c(0, 1, -1), 
      myHex + c(1,  0, -1), 
      myHex + c(-1, 1,  0), 
      myHex + c(1, -1,  0), 
      myHex + c(-1,  0, 1), 
      myHex + c(0, -1, 1)))
  }
  names(myNeighbors) <- c('x', 'y', 'z')
  row.names(myNeighbors) <- c('n1', 'n2', 'n3', 'n4', 'n5', 'n6')
  return(myNeighbors)
}

####################################
# build an hexagonal grid with a radius of R cells over which to simulate 
# cultural evolution. Assume that half the world is appropriate for foraging
# (env = 1; trait = 1) and the other half is good for domestication 
# (env = 2; trait = 2)
R <- 4
myWorld <- as.data.frame(matrix(c(0, 0, 0), 1, 3))
names(myWorld) <- c('x', 'y', 'z') # cube coordinates for hexagonal grid systems (see http://www.redblobgames.com/grids/hexagons/)
myWorld$Parent <- NA
myWorld$BirthT <- NA
myWorld$Trait <- NA
myWorld$Environment <- sample(1:2,1)

for (i in 1:R) {
  for (j in 1:nrow(myWorld)) {
    myHex <- myWorld[j, c('x', 'y', 'z')]
    myneighbors <- neighbors(myHex, check = F)
    for (k in 1:nrow(myneighbors)) {
      if (sum(myWorld$x == myneighbors[k, 1] &
              myWorld$y == myneighbors[k, 2] &
              myWorld$z == myneighbors[k, 3]) == 0) {
        myWorld <- rbind(myWorld, as.numeric(c(myneighbors[k, ], NA, NA, NA, sample(1:2,1))))
      }
    }
  }
}

myWorld$TipLabel <- paste('t', row.names(myWorld), sep = '')

