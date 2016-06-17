#==================================================================
# SimulationFunctions.R
#
# Contains utility functions for simulation of cultural evolution in space and time
# Allows for (1) Vertical Transmission (phylogenetic inheritance); (2) Horizontal 
# Transmission (cultural diffusion); (3) Ecological selection (Both speciation and
# extinction are determined by the match between the state of a binary trait and the
# environment a societuy occupies).
#
# 7 Jun 2016 
# Carlos A. Botero
# Washington University in Saint Louis
#==================================================================
# Load required packages
library(gtools)
library(ape)
library(adephylo)
library(diversitree)


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



#==================================================================
RunSim <- function(myWorld, P.extinction, P.speciation, 
                   P.diffusion, P.Arisal, P.TakeOver,
                   N.steps = 250) {
  
  world.size <- nrow(myWorld)
  # Initialize parameters we will use later to build the phylogeny
  rootnode <-  world.size + 1 # standard convention for root node number
  
  # set the seed for simulation
  start <- sample(1:world.size, 1)
  myWorld[start, 4:6] <- c(0, 0, 1) # Setting root(0), time(0), ancestral(1, forager)
  
  # Keep track of the tip numbers for each position in myWorld (when colonized)
  NodeData <- matrix(c(rootnode, start), 1, 2)
  colnames(NodeData) <- c('Node', 'Tip') 
  
  mytree <- NULL
  myT <- 0
  cat("0% [")
  
  for (steps in 1:N.steps) {
    if (steps%% (N.steps/10) == 0) { 
      cat('-') 
    }
    if (steps == N.steps) { 
      cat("] 100 %\n")
    }
    
    # Extinction time!!! buuuuu
    if (sum(P.extinction) != 0) {
      after.ext <- getExtinct(myWorld, mytree, P.extinction, NodeData)
      mytree <- after.ext$mytree
      myWorld <- after.ext$myWorld
      NodeData <- after.ext$NodeData
    }
    
    # Diffusion: passing the know-how to my neighbors
    if (sum(P.diffusion) != 0) {
      myWorld <- Diffusion(myWorld, P.diffusion, multiplier = 2)
    }
    
    # TakeOver (war time)!
    if (sum(P.TakeOver) != 0) {
      after.invasion <- TakeOver(myWorld, mytree, P.TakeOver, 
                                 NodeData, myT, multiplier = 2)
      mytree <- after.invasion$mytree
      myWorld <- after.invasion$myWorld
      NodeData <- after.invasion$NodeData
      myT <- after.invasion$myT
    }
    # Speciation (god making his job)
    after.god <- Speciation(myWorld, mytree, P.speciation,
                            P.Arisal, NodeData, myT)
    mytree <- after.god$mytree
    myWorld <- after.god$myWorld
    NodeData <- after.god$NodeData
    myT <- after.god$myT
    
    myWorld <- Arisal(myWorld)
  }
  return(list('mytree' = mytree, 'NodeData' = NodeData, 'myWorld' = myWorld))
}
