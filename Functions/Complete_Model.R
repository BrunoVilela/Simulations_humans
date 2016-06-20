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
    if (steps %% (N.steps/10) == 0) { 
      cat('-') 
    }
    if (steps == N.steps) { 
      cat("] 100 %\n")
    }
    
    # Extinction time!!! buuuuu
    if (sum(P.extinction) != 0) {
      after.ext <- Extinction(myWorld, mytree, P.extinction, NodeData)
      mytree <- after.ext$mytree
      myWorld <- after.ext$myWorld
      NodeData <- after.ext$NodeData
    }
    
    # Diffusion: passing the know-how to my neighbors
    if (sum(P.diffusion) != 0) {
      myWorld <- Diffusion(myWorld, P.diffusion, multiplier = 2)
    }
    # Speciation / takeover
    after.god.invasion <- SpeciationTakeOver(myWorld, mytree, P.speciation,
                                             P.TakeOver, NodeData, myT)
    
    mytree <- after.god.invasion$mytree
    myWorld <- after.god.invasion$myWorld
    NodeData <- after.god.invasion$NodeData
    myT <- after.god.invasion$myT
    # Arisal
    myWorld <- Arisal(myWorld, P.Arisal)
  }
  return(list('mytree' = mytree, 'NodeData' = NodeData, 'myWorld' = myWorld))
}
