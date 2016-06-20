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
                   N.steps = 250, multiplier = 1.3) {
  
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
  
  
  input <- list(P.speciation, P.Arisal, P.diffusion, P.extinction, P.TakeOver,
                myWorld, mytree, NodeData, myT, multiplier)
  
  
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
      input <- Extinction(input)
    }
    
    # Diffusion: passing the know-how to my neighbors
    if (sum(P.diffusion) != 0) {
      input <- Diffusion(input)
    }
    # Speciation / takeover
    input <- SpeciationTakeOver(input)
  
    # Arisal
    myWorld <- Arisal(input)
  }
  return(list('mytree' = mytree, 'NodeData' = NodeData, 'myWorld' = myWorld))
}
