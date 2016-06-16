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
  myT <- 0 # Counter
  
  cat("0% [")
  
  for (steps in 1:N.steps) {
    # screen update to allow monitoring progress
    if (steps %% (N.steps/10) == 0) { 
      cat('-')
    }
    if (steps == N.steps) { 
      cat("] 100 %\n") 
    }
    # add one time step to the process
    myT <- myT + 1
    
    # Extinction time!!! buuuuu
    after.ext <- getExtinct(myWorld, mytree, P.extinction, NodeData)
    mytree <- after.ext$mytree
    myWorld <- after.ext$myWorld
    NodeData <- after.ext$NodeData
    
    # Diffusion: passing the know-how to my neighbors
    myWorld <- Diffusion(myWorld, P.diffusion, multiplier = 2)
    
    
    # TAKEOVER
    after.invasion <- TakeOver(myWorld, mytree, P.TakeOver, 
                               NodeData, multiplier = 2)
    mytree <- after.invasion$mytree
    myWorld <- after.invasion$myWorld
    NodeData <- after.invasion$NodeData
    
    # Now we can allow the colonized cells attempt to reproduce (in random order)
    not.na <- !is.na(myWorld$Trait)
    sum.notna <- sum(not.na)
    who.notna <- which(not.na)
    if (sum.notna > 1) { 
      myOrder <- sample(who.notna,
                        sum.notna)
    } else {
      myOrder <- who.notna
    }
    
    # go one by one allowing each society a chance to reproduce
    for (i in 1:sum.notna) {
      Row.In.Node.Data <- which(NodeData$Tip == myOrder[i])
      
      if (myWorld$Trait[myOrder[i]] == 1) {
        if (myWorld$Environment[myOrder[i]] == 1) { 
          Pspec <- P.speciation["For","EnvF"]
        } else {
          Pspec <- P.speciation["For","EnvD"] 
        }
      } else {
        if (myWorld$Environment[myOrder[i]] == 1) {
          Pspec <- P.speciation["Dom","EnvF"]
        } else {
          Pspec <- P.speciation["Dom","EnvD"]
        }
      }
      
      if (runif(1) <= Pspec) {
        # speciate (i.e., send diaspora to an adjacent empty cell)
        myHex <- myWorld[myOrder[i], c('x', 'y', 'z')]
        PosTargets <- getTargets(myHex, myWorld, takeover = FALSE)
        if (!is.null(PosTargets)) {
          Temp <- Speciate(myT = myT, Parent = myOrder[i], PosTargets = PosTargets, 
                           myWorld = myWorld, mytree = mytree, NodeData = NodeData, 
                           takeover = FALSE)
          
          myWorld <- Temp$myWorld
          mytree <- Temp$mytree
          NodeData <- Temp$NodeData
        }
      }
    }
    
    if (!is.null(mytree)) {
      # Extend the tips of branches that did not reproduce to maintain
      # an ultrametric tree
      for (i in 1:length(mytree$tip.label)) {
        dist.root <- distRoot(mytree, tips = i, method = "patristic")
        if (dist.root < myT) {
          ThisBranch <- which(mytree$edge[, 2] == i)
          sub <- (myT - dist.root)
          mytree$edge.length[ThisBranch] <- mytree$edge.length[ThisBranch] + sub
        }
      }
    }
  } 
  
  return(list('mytree' = mytree, 'NodeData' = NodeData, 'myWorld' = myWorld))
}


#==================================================================
# Plot output (single tree)
myplot <- function(RunSim.Output, i) {
  par(oma = c(1, 1, 3, 1))
  mytree <- RunSim.Output[[1]][[i]]
  myWorld <- RunSim.Output[[2]][[1]]
  row.names(myWorld) <- myWorld$TipLabel
  myWorld <- myWorld[, c("Trait", "Environment")]
  colors <- list("Trait" = c('blue', 'red'), 
                 "Environment" = c('Black','Brown'))
  labels <- list("Trait" = c('Forager', 'Domesticator'),
                 "Environment" = c('Good4For', 'Good4Dom'))
  trait.plot(mytree, dat = myWorld, cols = colors,
             lab = labels, type = 'p', w = 1/70)
  title(paste(deparse(substitute(RunSim.Output)), i), outer = TRUE)
}
