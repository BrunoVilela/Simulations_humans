####################################################################################
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
####################################################################################

library(gtools)
library(ape)
require(adephylo)

############################################
# first some simple hexagonal grid functions

# function to check whether a neighbor is out of bounds in the
checkNeighbor <- function (myHex, direction, myWorld) {
  Neigh <- as.numeric(myHex + direction)
  if (length(which(myWorld$x == Neigh[1] & 
                   myWorld$y == Neigh[2] &
                   myWorld$z == Neigh[3])) > 0) {
    return (Neigh)
  }
  else { return (c(NA, NA, NA)) }
}

# returns the coordinates of all possible neighbors (NA if neighbor is outside of desired grid system)
neighbors <- function(myHex, check = TRUE, myWorld) {
  if (check) {
    myNeighbors <- as.data.frame(rbind(
      checkNeighbor(myHex, c(0, 1, -1), myWorld), 
      checkNeighbor(myHex, c(1,  0, -1), myWorld), 
      checkNeighbor(myHex, c(-1, 1,  0), myWorld), 
      checkNeighbor(myHex, c(1, -1,  0), myWorld), 
      checkNeighbor(myHex, c(-1,  0, 1), myWorld), 
      checkNeighbor(myHex, c(0, -1, 1), myWorld)))
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
# cultural evolution. Assume a proportion P of the world is appropriate for foraging
# (env = 1; trait = 1) and the rest is good for domestication (env = 2; trait = 2)
BuildWorld <- function (R, P) {
  myWorld <- as.data.frame(matrix(c(0, 0, 0), 1, 3))
  names(myWorld) <- c('x', 'y', 'z') # cube coordinates for hexagonal grid systems (see http://www.redblobgames.com/grids/hexagons/)
  myWorld$Parent <- NA
  myWorld$BirthT <- NA
  myWorld$Trait <- NA
  if (runif(1) <= P) { myWorld$Environment <- 2}
  else { myWorld$Environment <- 1}
  
  for (i in 1:R) {
    for (j in 1:nrow(myWorld)) {
      myHex <- myWorld[j, c('x', 'y', 'z')]
      myneighbors <- neighbors(myHex, check = F)
      for (k in 1:nrow(myneighbors)) {
        if (sum(myWorld$x == myneighbors[k, 1] &
                myWorld$y == myneighbors[k, 2] &
                myWorld$z == myneighbors[k, 3]) == 0) {
          if (runif(1) <= P) { ThisEnv <- 2}
          else { ThisEnv <- 1} 
          myWorld <- rbind(myWorld, as.numeric(c(myneighbors[k, ], NA, NA, NA, ThisEnv)))
        }
      }
    }
  }
  
  myWorld$TipLabel <- paste('t', row.names(myWorld), sep = '')
  
  return (myWorld)
}

####################################################
getTargets <- function(myHex, myWorld, takeover) {
  AllTargets <- na.omit(neighbors(myHex, check = T, myWorld = myWorld))
  PosTargets <- NULL
  
  # figure out which of the neighboring cells are good options for this context
  if (takeover == F) {
    for(j in 1:nrow(AllTargets)) { # empty cells for colonization
      index <- which(myWorld$x == AllTargets[j, 1] &
                       myWorld$y == AllTargets[j, 2] &
                       myWorld$z == AllTargets[j, 3])
      if (is.na(myWorld$Trait[index])) { PosTargets <- c(PosTargets, index) }
    }
  }
  else { # neighboring cells that are foragers (potential take overs or diffusion)
    for (j in 1:nrow(AllTargets)) {
      index <- which(myWorld$x == AllTargets[j, 1] &
                       myWorld$y == AllTargets[j, 2] &
                       myWorld$z == AllTargets[j, 3])
      if (!is.na(myWorld$Trait[index])) {
        if (myWorld$Trait[index] == 1) { PosTargets <- c(PosTargets, index) }
      }
    }
  }
  return (PosTargets)
}

#########
Speciate <- function(myT, Parent, PosTargets, myWorld, mytree, NodeData, takeover) {
  # create descendant lineage
  if (length(PosTargets) > 1) { NewSoc <- sample(PosTargets, 1) } 
  else { NewSoc <- PosTargets }
  
  if( is.null(mytree) ) { 
    # create a phylo object
    mytree <- read.tree(text = paste("(t",Parent,":",myT,",t",NewSoc,":",myT,");", sep='')) 
    NodeData[1,] <- c(1,Parent)
    NodeData[2,] <- c(2,NewSoc)
  }
  else {
    # add a bifurcation to the node that used to be the parent
    BL <- myT - distRoot(mytree,1,method='patristic')
    newtips <- read.tree(text = paste("(t",Parent,":",BL,",t",NewSoc,":",BL,");", sep='')) 
    
    OldParentalNode <- NodeData$Node[which(NodeData$Tip==Parent)]
    
    mytree <- read.tree(text=write.tree(bind.tree(mytree,newtips,where=OldParentalNode), file=''))
    
    # update NodeData
    NodeData <- as.data.frame(matrix(NA,length(mytree$tip.label),2))
    names(NodeData) <- c('Node', 'Tip')
    NodeData[,1] <- 1:length(mytree$tip.label) 
    NodeData[,2] <- as.numeric(gsub('t','', mytree$tip.label))
  }
  
  # keep track of this for confirmation
  myWorld$Parent[NewSoc] <- Parent
  myWorld$BirthT[NewSoc] <- myT
  
  # define the trait value that the new society will exhibit
  if (takeover==F) {
    # we assume that the baseline is to inherit whatever the parents did
    myWorld$Trait[NewSoc] <- myWorld$Trait[Parent]
    
    #... but allow the possibility of developing new modes of subsistence de novo
    if (myWorld$Trait[Parent] == 1) { 
      if (myWorld$Environment[NewSoc]==1) {
        if (runif(1) < P.Arisal[2,1]) { myWorld$Trait[NewSoc] <- 2 } # domestication evolves
      }
      else {
        if (runif(1) < P.Arisal[2,2]) { myWorld$Trait[NewSoc] <- 2 } # domestication evolves
      }
    }
    else {
      if (myWorld$Environment[NewSoc]==1) {
        if (runif(1) < P.Arisal[1,1]) { myWorld$Trait[NewSoc] <- 1 } # foraging evolves
      }
      else {
        if (runif(1) < P.Arisal[1,2]) { myWorld$Trait[NewSoc] <- 1 } # foraging evolves
      }
    }
  }
  else { # This is a Take Over event (the earlier soc has already been wiped out of the phylogeny
    # outside of this function and now all is left is to replace it by a descendant of the parent)
    myWorld$Trait[NewSoc] <- myWorld$Trait[Parent] 
  } 
  
  return ( list("myWorld" = myWorld, "mytree" = mytree, "NodeData" = NodeData) )
}

########
Extinct <- function(mytree, NodeData, myWorld, Ext.tip) {
  
  # remove the from phylogeny
  mytree <- drop.tip(mytree, tip = NodeData$Node[which(NodeData$Tip == Ext.tip)])
  
  # update NodeData
  NodeData <- as.data.frame(matrix(NA,length(mytree$tip.label),2))
  names(NodeData) <- c('Node', 'Tip')
  NodeData[,1] <- 1:length(mytree$tip.label) 
  NodeData[,2] <- as.numeric(gsub('t','', mytree$tip.label))
  
  # remove tip from map
  myWorld$Parent[Ext.tip] <- myWorld$BirthT[Ext.tip] <- myWorld$Trait[Ext.tip] <- NA
  
  return ( list("myWorld" = myWorld, "mytree" = mytree, "NodeData" = NodeData) )
}

##########################################
RunSim <- function(myWorld, P.extinction, P.speciation, 
                   P.diffusion, P.Arisal, P.TakeOver) {
  myT = 0
  
  # initialize parameters we will use later to build the phylogeny
  rootnode <- nrow(myWorld) + 1 # standard convention for root node number
  
  # set the seed for simulation
  start <- sample(1:dim(myWorld)[1], 1)
  myWorld$Parent[start] <- 0 # root
  myWorld$BirthT[start] <- 0 # starts at time zero
  myWorld$Trait[start] <- 1 # assumes ancestral state is forager
  
  # keep track of the tip numbers for each position in myWorld (when colonized)
  NodeData <- as.data.frame(matrix(c(rootnode, start), 1, 2))
  names(NodeData) <- c('Node', 'Tip') 
  
  mytree <- NULL
  
  cat("0% [")
   
  for (steps in 1:250) {
    # screen update to allow monitoring progress
    if (steps%%25 == 0) { cat('-') }
    if (steps==250) { cat("] 100 %\n") }
    
    # add one time step to the process
    myT <- myT + 1
    
    if (length(which(!is.na(myWorld$Trait))) > 2) {
      # allow the possibility of extinction
      for ( i in which(!is.na(myWorld$Trait))) {
        if (myWorld$Trait[i] == 1) {
          if (myWorld$Environment[i] == 1) { Pext <- P.extinction["For","EnvF"] }
          else { Pext <- P.extinction["For","EnvD"] }
        }
        else {
          if (myWorld$Environment[i] == 1) { Pext <- P.extinction["Dom","EnvF"] }
          else { Pext <- P.extinction["Dom","EnvD"] }
        }
        if ( !is.null(mytree) ){
          if (length(mytree$tip.label) > 3 & runif(1) < Pext ) { 
            
            Temp <- Extinct(mytree, NodeData, myWorld, Ext.tip = i) 
            myWorld <- Temp$myWorld
            mytree <- Temp$mytree
            NodeData <- Temp$NodeData
          }
        }
      }
    }
    
    if (length(which(!is.na(myWorld$Trait))) > 2) {
      # allow possibility of diffusion (phylogenies don't change)
      UsedCells <- which(!is.na(myWorld$Trait))
      
      if (length(UsedCells) > 1) {
        for (i in UsedCells) { 
          myHex <- myWorld[i, c('x', 'y', 'z')]
          PosTargets <- getTargets(myHex, myWorld, takeover = T)
          PosTargets <- PosTargets[which(myWorld$Trait[PosTargets] != myWorld$Trait[i])]
          
          if (length(PosTargets) > 1) { NewSoc <- sample(PosTargets, 1) } 
          else { NewSoc <- PosTargets }
          
          if (length(NewSoc) == 1) {
            if (myWorld$Trait[i] == 1){
              if (myWorld$Environment[NewSoc] == 1) {
                if (runif(1) < P.diffusion[1,1]) { myWorld$Trait[NewSoc] <- myWorld$Trait[i] }
              }
              else {
                if (runif(1) < P.diffusion[1,2]) { myWorld$Trait[NewSoc] <- myWorld$Trait[i] }
              }
            }
            else {
              if (myWorld$Environment[NewSoc] == 1) {
                if (runif(1) < P.diffusion[2,1]) { myWorld$Trait[NewSoc] <- myWorld$Trait[i] }
              }
              else {
                if (runif(1) < P.diffusion[2,2]) { myWorld$Trait[NewSoc] <- myWorld$Trait[i] }
              }
            }
          }
        }
      }
       
      # allow possibility of hostile take overs (phylogenies DO change)
      if (length(UsedCells) > 1) {
        for (i in UsedCells) {   
          myHex <- myWorld[i, c('x', 'y', 'z')]
          PosTargets <- getTargets(myHex, myWorld, takeover = T)
          PosTargets <- PosTargets[which(myWorld$Trait[PosTargets] != myWorld$Trait[i])]
          
          if (length(PosTargets) > 1) { Ext.tip <- sample(PosTargets, 1) } 
          else { Ext.tip <- PosTargets }
          
          if (length(Ext.tip) == 1) { 
            TakeOver <- F # baseline
            if (myWorld$Trait[i] == myWorld$Environment[i]) { # Source is in right habitat
              if (myWorld$Trait[i] == myWorld$Environment[Ext.tip]) { # target is also in appropriate habitat
                if (runif(1) < P.TakeOver[1,1]) { TakeOver <- T }
              }
              else { if (runif(1) < P.TakeOver[2,1]) { TakeOver <- T } }
            }
            else {
              if (myWorld$Trait[i] == myWorld$Environment[Ext.tip]) { # target is also in appropriate habitat
                if (runif(1) < P.TakeOver[1,2]) { TakeOver <- T }
              }
              else { if (runif(1) < P.TakeOver[2,2]) { TakeOver <- T } }
            }
            
            if (TakeOver) {
              # eliminate any record of the society that used to occupy the chosen spot
              Temp <- Extinct(mytree, NodeData, myWorld, Ext.tip) 
              myWorld <- Temp$myWorld; mytree <- Temp$mytree; NodeData <- Temp$NodeData
              
              # and now occupy this spot with a descendant of the domest society
              Temp <- Speciate(myT = myT, Parent = i, PosTargets = Ext.tip, 
                               myWorld = myWorld, mytree = mytree, NodeData = NodeData, 
                               takeover = T)
              myWorld <- Temp$myWorld; mytree <- Temp$mytree; NodeData <- Temp$NodeData
            }
          }
        }
      }
    }
    
    # Now we can allow the colonized cells attempt to reproduce (in random order)
    not.na <- !is.na(myWorld$Trait)
    sum.notna <- sum(not.na)
    who.notna <- which(not.na)
    if (sum.notna > 1) { 
      myOrder <- sample(who.notna,
                        sum.notna)
    } else {myOrder <- who.notna}
    
    # go one by one allowing each society a chance to reproduce
    for (i in 1:sum.notna){
      Row.In.Node.Data <- which(NodeData$Tip == myOrder[i])
      
      if (myWorld$Trait[myOrder[i]] == 1) {
        if (myWorld$Environment[myOrder[i]] == 1) { Pspec <- P.speciation["For","EnvF"] }
        else { Pspec <- P.speciation["For","EnvD"] }
      }
      else {
        if (myWorld$Environment[myOrder[i]] == 1) { Pspec <- P.speciation["Dom","EnvF"] }
        else { Pspec <- P.speciation["Dom","EnvD"] }
      }
      
      if (runif(1) <= Pspec) {
        # speciate (i.e., send diaspora to an adjacent empty cell)
        myHex <- myWorld[myOrder[i], c('x', 'y', 'z')]
        PosTargets <- getTargets(myHex, myWorld, takeover = F)
        if (!is.null(PosTargets)) {
          Temp <- Speciate(myT = myT, Parent = myOrder[i], PosTargets = PosTargets, 
                           myWorld = myWorld, mytree = mytree, NodeData = NodeData, 
                           takeover = F)
          
          myWorld <- Temp$myWorld
          mytree <- Temp$mytree
          NodeData <- Temp$NodeData
        }
      }
    }
    
    if(!is.null(mytree)) {
      # extend the tips of branches that did not reproduce to maintain an ultrametric tree
      for (i in 1:length(mytree$tip.label)) {
        if (distRoot(mytree, tips = i, method="patristic") < myT) {
          ThisBranch <- which(mytree$edge[,2]==i)
          mytree$edge.length[ThisBranch] <- mytree$edge.length[ThisBranch] + (myT-distRoot(mytree, tips = i, method="patristic"))
        }
      }
    }
  } 
  
  return (list('mytree' = mytree, 'NodeData' = NodeData, 'myWorld' = myWorld))
}

# Plot output (single tree)
myplot <- function(RunSim.Output, i) {
  par(oma=c(1,1,3,1))
  mytree <- RunSim.Output[[1]][[i]]
  myWorld <- RunSim.Output[[2]][[1]]
  row.names(myWorld) <- myWorld$TipLabel
  myWorld <- myWorld[,c("Trait","Environment")]
  trait.plot(mytree, dat = myWorld, cols = list("Trait"=c('blue','red'),"Environment"=c('Black','Brown')),
            lab = list("Trait"=c('Forager', 'Domesticator'), "Environment"=c('Good4For', 'Good4Dom')),
            type='p', w = 1/70)
  title(paste(deparse(substitute(RunSim.Output)), i), outer=T)
}
