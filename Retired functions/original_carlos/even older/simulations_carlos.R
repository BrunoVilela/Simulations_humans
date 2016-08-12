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

################################################
# Start with simple simulation (only vertical transmission to adjacent neighbors, colonize an empty world)
P.speciation <- as.data.frame(matrix(c(0.5, 0.5, 0.5, 0.5), 2, 2, byrow = T))
P.extinction <- as.data.frame(matrix(c(0, 0.2, 0.4, 0), 2, 2, byrow = T))
names(P.extinction) <- names(P.speciation) <- c("EnvF", "EnvD") 
row.names(P.extinction) <- row.names(P.speciation) <- c("For", "Dom")

P.diffusion <- as.data.frame(matrix(c(0.5, 0.5, 0.5, 0.5), 2, 2, byrow = T))
names(P.diffusion) <- c("EnvF", "EnvD") # environment of the potential Recipient
row.names(P.diffusion) <- c("For", "Dom")

P.reversal <- as.data.frame(matrix(c(0.35, 0.05), 1, 2, byrow = T)) # defines probability of inheriting the opposite trait vaue from parent
colnames(P.reversal) <- c("F2D", "D2F")

start <- sample(1:dim(myWorld)[1], 1)
myWorld$Parent[start] <- 0 # root
myWorld$BirthT[start] <- 0 # starts at time zero
myWorld$Trait[start] <- 1 # assumes ancestral state is forager

t = 0

# initialize parameters we will use later to build the phylogeny
rootnode <- nrow(myWorld) + 1 # standard convention for root node numer
curNode <- rootnode

# keep track of when each node arises and whether it is currently or not a tip (if so which)
node.Data <- as.data.frame(matrix(c(rootnode, 0, start), 1, 3))
names(node.Data) <- c('Node', 'Time', 'Tip') 
myedge <- NULL
mytiplabels <- NULL
myedgelength <- NULL
intNodes <- NULL

while(any(is.na(myWorld$Trait))) {
  # add one time step to the process
  t <- t + 1
  
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
      
      if (runif(1) <= Pext) { 
        # remove the from phylogeny
        index <- which(myedge[,2]==node.Data$Node[which(node.Data$Tip==i)])
        StartNode <- myedge[index,1] 
        NewStartNode <- myedge[which(myedge[,2] == StartNode),1]
        
        # remove the corresponding node and edge
        node.Data <- node.Data[-which(node.Data$Tip==i),]
        myedgelength <- myedgelength[-index]
        myedge <- myedge[-index,]
        
        if(length(NewStartNode)==0) {
          # this means that the society we just removed was the first one in the map (root)
          myedge <- myedge[-which(myedge[,1] == StartNode),]
        }
        else {
          #remove phantom node
          myedge[which(myedge[,1] == StartNode),1] <- NewStartNode
          myedge <- myedge[-which(myedge[,2] == StartNode),]
        }
                
        # remove tip from map
        myWorld$Parent[i] <- myWorld$BirthT[i] <- myWorld$Trait[i] <- NA
      }
    }
  }
  
  # Now we can allow the surviving cells attempt to reproduce (in random order)
  not.na <- !is.na(myWorld$Trait)
  sum.notna <- sum(not.na)
  who.notna <- which(not.na)
  if (sum.notna > 1) { 
    myOrder <- sample(who.notna,
                      sum.notna)
  } else {myOrder <- who.notna}
  
  # go one by one allowing these societies a chance to reproduce
  for (i in 1:sum.notna){
    Row.In.Node.Data <- which(node.Data$Tip == myOrder[i])
    
    if (myWorld$Trait[myOrder[i]] == 1) {
      if (myWorld$Environment[myOrder[i]] == 1) { Pspec <- P.speciation["For","EnvF"] }
      else { Pspec <- P.speciation["For","EnvD"] }
    }
    else {
      if (myWorld$Environment[myOrder[i]] == 1) { Pspec <- P.speciation["Dom","EnvF"] }
      else { Pspec <- P.speciation["Dom","EnvD"] }
    }
    
    if (runif(1) <= Pspec) {
      # speciate (i.e., colonize an adjacent cell with a descendant)
      myHex <- myWorld[myOrder[i], c('x', 'y', 'z')]
      AllTargets <- na.omit(neighbors(myHex))
      PosTargets <- NULL
      
      # figure out which of the neighboring cells have not yet been colonized
      for(j in 1:nrow(AllTargets)) {
        index <- which(myWorld$x == AllTargets[j, 1] &
                         myWorld$y == AllTargets[j, 2] &
                         myWorld$z == AllTargets[j, 3])
        if (is.na(myWorld$Trait[index])) {
          PosTargets <- c(PosTargets, index)
        }
      }
      
      if (!is.null(PosTargets)) { # there are still open spots so go ahead...
        # create descendant lineage
        if (length(PosTargets) > 1) {
          NewSoc <- sample(PosTargets, 1) # index to myWorld
        } else {
          NewSoc <- PosTargets
        }
        # bifurcate tree, start by adding the two edges and descendant nodes
        curNode <- curNode + 2
        myedge <- rbind(myedge, c(node.Data$Node[Row.In.Node.Data], curNode - 1),
                        c(node.Data$Node[Row.In.Node.Data], curNode))
        node.Data <- rbind(node.Data, c(curNode - 1, t, myOrder[i]),
                           c(curNode, t, NewSoc))
        
        # remove the link between the parent node and the tip (because the tip is now linked to a descendant node)
        node.Data$Tip[Row.In.Node.Data] <- NA
        
        # recalculate internal edge lengths (when simulation is done need to recompute terminal branches)
        myedgelength <- NULL
        for(j in 1:nrow(myedge)) {
          myedgelength[j] <- node.Data$Time[node.Data$Node == myedge[j, 2]] - 
            node.Data$Time[node.Data$Node == myedge[j, 1]]
        }
        
        # keep track of this for confirmation
        myWorld$Parent[NewSoc] <- myOrder[i]
        myWorld$BirthT[NewSoc] <- t
        
        if (runif(1) <= P.reversal[myWorld$Trait[myOrder[i]]]) {
          myWorld$Trait[NewSoc] <- myWorld$Trait[myOrder[i]]
        } else { 
          if (myWorld$Trait[myOrder[i]] == 1) {
            myWorld$Trait[NewSoc] <- 2
          } else { 
            myWorld$Trait[NewSoc] <- 1
          }
        }
      }
    }
  }
} 

# now we can adjusts the terminal branches
TerminalNodes <- which(!is.na(node.Data$Tip))
TipIndex <- which(myedge[, 2] %in% node.Data$Node[TerminalNodes])
myedge[TipIndex, 2] <- node.Data$Tip[TerminalNodes]
for ( i in TipIndex ) {
  myedgelength[i] <- t - node.Data$Time[which(node.Data$Node==myedge[i, 1])]
}

nnode <- length(unique(myedge[, 1]))
mytree <- list("edge" = myedge, "tip.label" = paste0("t", sort(node.Data$Tip[TerminalNodes])), 
               "edge.length" = myedgelength, "Nnode" =  nnode)
attributes(mytree) <- list(names = c("edge", "tip.label", "edge.length", "Nnode"), 
                           class = "phylo")

newNode <- (Ntip(mytree) + 1):(Ntip(mytree) + Nnode(mytree))

Nodes <- sort(unique(myedge[, 1]))

for(i in 1:length(newNode)) {
  myedge[myedge == Nodes[i]] <- newNode[i]
}

# build the final tree and root it
nnode <- length(unique(myedge[, 1]))
mytree <- list("edge" = myedge, "tip.label" = paste0("t", sort(node.Data$Tip[TerminalNodes])), 
               "edge.length" = myedgelength, "Nnode" =  nnode)
attributes(mytree) <- list(names = c("edge", "tip.label", "edge.length", "Nnode"), 
                           class = "phylo")

is.rooted(mytree)
plot(mytree)

