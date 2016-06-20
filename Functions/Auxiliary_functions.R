# Functions to make the code cleaner and less repetitive


#==================================================================
# Function to make the parameters matrix automatic
parameters <- function(prob1, prob2, prob3, prob4, colname1, colname2, 
                       rowname1, rowname2) {
  result <- matrix(c(prob1, prob2, prob3, prob4), 2, 2, byrow = TRUE)
  colnames(result) <- c(colname1, colname2)
  rownames(result) <- c(rowname1, rowname2)
  return(result)
}
# Example
# parameters(0.05, 0, 0, 0.05, "test1", "test2", "test3", "test4")

#==================================================================
# Function to check if a cell is inside the world
is.inside <- function(x, y, response = "logical") {
  # x = values to be checked inside y
  # y = myworld coordinates in the same order as y
  # response can be logical or index numbers
  index.neigh <- apply(x, 1, paste, collapse = " ")
  index.world <- apply(na.omit(y), 1, paste, collapse = " ")
  if (response == "logical") {
  answer <- index.neigh %in% index.world
  }
  if (response == "index") {
    answer <- match(index.neigh, index.world)
  }
  return(answer)
}


#==================================================================
# Function to select targets for difusion and takeover
getTargets <- function(myHex, myWorld, empty) {
  # empty if TRUE will keep targets with no trait, 
  #       if false will keep only targets with traits

  AllTargets <- neighbors(myHex, inside = TRUE,
                          myWorld = myWorld)
  PosTargets <- NULL
  
  # Figure out which of the neighboring cells are good options for this context
  nAll <- nrow(AllTargets)
  PosTargets <- numeric(nAll)
  
  indexs <- is.inside(x = AllTargets, y = myWorld[, 1:3],
                      response = "index")
  
  if (empty) {
    PosTargets <- indexs[is.na(myWorld[indexs, 6])]
  } 
  if (!empty) {
    PosTargets <- indexs[!is.na(myWorld[indexs, 6])]
  }
  if (length(PosTargets) == 0) {
    PosTargets <- NULL
  }
  return(PosTargets)
}

#==================================================================
# First some simple hexagonal grid functions
# Returns the coordinates of all possible neighbors 
# inside = TRUE will only return neighbors that are inside the matrix, 
# FALSE only the outside ones
neighbors <- function(myHex, inside = TRUE, myWorld) {
  myNeighbors <- structure(c(0, 1, -1, 1, -1, 0, 1, 0, 1, -1, 0, -1, -1, -1, 0, 
                             0, 1, 1), .Dim = c(6L, 3L))
  myNeighbors <- t(apply(myNeighbors, 1, function(x, y){x + y}, y = myHex))
  neigh.inside <- is.inside(x = myNeighbors, y = myWorld[, 1:3])
  if (inside) {
    myNeighbors <- myNeighbors[neigh.inside, , drop = FALSE] 
  }
  if(!inside) {
    myNeighbors <- myNeighbors[!neigh.inside, , drop = FALSE] 
  }
  colnames(myNeighbors) <- c('x', 'y', 'z')
  return(myNeighbors)
}

#==================================================================
# Extend the tips of branches that did not reproduce to maintain
# an ultrametric tree
uniformBranchs <- function(mytree, myT) {
  # mytree the phylogenetic tree
  # myT the current time step
  if (!is.null(mytree)) {
    for (i in 1:length(mytree$tip.label)) {
      dist.root <- distRoot(mytree, tips = i, method = "patristic")
      if (dist.root < myT) {
        ThisBranch <- which(mytree$edge[, 2] == i)
        sub <- (myT - dist.root)
        mytree$edge.length[ThisBranch] <- mytree$edge.length[ThisBranch] + sub
      }
    }
  }
  return(mytree)
}

#==================================================================
# Function to remove the species from the world and the 
# phylogenetic tree
extinct <- function(mytree, remove, myWorld) {
  mytree <- drop.tip(mytree, tip = paste0("t", myWorld[remove, 8]))
  myWorld[remove, 4:6] <- NA
  # update NodeData
  tip.length <- Ntip(mytree)
  NodeData <- matrix(NA, tip.length, 2)
  colnames(NodeData) <- c('Node', 'Tip')
  NodeData[, 1] <- 1:tip.length
  NodeData[, 2] <- as.numeric(gsub("t", "", mytree$tip.label))
  return(list("mytree" = mytree, "myWorld" = myWorld,
              "NodeData" = NodeData))  
}