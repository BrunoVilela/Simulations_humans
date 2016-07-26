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
# Function to select targets for difusion and takeover
getTargets <- function(cellID, myWorld, empty, nbs, traits = FALSE) {
  # empty if TRUE will keep targets with no trait, 
  #       if false will keep only targets with traits
  # nbs a neihbor class object
  AllTargets <-  nbs[[cellID]]
  PosTargets <- NULL
  
  # Figure out which of the neighboring cells are good options for this context
  nAll <- length(AllTargets)
  PosTargets <- numeric(nAll)

  
  if (empty) {
    PosTargets <- AllTargets[is.na(myWorld[AllTargets, 6])]
  } 
  if (!empty) {
    PosTargets <- AllTargets[!is.na(myWorld[AllTargets, 6])]
  }
  if (length(PosTargets) == 0) {
    PosTargets <- NULL
  }
  if (traits) {
    PosTargets <- myWorld[PosTargets, 6]
  }
  return(PosTargets)
}


#==================================================================
# Extend the tips of branches that did not reproduce to maintain
# an ultrametric tree
uniformBranchs <- function(mytree, myT) {
  # mytree the phylogenetic tree
  # myT the current time step
  if (!is.null(mytree)) {
      dist.root <- distRoot(mytree, tips = 1:Ntip(mytree), method = "patristic")
      sub <- (myT - dist.root)
      n <- Ntip(mytree)
      tips <- sapply(1:n, function(x,y) which(y==x),y=mytree$edge[,2])
      mytree$edge.length[tips] <- mytree$edge.length[tips] + sub
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


#==================================================================
# Function to add a species to a tip
# phylogenetic tree
bind.tip <- function(tree, tip.label, edge.length, where,
                     parent, target) {
  tip <- list(edge = matrix(c(3, 3, 1, 2),
                            ncol = 2, nrow = 2),
            tip.label = tip.label,
            edge.length = rep(as.numeric(edge.length), 2),
            Nnode = 1)
  class(tip) <- "phylo"
  obj <- bind.tree(tree, tip, where = where)
  return(obj)
}
