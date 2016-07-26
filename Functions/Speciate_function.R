# Specitaion function
speciate <- function(myT, Parent, PosTargets, myWorld,
                     mytree, NodeData, BL) {
  # Create descendant lineage
  if (length(PosTargets) > 1) {
    PosTargets <- sample(PosTargets, 1)
  }
  
  # Add a bifurcation to the node that used to be the parent
  if (!is.null(mytree)) {
    OldParentalNode <- NodeData[NodeData[, 2] == Parent, 1]
    mytree <- bind.tip(mytree, c(paste0("t", Parent), 
                                 paste0("t", PosTargets)), 
                       edge.length = BL, where = OldParentalNode,
                       Parent, PosTargets)
    
    # update NodeData
    tip.length <- Ntip(mytree)
    NodeData <- matrix(NA, tip.length, 2)
    colnames(NodeData) <- c('Node', 'Tip')
    NodeData <- cbind(1:tip.length, as.numeric(gsub("t", "", mytree$tip.label)))
  } else { 
    mytree <- read.tree(text = paste0("(t", Parent, ":",
                                      myT, ",t", PosTargets,
                                      ":", myT, ");")) 
    NodeData <- rbind(c(1, Parent), c(2, PosTargets))
  } 
  
  # keep track of this for confirmation
  myWorld[PosTargets, 4] <- Parent
  myWorld[PosTargets, 5] <- myT
  myWorld[PosTargets, 6] <- myWorld[Parent, 6]
  
  
  return(list("myWorld" = myWorld, "mytree" = mytree,
              "NodeData" = NodeData))
}