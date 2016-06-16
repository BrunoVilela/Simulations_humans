# Specitaion function

speciate <- function(myT, Parent, PosTargets, myWorld,
                     mytree, NodeData, takeover, 
                     arisal = TRUE) {
  # Create descendant lineage
  if (length(PosTargets) > 1) {
    PosTargets <- sample(PosTargets, 1)
  }
  
  # Add a bifurcation to the node that used to be the parent
  if (!is.null(mytree)) {
    BL <- myT - distRoot(mytree, 1, method = 'patristic')
    newtips <- read.tree(text = paste0("(t", Parent, ":",
                                       BL, ",t", PosTargets,
                                       ":", BL, ");")) 
    
    OldParentalNode <- NodeData[NodeData[, 2] == Parent, 1]
    
    mytree <- read.tree(text = write.tree(bind.tree(mytree, 
                                                    newtips,
                                                    where = OldParentalNode),
                                          file = ''))
    
    # update NodeData
    tip.length <- Ntip(mytree)
    NodeData <- matrix(NA, tip.length, 2)
    colnames(NodeData) <- c('Node', 'Tip')
    NodeData <- cbind(1:tip.length, mytree$tip.label)
  }
  
  # Create a phylo object
  if (is.null(mytree)) { 
    mytree <- read.tree(text = paste0("(t", Parent, ":",
                                      myT, ",t", PosTargets,
                                      ":", myT, ");")) 
    NodeData <- rbind(c(1, Parent), c(2, PosTargets))
  } 
  

  
  # keep track of this for confirmation
  myWorld[PosTargets, 4] <- Parent
  myWorld[PosTargets, 5] <- myT
  
  # define the trait value that the new society will exhibit
  if (!takeover) {
    # we assume that the baseline is to inherit whatever the parents did
    myWorld[PosTargets, 6] <- myWorld[i, 6]
    if (arisal) {
      #... but allow the possibility of developing new modes of subsistence de novo
      l.news <- length(PosTargets)
      prob.ar <- numeric()
      env.match <- myWorld[PosTargets, 7] == myWorld[PosTargets, 6]
      env.D <- myWorld[PosTargets, 7] == 1
      prob.ar[env.D & !env.match] <- P.Arisal[2, 2] # Prob of
      prob.ar[!env.D & !env.match] <- P.Arisal[1, 1] # Prob of
      origins <- runif(l.news) > prob.ar
      if(sum(origins) > 0) {
        myWorld[PosTargets[origins], 6] <- ifelse(myWorld[PosTargets[origins], 6] == 1, 2, 1)
      }
    }
  } 
  if (takeover) { # This is a Take Over event 
    # (the earlier soc has already been wiped out of the phylogeny
    # outside of this function and now all is left is to replace it
    # by a descendant of the parent)
    myWorld[PosTargets, 6] <- myWorld[i, 6]
  } 
  
  return(list("myWorld" = myWorld, "mytree" = mytree,
              "NodeData" = NodeData))
}