# Specitaion function
speciate <- function(myT, Parent, PosTargets, myWorld,
                     mytree, BL) {
  # Create descendant lineage
  if (length(PosTargets) > 1) {
    PosTargets <- sample(PosTargets, 1)
  }
  
  # Add a bifurcation to the node that used to be the parent
  if (!is.null(mytree)) {
    par.tip <- paste0("t", Parent)
    OldParentalNode <- which(mytree$tip.label == par.tip)
    mytree <- bind.tip(tree = mytree, tip.label = c(par.tip, paste0("t", PosTargets)), 
                       edge.length = BL, where = OldParentalNode)
  } else { 
    mytree <- read.tree(text = paste0("(t", Parent, ":",
                                      myT, ",t", PosTargets,
                                      ":", myT, ");")) 
  } 
  
  # keep track of this for confirmation
  myWorld[PosTargets, 4] <- Parent
  myWorld[PosTargets, 5] <- myT
  myWorld[PosTargets, 6] <- myWorld[Parent, 6]
  
  
  return(list("myWorld" = myWorld, "mytree" = mytree))
}