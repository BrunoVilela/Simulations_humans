# Make branch lengths uniform
uniformBranchs <- function(mytree, myT) {
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
  return(mytree)
}