
# BirthDeath function
bd <- function(tree) {
  tree$edge.length <- tree$edge.length / max(tree$edge.length) 
  x <- birthdeath(tree)  
  b <- x$para[2] / (1 - x$para[1])
  d <- b - x$para[2]
  c(setNames(c(b, d), c("b", "d")), x$para)
}

# Phylogenetic signal
D <- function(mytree, myWorld) {
  traits <- data.frame("trait" = myWorld[, 6], 
                       "tips" = myWorld[, 8])
  compdata <- comparative.data(mytree, traits, tips)
  # Phylogenetic signal for binary traits (D of Fritz and Purvis 2010)
  phylo.d(compdata, binvar = trait, permut = 1)$DEstimate
}

# Transition rates
transitions <- function(mytree, myWorld) {
  traits <- setNames(myWorld[, 6], myWorld[, 8])
  model <- fitDiscrete(mytree, traits[!is.na(traits)], model = "ARD")
  setNames(c(model$opt$q12, model$opt$q21), c("q12", "q21"))
}

# Diversification dependent
DivDep <- function(mytree, myWorld) {
  traits <- setNames(myWorld[, 6], myWorld[, 8])
  musse <- make.musse(mytree, traits, 2)
  p <- starting.point.musse(mytree, k = 2)
  fit.musse <- find.mle(musse, x.init = p[argnames(musse)])
  fit.musse$par
}
