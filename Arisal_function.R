# The arisal of a new trait state
Arisal <- function(myWorld, mytree, P.extinction, NodeData) {
  trait.nonNA <- !is.na(myWorld[, 6])
  trait.length <- sum(trait.nonNA)
    prob.ext <- numeric(trait.length)
    index.tips <- which(trait.nonNA)  
    env.match <- myWorld[PosTargets, 7] == myWorld[PosTargets, 6]
    env.D <- myWorld[PosTargets, 7] == 1
    prob.ar[env.D & !env.match] <- P.Arisal[2, 2] # Prob of
    prob.ar[!env.D & !env.match] <- P.Arisal[1, 1] # Prob of
    arisal <- runif(trait.length) < prob.ar
    if (any(arisal)) {
      myWorld[arisal, 6] <- ifelse(myWorld[arisal, 6] == 1, 2, 1)
    }
  return(myWorld)  
}


