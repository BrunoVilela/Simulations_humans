# Diffusion function
Diffusion <- function(myWorld, P.diffusion, multiplier = multiplier) {
  
  trait.nonNA <- !is.na(myWorld[, 6])
  trait.length <- sum(trait.nonNA)
  index.tips <- which(trait.nonNA)
  
  if (trait.length > 1) { # Only occurs if there is more than 1 societies
    index.tips <- sample(index.tips)
    for (i in index.tips) { 
      myHex <- myWorld[i, 1:3]
      PosTargets <- getTargets(myHex, myWorld, empty = FALSE)
      PosTargets <- PosTargets[myWorld[PosTargets, 6] != myWorld[i, 6]]
      l.targets <- length(PosTargets)
      if (l.targets > 0) {
        # PosTargets different from me
        if (l.targets > 1) {
          PosTargets <- sample(PosTargets, 1)
        }
        source.trait <- myWorld[i, 6] == 2
        prob.dif <- numeric(1)
        prob.dif[source.trait] <- P.diffusion[2, 1] # Prob of
        prob.dif[!source.trait] <- P.diffusion[1, 2] # Prob of
        # HERE MULTIPLE THE PROBABILITY BY THE ENV MATCH
        prob.dif <- ifelse(myWorld[PosTargets, 6] == myWorld[PosTargets, 7] &
                           myWorld[PosTargets, 6] == 2,
                           prob.dif/multiplier, prob.dif)
        if (prob.dif > runif(1)) {
          myWorld[PosTargets, 6] <- myWorld[i, 6]
        }
      }
    }
  }
  return(myWorld)  
}