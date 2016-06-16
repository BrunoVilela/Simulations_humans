# Diffusion function

Diffusion <- function(myWorld, P.diffusion) {
  
  trait.nonNA <- !is.na(myWorld[, 6])
  trait.length <- sum(trait.nonNA)
  index.tips <- which(trait.nonNA)  
  index.tips <- sample(index.tips)
  
  if (trait.length > 1) { # Only occurs if there is more than 1 societies
    
    for (i in index.tips) { 
      myHex <- myWorld[i, 1:3]
      PosTargets <- getTargets(myHex, myWorld, empty = FALSE)
      if(length(PosTargets) > 1) {
        # PosTargets different from me
        PosTargets <- PosTargets[myWorld[PosTargets, 6] != myWorld[i, PosTargets]]
        PosTargets <- sample(PosTargets, 1)
        source.trait <- myWorld[i, 6] == 1
        target.trait <- myWorld[PosTargets, 6] == 1
        prob.dif <- numeric(1)
        prob.dif[source.trait & target.trait] <- P.diffusion[2, 2] # Prob of 
        prob.dif[source.trait & !target.trait] <- P.diffusion[2, 1] # Prob of
        prob.dif[!source.trait & target.trait] <- P.diffusion[1, 2] # Prob of
        prob.dif[!source.trait & !target.trait] <- P.diffusion[1, 1] # Prob of
        if (prob.dif > runif(1)) {
          myWorld[PosTargets, 6] <- myWorld[i, 6]
        }
      }
    }
  }
  return(myWorld)  
}