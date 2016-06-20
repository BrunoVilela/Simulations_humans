# Extinction function
getExtinct <- function(myWorld, mytree, P.extinction, NodeData) {
  trait.nonNA <- !is.na(myWorld[, 6])
  trait.length <- sum(trait.nonNA)
  if (trait.length > 2) { # Only occurs if there is more than 5 societies
    prob.ext <- numeric(trait.length)
    index.tips <- which(trait.nonNA)  
    env.match <- myWorld[trait.nonNA, 7] == myWorld[trait.nonNA, 6]
    domesticator <- myWorld[trait.nonNA, 6] == 1
    prob.ext[env.match & domesticator] <- P.extinction[1, 1] # Prob of 
    prob.ext[env.match & !domesticator] <- P.extinction[2, 2] # Prob of
    prob.ext[!env.match & domesticator] <- P.extinction[1, 2] # Prob of
    prob.ext[!env.match & !domesticator] <- P.extinction[2, 1] # Prob of
    extinction <- runif(trait.length) < prob.ext
    survivors <- (trait.length - sum(extinction)) 
    if(survivors <= 1) {
      stop("One or less survivors, World extinction!!!")
    }
    if (any(extinction)) {
      temp <- extinct(mytree, index.tips[extinction], myWorld)
      mytree <- temp$mytree
      myWorld <- temp$myWorld
      NodeData <- temp$NodeData
    }
  }
  return(list("mytree" = mytree, "myWorld" = myWorld,
              "NodeData" = NodeData))  
}  

