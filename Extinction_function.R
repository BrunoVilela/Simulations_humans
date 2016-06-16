# Extinction function
getExtinct <- function(myWorld, mytree, P.extinction, NodeData) {
  trait.nonNA <- !is.na(myWorld[, 6])
  trait.length <- sum(trait.nonNA)
  if (trait.length > 2) { # Only occurs if there is more than 2 societies
    prob.ext <- numeric(trait.length)
    index.tips <- which(trait.nonNA)  
    env.match <- myWorld[trait.nonNA, 7] == myWorld[trait.nonNA, 6]
    domesticator <- myWorld[trait.nonNA, 6] == 1
    prob.ext[env.match & domesticator] <- P.extinction[1, 1] # Prob of 
    prob.ext[env.match & !domesticator] <- P.extinction[2, 2] # Prob of
    prob.ext[!env.match & domesticator] <- P.extinction[1, 2] # Prob of
    prob.ext[!env.match & !domesticator] <- P.extinction[2, 1] # Prob of
    extinction <- runif(trait.length) > prob.ext
    if (any(extinction)) {
      mytree <- drop.tip(mytree, tip = myWorld[index.tips[extinction], 8])
      myWorld[index.tips[extinction], 4:6] <- NA
      # update NodeData
      tip.length <- length(mytree$tip.label)
      NodeData <- matrix(NA, tip.length, 2)
      colnames(NodeData) <- c('Node', 'Tip')
      NodeData[, 1] <- 1:tip.length
      NodeData[, 2] <- mytree$tip.label
    }
  }
  return(list("mytree" = mytree, "myWorld" = myWorld,
              "NodeData" = NodeData))  
}  

