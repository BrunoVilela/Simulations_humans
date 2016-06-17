# Speciation function!
Speciation <- function(myWorld, mytree, P.speciation,
                       P.Arisal, NodeData, myT) {
  
  trait.nonNA <- !is.na(myWorld[, 6])
  trait.length <- sum(trait.nonNA)
  index.tips <- which(trait.nonNA)
  if (trait.length > 1) { # Only shuffles if there is more than 1 societies
    index.tips <- sample(index.tips)
  }
  for (i in index.tips) { 
    myT <- (1/trait.length) + myT
    Row.In.Node.Data <- which(NodeData[, 2] == i)
    env.match <- myWorld[i, 7] == myWorld[i, 6]
    domesticator <- myWorld[i, 6] == 1
    prob.sp <- numeric(1)
    prob.sp[env.match & domesticator] <- P.speciation[2, 2] # Prob of 
    prob.sp[env.match & !domesticator] <- P.speciation[1, 1] # Prob of
    prob.sp[!env.match & domesticator] <- P.speciation[2, 1] # Prob of
    prob.sp[!env.match & !domesticator] <- P.speciation[1, 2] # Prob of
    
    if (runif(1) > prob.sp) {
      # speciate (i.e., send diaspora to an adjacent empty cell)
      myHex <- myWorld[i, 1:3]
      PosTargets <- getTargets(myHex, myWorld, empty = TRUE)
      if (!is.null(PosTargets)) {
        Temp <- speciate(myT = myT, Parent = i, PosTargets = PosTargets, 
                         myWorld = myWorld, mytree = mytree, NodeData = NodeData, 
                         takeover = FALSE)
        
        myWorld <- Temp$myWorld
        mytree <- Temp$mytree
        NodeData <- Temp$NodeData
      }
    }
  }
  mytree <- uniformBranchs(mytree, myT)
  return(list("mytree" = mytree, "myWorld" = myWorld,
              "NodeData" = NodeData, "myT" = myT))  
}




