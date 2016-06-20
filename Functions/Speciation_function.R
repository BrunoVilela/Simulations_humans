# Speciation function used inside the speciation takeover
Speciation <- function(NodeData, myWorld, i, P.speciation,
                       myT, PosTargets, mytree) {
  Row.In.Node.Data <- which(NodeData[, 2] == i)
  env.match <- myWorld[i, 7] == myWorld[i, 6]
  domesticator <- myWorld[i, 6] == 2
  prob.sp <- numeric(1)
  prob.sp[env.match & domesticator] <- P.speciation[2, 2] # Prob of 
  prob.sp[env.match & !domesticator] <- P.speciation[1, 1] # Prob of
  prob.sp[!env.match & domesticator] <- P.speciation[2, 1] # Prob of
  prob.sp[!env.match & !domesticator] <- P.speciation[1, 2] # Prob of
  
  if (runif(1) > prob.sp) {
    # speciate (i.e., send diaspora to an adjacent empty cell)
    Temp <- speciate(myT = myT, Parent = i, PosTargets = PosTargets, 
                     myWorld = myWorld, mytree = mytree, NodeData = NodeData)
    
    myWorld <- Temp$myWorld
    mytree <- Temp$mytree
    NodeData <- Temp$NodeData
  }
  return(list("mytree" = mytree, "myWorld" = myWorld,
              "NodeData" = NodeData))
}