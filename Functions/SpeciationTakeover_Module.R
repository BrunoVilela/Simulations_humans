# Speciation function!
SpeciationTakeOver <- function(myWorld, mytree, P.speciation,
                               P.TakeOver, NodeData, myT, multiplier = multiplier) {
  
  trait.nonNA <- !is.na(myWorld[, 6])
  trait.length <- sum(trait.nonNA)
  index.tips <- which(trait.nonNA)
  extinct.list <- c()
  if (trait.length > 1) { # Only shuffles if there is more than 1 societies
    index.tips <- sample(index.tips)
  }
  
  # Probabililty of speciation
  
  for (i in index.tips) { 
    myT <- (1 / trait.length) + myT # Change time
    
    if (!i %in% extinct.list) {
      myHex <- myWorld[i, 1:3]
      PosTargets <- getTargets(myHex, myWorld, empty = TRUE)
      emptyORtakeover <- is.null(PosTargets)
      
      # If not null than Speciate going to an empty cell
      if (!emptyORtakeover) {
        temp <- Speciation(NodeData, myWorld, i, P.speciation,
                           myT, PosTargets, mytree)
        mytree <- temp$mytree
        myWorld <- temp$myWorld
        NodeData <- temp$NodeData
      }
      
      # If yes go to take over
      if (emptyORtakeover) {
        temp <- TakeOver(myWorld, mytree, P.TakeOver, 
                         NodeData, myT, multiplier = multiplier,
                         i)
        mytree <- temp$mytree
        myWorld <- temp$myWorld
        NodeData <- temp$NodeData
        extinct.list <- c(extinct.list, temp$extinct.list)
      }
    }
  }
  mytree <- uniformBranchs(mytree, myT)
  return(list("mytree" = mytree, "myWorld" = myWorld,
              "NodeData" = NodeData, "myT" = myT))  
}




