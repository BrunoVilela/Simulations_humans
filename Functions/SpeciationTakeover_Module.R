# Speciation function!
SpeciationTakeOver <- function(input) {
  P.speciation <- input[[1]]
  P.Arisal <- input[[2]]
  P.diffusion <- input[[3]]
  P.extinction <- input[[4]]
  P.TakeOver <- input[[5]]
  myWorld <- input[[6]]
  mytree <- input[[7]]
  NodeData <- input[[8]]
  myT <- input[[9]]
  multiplier <- input[[10]]
  nbs <- input[[11]]
  
  
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
      myHex <- myWorld[i, 1]
      PosTargets <- getTargets(myHex, myWorld, nbs, empty = TRUE)
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
      if (emptyORtakeover & sum(P.diffusion) != 0) {
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
  output <- list(P.speciation, P.Arisal, P.diffusion, P.extinction, P.TakeOver,
                 myWorld, mytree, NodeData, myT, multiplier, nbs)
  return(output)
}




