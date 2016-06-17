# Takeover function

TakeOver <- function(myWorld, mytree, P.TakeOver, 
                     NodeData, myT, multiplier = 2) {
  
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
        source.trait <- myWorld[i, 6] == 1
        prob.to <- numeric(1)
        prob.to[source.trait] <- P.TakeOver[2, 1] # Prob of
        prob.to[!source.trait] <- P.TakeOver[1, 2] # Prob of
        # HERE MULTIPLE THE PROBABILITY BY THE ENV MATCH
        prob.to <- ifelse(myWorld[PosTargets, 6] == myWorld[PosTargets, 7],
                          prob.to/multiplier, prob.to)
        if (prob.to > runif(1)) {
          takeover <- TRUE
        } else {
          takeover <- FALSE
        }
        if (takeover) {
          temp <- sub.TakeOver(mytree, index.tips = PosTargets, 
                               myWorld, myT, i, NodeData)
          mytree <- temp$mytree
          myWorld <- temp$myWorld
          NodeData <- temp$NodeData
        }
      } 
    }
  }
  return(list("mytree" = mytree, "myWorld" = myWorld,
              "NodeData" = NodeData))  
}



#==================================================================
sub.TakeOver <- function(mytree, index.tips, myWorld, myT,
                         i, NodeData) { #index.tips = PosTargets
  # eliminate any record of the society that used to occupy the chosen spot
  temp <- extinct(mytree, index.tips, myWorld)
  mytree <- temp$mytree
  myWorld <- temp$myWorld
  NodeData <- temp$NodeData
  
  # and now occupy this spot with a descendant of the domest society
  Temp <- speciate(myT = myT, Parent = i, index.tips, 
                   myWorld = myWorld, mytree = mytree,
                   NodeData = NodeData, takeover = TRUE)
  return(Temp)
}
