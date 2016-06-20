# Takeover function
TakeOver <- function(myWorld, mytree, P.TakeOver, 
                     NodeData, myT, multiplier = 1.3,
                     i) {
  
  extinct.list <- NULL
  myHex <- myWorld[i, 1:3]
  PosTargets <- getTargets(myHex, myWorld, empty = FALSE)
  source.trait.dom <- myWorld[i, 6] == 2
  target.trait.dom <- myWorld[PosTargets, 6] == 2
  P.TakeOver2 <- P.TakeOver
  # If I am at the rigth place give me more chances!
  if (myWorld[i, 6] == myWorld[i, 7] & source.trait.dom) { 
    P.TakeOver2 <- P.TakeOver * multiplier
  }
  # How easy is to takeover
  prob.to <- numeric(1)
  prob.to[source.trait.dom & target.trait.dom] <- P.TakeOver2[2, 2] 
  prob.to[source.trait.dom & !target.trait.dom] <- P.TakeOver2[2, 1] 
  prob.to[!source.trait.dom & target.trait.dom] <- P.TakeOver2[1, 2] 
  prob.to[!source.trait.dom & !target.trait.dom] <- P.TakeOver2[1, 1] 
  match.env.targ <- myWorld[PosTargets, 6] != myWorld[PosTargets, 7]
  # If target is in the 
  Dom.in.For <- match.env.targ & source.trait.dom
  prob.to[Dom.in.For] <- prob.to[Dom.in.For] * multiplier

  # How good is the env for me
  good <- myWorld[i, 6] == myWorld[PosTargets, 7] & source.trait.dom
  
  if (any(good)) {
    PosTargets <- PosTargets[good]
    prob.to <- prob.to[good]
  }
  
  l.pos <- length(PosTargets)
  if (l.pos > 1) {
    choice <- sample(1:l.pos, 1)
    PosTargets <- PosTargets[choice]
    prob.to <- prob.to[choice]
  }

  if (prob.to > runif(1)) {
    extinct.list <- PosTargets
    temp <- sub.TakeOver(mytree, index.tips = PosTargets, 
                         myWorld, myT, i, NodeData)
    mytree <- temp$mytree
    myWorld <- temp$myWorld
    NodeData <- temp$NodeData
  }
  
return(list("mytree" = mytree, "myWorld" = myWorld,
            "NodeData" = NodeData, "extinct.list" = extinct.list))  
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
                   NodeData = NodeData)
  return(Temp)
}
