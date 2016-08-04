# Speciation function!
SpeciationTakeOver <- function(input) {
  P.speciation <- input[[1]]
  P.Arisal <- input[[2]]
  P.diffusion <- input[[3]]
  P.extinction <- input[[4]]
  P.TakeOver <- input[[5]]
  myWorld <- input[[6]]
  mytree <- input[[7]]
  myT <- input[[8]]
  multiplier <- input[[9]]
  nbs <- input[[10]]
  independent <- input[[11]]
  
  trait.nonNA <- !is.na(myWorld[, 6])
  trait.length <- sum(trait.nonNA)
  index.tips <- which(trait.nonNA)
  extinct.list <- c()
  if (trait.length > 1) { # Only shuffles if there is more than 1 societies
    index.tips <- sample(index.tips)
  }
  
  # Probabililty of speciation
  BL <- (1 / trait.length)
  for (i in index.tips) { 
    myT <-  BL + myT # Change time
    
    if (!i %in% extinct.list) {
      myHex <- myWorld[i, 1]
      PosTargets <- getTargets(myHex, myWorld, nbs, empty = TRUE)
      emptyORtakeover <- is.null(PosTargets)
      
      # If not null than Speciate going to an empty cell
      if (!emptyORtakeover) {
        temp <- Speciation(myWorld, i, P.speciation,
                           myT, PosTargets, mytree, BL)
        mytree <- temp$mytree
        myWorld <- temp$myWorld
      }
      
      # If yes go to take over
      if (emptyORtakeover & sum(P.TakeOver) != 0) {
        temp <- TakeOver(myWorld, mytree, P.TakeOver, 
                         myT, multiplier = multiplier,
                         i, BL, independent)
        mytree <- temp$mytree
        myWorld <- temp$myWorld
        extinct.list <- c(extinct.list, temp$extinct.list)
      }
    }
  }
  #mytree <- uniformBranchs(mytree, myT)
  if (!is.null(mytree)) {
 # mytree2 = mytree  
  n <- length(mytree$tip.label)
  tips <- sapply(1:n, function(x, y) which(y == x), y = mytree$edge[,2])
  sub <- 1 - abs(round(floor(signif(mytree$edge.length[tips])) - (mytree$edge.length[tips]), digits = 5))
  mytree$edge.length[tips] <- round(mytree$edge.length[tips] + sub, digits = 5)
#  plot(mytree)
  }
  output <- list(P.speciation, P.Arisal, P.diffusion, P.extinction, P.TakeOver,
                 myWorld, mytree, myT, multiplier, nbs, independent)
  return(output)
}




