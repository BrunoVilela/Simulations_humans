# The arisal of a new trait state
Arisal <-  <- function(input) {
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
  trait.nonNA <- !is.na(myWorld[, 6])
  trait.length <- sum(trait.nonNA)
  prob.ar <- numeric(trait.length)
  index.tips <- which(trait.nonNA)  
  env.match <- myWorld[trait.nonNA, 7] == myWorld[trait.nonNA, 6]
  env.D <- myWorld[trait.nonNA, 7] == 2
  prob.ar[env.D & !env.match] <- P.Arisal[2, 1] # Prob of
  prob.ar[!env.D & !env.match] <- P.Arisal[1, 2] # Prob of
  prob.ar[env.D & env.match] <- P.Arisal[2, 2] # Prob of
  prob.ar[!env.D & env.match] <- P.Arisal[1, 1] # Prob of
  arisal <- runif(trait.length) < prob.ar
  if (any(arisal)) {
    myWorld[index.tips[arisal], 6] <- ifelse(myWorld[index.tips[arisal], 6] == 1, 2, 1)
  }
  output <- list(P.speciation, P.Arisal, P.diffusion, P.extinction, P.TakeOver,
                 myWorld, mytree, NodeData, myT, multiplier)
  return(output)
}


