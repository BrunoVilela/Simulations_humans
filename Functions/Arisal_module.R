# The arisal of a new trait state
Arisal <- function(myWorld, P.Arisal) {
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
  return(myWorld)  
}


