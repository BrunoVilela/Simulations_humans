#==================================================================
JoinCount <- function(myWorld, nbs, repetitions = 100) {
  obs <- JoinCountObs(myWorld)
  rnd <- matrix(nrow = repetitions, ncol = 3)
  myWorld2 <- myWorld
  for (i in 1:repetitions) {
    myWorld2[, 6] <- sample(myWorld[, 6])
    rnd[i, ] <- JoinCountObs(myWorld2, nbs)
  }
  rnd.means <- colMeans(rnd)
  rnd.sd <- apply(rnd, 2, sd)
  z <- (obs - rnd.means) / rnd.sd
  names(z) <- c("DF", "FF", "DD")
  return(z)
}
#==================================================================
# Auxiliar function of join count
JoinCountObs <- function(myWorld, nbs) {
  neighs <- apply(myWorld[, 1], 1, getTargets, myWorld = myWorld, nbs,
                  empty = FALSE, traits = TRUE)
  l.neighs <- length(neighs)
  BB <- list()
  for (i in 1:l.neighs) {
    BB[[i]] <- paste0(neighs[[i]], myWorld[i, 6])
  }
  counts <- table(unlist(BB))
  counts <- c(counts[2] + counts[3], counts[1], counts[4])
  return(counts)
}
