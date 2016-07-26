# Function to get the lineages trough time by 1 step time.
# x = phy object

ltt.custom <- function(x) {
  branch.time2 <- branching.times(x)
  time.ltt2 <- seq(0, max(branch.time2), 1)
  n <- length(time.ltt2)
  acu.branch2 <- acu.branch.fora2 <- numeric(n)
  for(i in 1:n) {
    which.branch2 <- which(branch.time2 >= time.ltt2[i])
    acu.branch2[i] <-length(which.branch2)
  }
  time <- rank(-time.ltt2) - 1
  return(cbind(time, acu.branch2))
}


plot(ltt.custom(tree), type = "l") # Tree must be your own tree