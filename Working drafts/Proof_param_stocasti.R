# Gini
gini_index <- function(x) {
  l <- length(x)
  gs <- numeric(l)
  for(i in 1:l) {
    gs[i] <- sum(abs(x[i] - x[-i]))
  }
  G <- sum(gs) / (2 * l * sum(x))
  return(G)
}

# Model
estinmod <- function(n, resources, alfa = 1) {
  a <- numeric(n)
  prob0 <- rep(1/n, n)
  for(i in 1:resources) {
    if(sum(a) != 0) {
      prob <- (prob0 + ((a/sum(a)) * alfa) ) / (1 + alfa)
    } else {
      prob <- prob0
    }
    j <- sample(1:n, 1, prob = prob)
    a[j] <- a[j] + 1
  }
  hist(a, 30, xlab = "Money/Year", ylab = "People")
  return(gini_index(a))
}

# Vary resources
rep <- 100
pop <- 1000
seq.1 <- seq(5, 1000, 100)
l <- length(seq.1)
sds.2 <- matrix(nrow = rep, ncol = l)
for(j in 1:rep) {
  print(j)
  sds <- numeric(l)
  cont <- 0
  for(i in seq.1) {
    cont <- cont + 1
    sds[cont] <- estinmod(pop, i)
  }
  sds.2[j, ] <- sds
}
sd.sds.2 <- apply(sds.2, 2, sd, na.rm = T)
m <- colMeans(sds.2, na.rm = T)
plus <- m + sd.sds.2
minus <- m - sd.sds.2
plot(y = m, x = seq.1, type = "l", ylim = c(min(minus), max(plus)),
     ylab = "Gini", xlab = "Resources", lwd = 2)
lines(y = plus, x = seq.1, lty = 2, col = "gray20")
lines(y = minus, x = seq.1, lty = 2, col = "gray20")


# Vary population
rep <- 100
pop <- seq(10, 1000, 100)
seq.1 <- 500
l <- length(pop)
sds.2 <- matrix(nrow = rep, ncol = l)
for(j in 1:rep) {
  print(j)
  sds <- numeric(l)
  cont <- 0
  for(i in pop) {
    cont <- cont + 1
    sds[cont] <- estinmod(i, seq.1)
  }
  sds.2[j, ] <- sds
}
sd.sds.2 <- apply(sds.2, 2, sd, na.rm = T)
m <- colMeans(sds.2, na.rm = T)
plus <- m + sd.sds.2
minus <- m - sd.sds.2
plot(y = m, x = pop, type = "l", ylim = c(min(minus), max(plus)),
     ylab = "Gini", xlab = "Population", lwd = 2)
lines(y = plus, x = pop, lty = 2, col = "gray20")
lines(y = minus, x = pop, lty = 2, col = "gray20")



# Vary pop and resources but not rate
rep <- 100
pop <- seq(800, 10000, 100)
seq.1 <- pop * .10

l <- length(pop)
sds.2 <- matrix(nrow = rep, ncol = l)
for(j in 1:rep) {
  print(j)
  sds <- numeric(l)
  cont <- 0
  for(i in pop) {
    cont <- cont + 1
    sds[cont] <- estinmod(i, seq.1[cont])
  }
  sds.2[j, ] <- sds
}
sd.sds.2 <- apply(sds.2, 2, sd, na.rm = T)
m <- colMeans(sds.2, na.rm = T)
plus <- m + sd.sds.2
minus <- m - sd.sds.2
plot(y = m, x = pop, type = "l", ylim = c(min(minus), max(plus)),
     ylab = "Gini", xlab = "Population", lwd = 2)
lines(y = plus, x = pop, lty = 2, col = "gray20")
lines(y = minus, x = pop, lty = 2, col = "gray20")


