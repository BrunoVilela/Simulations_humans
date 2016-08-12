par(mfrow = c(3, 3))

# Origin of the Tree
size <- 5
start <- 3
test1 <- TheOriginOfSpecies(size, start)

# Speciation 1
parent <- 3
child <- 5
branch <- .5
test2 <- NewTip(test1, parent, child, branch)
plot(makePhy(test2))

# Speciation 2
parent <- 3
child <- 11
branch <- .5
test3 <- NewTip(test2, parent, child, branch)
plot(makePhy(test3))


# Speciation 2
parent <- 3
child <- 10
branch <- .7
test4 <- NewTip(test3, parent, child, branch)
plot(makePhy(test4))

# Speciation 2
parent <- 5
child <- 12
branch <- 5
test5 <- NewTip(test4, parent, child, branch)
plot(makePhy(test5))
tree <- makePhy(test5)


# Extinction
extinct <- 10
test6 <- DropTip(test5, extinct)
plot(makePhy(test6))

# Extinction2
extinct <- 5
test7 <- DropTip(test6, extinct)
plot(makePhy(test7))

# Extinction3
extinct <-12
test8 <- DropTip(test7, extinct)
plot(makePhy(test8))