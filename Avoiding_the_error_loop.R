P.extinction <- parameters(0.4, 0.4, 0.4, 0.4, "For", "Dom",
                           "For", "Dom") 
myWorld <- BuildWorld(R = 2, P = 0.5)

repetitions <- 10
y <- 0
x <- 0
results <- list(repetitions)
limit <- 200

while (y != repetitions & x < limit) {
  x <- x + 1
  check <- try(
    myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                    P.diffusion, P.Arisal, P.TakeOver, N.steps = 20,
                    multiplier = 1.3),
    silent = TRUE)
  if (class(check) != "try-error") {
    y <- y + 1
    myWorlds[[y]] <- myOut
  }
  names(check)
}
# Number of world extinctions:
x - y
