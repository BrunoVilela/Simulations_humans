

#==================================================================
# Plot output (single tree)
myplot <- function(RunSim.Output) {
  par(oma = c(1, 1, 3, 1))
  mytree <- RunSim.Output[[1]]
  myWorld <- RunSim.Output[[3]]
  row.names(myWorld) <- paste0('t', myWorld[, 8])
  myWorld2 <- myWorld[, 6:7]
  colors <- list("Trait" = c('blue', 'red'), 
                 "Environment" = c('Black','Brown'))
  labels <- list("Trait" = c('Forager', 'Domesticator'),
                 "Environment" = c('Good4For', 'Good4Dom'))
  trait.plot(mytree, dat = as.data.frame(myWorld2), cols = colors,
             lab = labels, type = 'p', w = 1/70)
}



