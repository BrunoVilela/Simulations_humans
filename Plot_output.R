

#==================================================================
# Plot output (single tree)
myplot <- function(RunSim.Output, i) {
  par(oma = c(1, 1, 3, 1))
  mytree <- RunSim.Output[[1]][[i]]
  myWorld <- RunSim.Output[[2]][[1]]
  row.names(myWorld) <- myWorld[, 8]
  myWorld <- myWorld[, c("Trait", "Environment")]
  colors <- list("Trait" = c('blue', 'red'), 
                 "Environment" = c('Black','Brown'))
  labels <- list("Trait" = c('Forager', 'Domesticator'),
                 "Environment" = c('Good4For', 'Good4Dom'))
  trait.plot(mytree, dat = myWorld, cols = colors,
             lab = labels, type = 'p', w = 1/70)
  title(paste(deparse(substitute(RunSim.Output)), i), outer = TRUE)
}
