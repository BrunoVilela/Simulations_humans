
# Transition rates
transitions <- function(mytree, myWorld) {
  traits <- setNames(myWorld[, 6], myWorld[, 8])
  model <- fitDiscrete(mytree, traits[!is.na(traits)], model = "ARD")
  setNames(c(model$opt$q12, model$opt$q21), c("q12", "q21"))
}
