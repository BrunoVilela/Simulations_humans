# Diversification dependent
DivDep <- function(mytree, myWorld) {
  traits <- setNames(myWorld[, 6], myWorld[, 8])
  musse <- make.musse(mytree, traits, 2)
  p <- starting.point.musse(mytree, k = 2)
  fit.musse <- find.mle(musse, x.init = p[argnames(musse)])
  fit.musse$par
}
