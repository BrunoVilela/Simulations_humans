

# Adjust branch lengths
adjBranch <- function(x) { 
  x$edge.length <- x$edge.length / max(x$edge.length)
  return(x)
}