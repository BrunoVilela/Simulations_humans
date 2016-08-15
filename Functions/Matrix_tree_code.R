# The begging
TheOriginOfSpecies <- function(size, start) {
  edge <- matrix(ncol = 4, nrow = (2 * size) - 1)
  edge[1, ] <- c(1, 2, start, 1)
  return(edge)
}


# Speciation
NewTip <- function(edge, parent, child, branch) {
  pos <- which(is.na(edge[, 1]))[1]
  father <- which(parent == edge[, 3])
  m <- max(edge[, 2], na.rm = TRUE) + 1
  pos2 <- pos:(pos + 1)
  edge[pos2, 1] <- edge[father, 2]
  edge[pos2, 2] <- c(m, m + 1)
  edge[pos2, 3] <- c(parent, child)
  edge[father, 3] <- NA
  edge[pos2, 4] <- branch
  tips <- !is.na(edge[, 3])
  tips[pos2] <- FALSE
  edge[tips, 4] <- edge[tips, 4] + branch
  return(edge)
}

# Extinction
DropTip <- function(edge, extinct) {
  for (i in 1:length(extinct)) {
  target <- which(edge[, 3] == extinct[i])
  trans <- edge[target, 1]
  edge[target, ] <- NA
  target2 <- which(trans == edge[, 1])
  target3 <- which(trans == edge[, 2])
  edge[target3, 2] <- edge[target2, 2]
  edge[target3, 4] <- edge[target2, 4] + edge[target3, 4]
  edge[target3, 3] <- edge[target2, 3]
  edge[target2, ] <- NA
  }
  pos <- !is.na(edge[, 1])
  s <- sum(pos)
  edge[1:s, ] <- edge[pos, ]
  edge[(s + 1):nrow(edge), ] <- NA
  return(edge)
}

# Make it  a phylogeny
makePhy <- function(edge) {
  #target0 <- which(edge[, 1] == edge[1, 2])
  #edge[target0, 4] <- edge[target0, 4] + edge[1, 4]
  edge <- edge[-1, ]
  
  edge2 <- edge[!is.na(edge[, 1]), ]
  Edge <- edge2[, 1:2]
  Edge2 <- Edge
  tips <- !Edge[, 2] %in% Edge[, 1]
  Ntips <- sum(tips)
  Nnodes <- Ntips - 1
  Edge2[tips, 2] <- 1:Ntips
  nodes <- unique(Edge[, 1]) # including the root
  newnodes <- (Ntips + 1):(Ntips + Nnodes)
  for (i in 1:(Nnodes + 1)) {
    Edge2[Edge == nodes[i]] <- newnodes [i]
  }
  
  labels <- paste0("t", na.omit(edge2[, 3]))
  mytree <- list("edge" = Edge2, "tip.label" = labels, 
                 "edge.length" = edge2[, 4], "Nnode" =  Nnodes)
  attributes(mytree) <- list(names = c("edge", "tip.label", "edge.length",
                                       "Nnode"), 
                             class = "phylo")
  mytree <- reorder.phylo(mytree, order = "cladewise")
  return(mytree)
}
