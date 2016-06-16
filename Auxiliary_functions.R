# Functions to make the code cleaner and less repetitive


#==================================================================
# Function to make the parameters matrix automatic
parameters <- function(prob1, prob2, prob3, prob4, colname1, colname2, 
                       rowname1, rowname2) {
  result <- matrix(c(prob1, prob2, prob3, prob4), 2, 2, byrow = TRUE)
  colnames(result) <- c(colname1, colname2)
  rownames(result) <- c(rowname1, rowname2)
  return(result)
}
# Example
# parameters(0.05, 0, 0, 0.05, "test1", "test2", "test3", "test4")

#==================================================================
# Function to check if a cell is inside the world
is.inside <- function(x, y, response = "logical") {
  # x = values to be checked inside y
  # y = myworld coordinates in the same order as y
  # response can be logical or index numbers
  index.neigh <- apply(x, 1, paste, collapse = " ")
  index.world <- apply(na.omit(y), 1, paste, collapse = " ")
  if (response == "logical") {
  answer <- index.neigh %in% index.world
  }
  if (response == "index") {
    answer <- match(index.neigh, index.world)
  }
  return(answer)
}


#==================================================================
# Function to select targets for difusion and takeover
getTargets <- function(myHex, myWorld, empty) {
  # empty if TRUE will keep targets with no trait, 
  #       if false will keep only targets with traits

  AllTargets <- neighbors(myHex, inside = TRUE,
                          myWorld = myWorld)
  PosTargets <- NULL
  
  # Figure out which of the neighboring cells are good options for this context
  nAll <- nrow(AllTargets)
  PosTargets <- numeric(nAll)
  
  indexs <- is.inside(x = AllTargets, y = myWorld[, 1:3],
                      response = "index")
  
  if (empty) {
    PosTargets <- indexs[is.na(myWorld[indexs, 6])]
  } 
  if (!empty) {
    PosTargets <- indexs[!is.na(myWorld[indexs, 6])]
  }
  if (length(PosTargets) == 0) {
    PosTargets <- NULL
  }
  return(PosTargets)
}