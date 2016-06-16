# Functions to make the code cleaner and less repetitive


#==================================================================
# Function to make the parameters matrix automatic
parameters <- function(prob1, prob2, prob3, prob4, colname1, colname2, 
                       rowname1, rowname2) {
  result <- as.data.frame(matrix(c(prob1, prob2, prob3, prob4), 2, 2, byrow = TRUE))
  colnames(result) <- c(colname1, colname2)
  rownames(result) <- c(rowname1, rowname2)
  return(result)
}
# Example
# parameters(0.05, 0, 0, 0.05, "test1", "test2", "test3", "test4")

#==================================================================
# Function to compare coordinates
compare <- function(x, y) {
  return(
  sum(x[, 1] == y[1] &
      x[, 2] == y[2] &
      x[, 3] == y[3],
      na.rm = TRUE) == 0
  )
}