# Required packages and functions
load.files <- list.files(path = "Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}
source("Plot_output.R")
library(gtools)
library(ape)
library(adephylo)
library(diversitree)

# Load the results
myfiles <- list.files("cluster outputs", full.names = TRUE)

# Empty results
l.myfiles <- length(myfiles)
signal <- rep(NA, l.myfiles)
spatial <- matrix(ncol = 3, nrow = l.myfiles)
# Loop
for (i in 1:l.myfiles) {
  print(i)
  if (!is.na(myOut)) {
    load(myfiles[i])
    rem <- is.na(myOut$myWorld[, 6])
    myWorld <- myOut$myWorld[!rem, ]
    spatial[i, ] <- JoinCount(myWorld)
    
    # Phylogenetic signal for binary traits (D of Fritz and Purvis 2010)
    if (length(unique(myWorld[, 6])) == 2) {
      traits <- data.frame("trait" = myWorld[, 6], 
                           "tips" = paste0("t", myWorld[, 8]))
      compdata <- comparative.data(myOut$mytree, traits, tips)
      signal[i] <- phylo.d(compdata, binvar = trait)$DEstimate
    }
  }
  colnames(spatial) <- c("DF", "FF", "DD")
}


