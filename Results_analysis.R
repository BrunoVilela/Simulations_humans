# Required packages
library(geiger)
library(phytools)
library(caper)



# Load the results
myfiles <- list.files("cluster outputs", full.names = TRUE)

# Empty results
l.myfiles <- length(myfiles)
signal <- rep(NA, l.myfiles)

# Loop
for (i in 1:l.myfiles) {
  load(myfiles[i])
  rem <- is.na(myOut$myWorld[, 6])
  
  # Phylogenetic signal for binary traits (D of Fritz and Purvis 2010)
  if (length(length(unique(myOut$myWorld[!rem, 6]))) == 2) {
    traits <- data.frame("trait" = myOut$myWorld[!rem, 6], 
                         "tips" = paste0("t", myOut$myWorld[!rem, 8]))
    compdata <- comparative.data(myOut$mytree, traits, tips)
    signal[i] <- phylo.d(compdata, binvar = trait)$DEstimate
  }
  
}