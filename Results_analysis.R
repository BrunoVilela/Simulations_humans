# Code to evaluate the outputs

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
split.file.name <- strsplit(myfiles, split = "_") 
positions <- c(3, 7, 10:13, 15:18, 20:23, 25:28, 30:33)
data.result <- data.frame(matrix(ncol = 23, nrow = length(myfiles)))
colnames(data.result) <- c("available_files", "replicate", "combo",
                           "speciation_1", "speciation_2", "speciation_3", "speciation_4",
                           "extinction_1", "extinction_2", "extinction_3", "extinction_4",
                           "diffusion_1", "diffusion_2", "diffusion_3", "diffusion_4",
                           "takeover_1", "takeover_2", "takeover_3", "takeover_4",
                           "arisal_1", "arisal_2", "arisal_3", "arisal_4")
data.result[, 1] <- myfiles
for (i in 1:length(positions)) {
  data.result[, i + 1] <- sapply(split.file.name, "[", positions[i])
}

# Empty results
l.myfiles <- length(myfiles)
signal <- rep(NA, l.myfiles)
spatial <- matrix(ncol = 3, nrow = l.myfiles)
colnames(spatial) <- c("DF", "FF", "DD")

# Loop
for (i in 1:l.myfiles) {
  print(i)
  if (!is.na(myOut)) {
    load(myfiles[i])
    rem <- is.na(myOut$myWorld[, 6])
    myWorld <- myOut$myWorld[!rem, ]
    spatial[i, ] <- JoinCount(myWorld, repetitions = 100)
    
    # Phylogenetic signal for binary traits (D of Fritz and Purvis 2010)
    if (length(unique(myWorld[, 6])) == 2) {
      traits <- data.frame("trait" = myWorld[, 6], 
                           "tips" = paste0("t", myWorld[, 8]))
      compdata <- comparative.data(myOut$mytree, traits, tips)
      signal[i] <- phylo.d(compdata, binvar = trait)$DEstimate
    }
  }
}


data.result$Phy_Signal <- signal
data.result <- cbind(data.result, spatial)