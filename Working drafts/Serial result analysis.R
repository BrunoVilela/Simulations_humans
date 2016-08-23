####
setwd("~/Box Sync/colliding ranges/Simulations_humans")

######Read in R libraries##############################
#source("Plot_output.R")
library(ape)
library(phytools)
library(picante)
library(apTreeshape)
library(caper)
library(geiger)
library(diversitree)
library(spdep)

######Read in R functions##############################
start_time <- Sys.time()

# Required packages and functions
load.files <- list.files(path = "Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}







###### Specify function ##############################

CalculateTreeUnits <- function(in_path, test_number) {
  
  setwd(in_path)
  getwd()
  # load my_tree and my_world
  load.files <- list.files(path = "Collapsed sim results/", pattern = ".Rdata",
                         full.names = TRUE)
	for (i in 1:length(load.files)) {
  		load(load.files[i])
	}


 ##### (0) Pull necessary variables from simulated trees and organize into a single object for all the tests below to pull from.
  
  #str(all_trees)
 
  
  if(test_number == 1){
  ## 0a) Branch lengths
  Branch_Lengths <- lapply(all_trees, function(x){x$edge.length})
  number_of_branches <- as.numeric(summary(Branch_Lengths)[,1])
  
  # Anchor test = PD (Faith's phylogenetic diversity) 
  Pylo_diversity_is_sum_of_BL <- sapply(Branch_Lengths, sum)
  
  # avPD -- Average phylogenetic diversity
  average_phylogenetic_diversity_is_mean_of_BL <- sapply(Branch_Lengths, mean)
  
  variance_Pylo_diversity_is_variance_of_BL <- sapply(Branch_Lengths, var)
 
  Branch_length_metrics <- cbind(number_of_branches, Pylo_diversity_is_sum_of_BL, average_phylogenetic_diversity_is_mean_of_BL, variance_Pylo_diversity_is_variance_of_BL)
  
  save(Branch_Lengths, file=paste0("Tree metrics from serial run/Raw_branch_lengths_for_", length(Branch_length_metrics[,1]), "_trees.Rdata"))
  
  save(Branch_length_metrics, file=paste0("Tree metrics from serial run/Branch_length_metrics_for_", length(Branch_length_metrics[,1]), "_trees.Rdata"))
  }
 
 
  if(test_number == 2){
  ## 0b) Pairwise distance between tips
  Pairwise_dist <- lapply(all_trees, cophenetic)  
  }
  
  
  if(test_number == 3){
  ## 0c) Phylogenetic isolation
  
  # Using equal.splits method, faster computation
  Evolutionary_distinctiveness <- lapply(all_trees, evol.distinct2, 
                                         type = "equal.splits") 
  }
  
  
  if(test_number == 4){
  ## 0d) tree topology
  
  all_trees_as_treeshape <- lapply(all_trees, as.treeshape)
  }

}


in_path <- "~/Box Sync/colliding ranges/Simulations_humans/results cluster output/"
test_number <- 1
CalculateTreeUnits(in_path, test_number)
