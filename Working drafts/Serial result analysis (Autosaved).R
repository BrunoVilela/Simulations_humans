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



in_path <- "~/Box Sync/colliding ranges/Simulations_humans/results cluster output/Collapsed sim results/"




###### Specify function ##############################

cluster_results_analysis <- function(in_path, test_number) {
  
  
  # load my_tree and my_world
  load.files <- list.files(path = in_path, pattern = ".Rdata",
                         full.names = TRUE)
	for (i in 1:length(load.files)) {
  		load(load.files[i])
	}







