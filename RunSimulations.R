# Github version
# Authors: Botero, Tuff & Vilela
# Clean everything
rm(list = ls())
#setwd("~/Box Sync/colliding ranges/Simulations_humans")
# Load the functions
source("SimulationFunctions.R")
source("Auxiliary_functions.R")
source("Extinction_function.R")
source("Difusion_function.R")
source("Takeover_function.R")
source("Speciate_function.R")
source("Speciation_function.R")
source("Uniform_branchs.R")
source("Plot_output.R")
source("Arisal_function.R")


#==================================================================
# Start function for cluster to call
#run_simulation <- function(i) {


#==================================================================
# Start with simple simulation 

# Only vertical transmission to adjacent neighbors, colonize an empty world)
P.speciation <- parameters(0.5, 0.5, 0.5, 0.5, "For", "Dom",
                           "For", "Dom") 
P.extinction <- parameters(0, 0.05, 0.05, 0, "For", "Dom",
                           "For", "Dom") 

# P of coming up with a novel subsistence mode
P.Arisal <-  parameters(0, 0, 0, 0.001, "For", "Dom",
                        "Env.For", "Env.Dom") 

#==================================================================
# Simple only vertical transmission

# P of diffusing your trait to a particular target
P.diffusion <- parameters(0, 0, 0, 0, "Target.Is.For", "Target.Is.Dom",
                          "Source.Is.For", "Source.Is.Dom")
                          
# P of taking over a neighbors position
P.TakeOver <- parameters(0, 0.1, 0.8, 0, "Target.Is.For", "Target.Is.Dom",
                         "Source.Is.For", "Source.Is.Dom")


#for (i in 1:Replicates) { #this is being replaced by the cluster function
  #print(paste("Replicate", i))
  myWorld <- BuildWorld(R = 3, P = 0.8)
  system.time (
  myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                  P.diffusion, P.Arisal, P.TakeOver, N.steps = 50)
  )
  plot(myOut$mytree)
  x <- myOut$myWorld[, 6]
  
