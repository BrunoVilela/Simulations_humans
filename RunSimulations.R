# Github version
# Authors: Botero, Tuff & Vilela
# Clean everything
rm(list = ls())

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

# Packages
library(phytools)

#==================================================================
# Start function for cluster to call
#run_simulation <- function(i) {


#==================================================================
# Start with simple simulation 

# Only vertical transmission to adjacent neighbors, colonize an empty world)
P.speciation <- parameters(0.5, 0.5, 0.5, 0.5, "For", "Dom",
                           "For", "Dom") 
P.extinction <- parameters(0, 0.5, 0.5, 0, "For", "Dom",
                           "For", "Dom") 

# P of coming up with a novel subsistence mode
P.Arisal <-  parameters(0, 0, 0, 0.001, "For", "Dom",
                        "Evolve.For", "Evolve.Dom") 

# Set up the number of replications (1 for general tests)
Replicates <- 1


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
  
  if (i == 1) {
    all.trees <- list(myOut$mytree)
    all.Worlds <- list(myOut$myWorld)
    all.node.Data <- list(myOut$NodeData)
  } else {
    all.trees[[i]] <- myOut$mytree
    all.Worlds[[i]] <- myOut$myWorld
    all.node.Data[[i]] <- myOut$NodeData
  }
  print (paste(i, 'out of', Replicates, 'simulations'))
}

VT.Sim <- list(all.trees, all.Worlds, all.node.Data)

#==================================================================
# Vertical transmission and takeover

# P of diffusing your trait to a particular target
P.diffusion <- parameters(0, 0, 0, 0, "Target.Is.For", "Target.Is.Dom",
                          "Source.Is.For", "Source.Is.Dom")
# P of taking over a neighbors position
P.TakeOver <- parameters(0, 0.4, 0.4, 0, "Target.Is.For", "Target.Is.Dom",
                         "Source.Is.For", "Source.Is.Dom")


for (i in 1:Replicates) {
  print(paste("Replicate", i))
  myWorld <- BuildWorld(R = 3, P = 0.5)
  myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                  P.diffusion, P.Arisal, P.TakeOver)
  if (i == 1) {
    all.trees <- list(myOut$mytree)
    all.Worlds <- list(myOut$myWorld)
    all.node.Data <- list(myOut$NodeData)
  }  else {
    all.trees[[i]] <- myOut$mytree
    all.Worlds[[i]] <- myOut$myWorld
    all.node.Data[[i]] <- myOut$NodeData
  }
  print (paste(i, 'out of', Replicates, 'simulations'))
}

TO.Sim <- list(all.trees, all.Worlds, all.node.Data)

#==================================================================
# Vertical transmission and takeover

# P of diffusing your trait to a particular target
P.diffusion <- parameters(0.5, 0, 0, 0.5, "Target.Is.For", "Target.Is.Dom",
                          "Source.Is.For", "Source.Is.Dom")

# P of taking over a neighbiors position
P.TakeOver <- parameters(0, 0.05, 0.4, 0, "Target.Is.For", "Target.Is.Dom",
                         "Source.Is.For", "Source.Is.Dom")

for (i in 1:Replicates) {
  print(paste("Replicate", i))
  myWorld <- BuildWorld(R = 3, P = 0.5)
  myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                  P.diffuVT.Sim[[1]][[1]], P.Arisal, P.TakeOver)
  
  if (i == 1) {
    all.trees <- list(myOut$mytree)
    all.Worlds <- list(myOut$myWorld)
    all.node.Data <- list(myOut$NodeData)
  }
  else {
    all.trees[[i]] <- myOut$mytree
    all.Worlds[[i]] <- myOut$myWorld
    all.node.Data[[i]] <- myOut$NodeData
  }
  print (paste(i, 'out of', Replicates, 'simulations'))
}

Dif.Sim <- list(all.trees, all.Worlds, all.node.Data)

save(VT.Sim, TO.Sim, Dif.Sim, 
     file = 'Results.Rdata')
     
#save_name <- paste("allresults 2_patch transplant_",namer,".Rdata", sep="")
#save(allresults,file= save_name)
## need to fix saving structure once we have a plan
    
     } #end function to be passed to cluster
     
#==================================================================
# cluster call


library(parallel)

# Set up cluster
cl <- makeCluster(6, type = "PSOCK")

# Push resources out to cluster
clusterEvalQ( cl, library(gtools) )
clusterEvalQ( cl, library(ape) )
clusterEvalQ( cl, library(adephylo) )
clusterEvalQ( cl, library(diversitree) )
clusterEvalQ( cl, source("SimulationFunctions.R") )
clusterEvalQ( cl, source("Auxiliary_functions.R") )
clusterEvalQ( cl, source("Extinction_function.R") )

# lset are the landscapes that we will run

        replicate_cycle <- c(1:10)
        clusterApplyLB(cl, replicate_cycle, run_simulation) 

stopCluster(cl)

#==================================================================
# plot function

######################
# plot a few examples
myplot(VT.Sim, i = 1)
myplot(VT.Sim, i = 2)
myplot(VT.Sim, i = 3)
myplot(TO.Sim, i = 1)
myplot(TO.Sim, i = 2)
myplot(TO.Sim, i = 3)
myplot(Dif.Sim, i = 1)
myplot(Dif.Sim, i = 2)
myplot(Dif.Sim, i = 3)
