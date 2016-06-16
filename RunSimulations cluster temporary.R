# Github version
# Authors: Botero, Tuff & Vilela
# Clean everything
rm(list = ls())

# Load the functions
source("SimulationFunctions.R")
source("Auxiliary_functions.R")

#==================================================================
#==================================================================
# Start with simple simulation 

run_simulation <- function(i){

# Only vertical transmission to adjacent neighbors, colonize an empty world)
P.speciation <- parameters(0.5, 0.5, 0.5, 0.5, "EnvF", "EnvD",
                           "For", "Dom") 
P.extinction <- parameters(0, 0.2, 0.4, 0, "EnvF", "EnvD",
                           "For", "Dom") 

# P of coming up with a novel subsistence mode
P.Arisal <-  parameters(0.05, 0, 0, 0.05, "EnvF", "EnvD",
                        "Evolve.For", "Evolve.Dom") 

# Set up the number of replications (1 for general tests)
Replicates <- 1


#==================================================================
# Simple only vertical transmission

# P of diffusing your trait to a particular target
P.diffusion <- parameters(0, 0, 0, 0, "Target.In.EnvF", "Target.In.EnvD",
                          "Source.Is.For", "Source.Is.Dom")
                          
# P of taking over a neighbiors position
P.TakeOver <- parameters(0, 0, 0, 0, "Source.In.AppHabitat", "Source.NOT.In.AppHabitat",
                         "Target.In.AppHabitat", "Target.NOT.In.AppHabitat")


#for (i in 1:Replicates) {

  print(paste("Replicate", i))
  myWorld <- BuildWorld(R = 3, P = 0.5)
  myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                  P.diffusion, P.Arisal, P.TakeOver)
  
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
P.diffusion <- parameters(0, 0, 0, 0, "Target.In.EnvF", "Target.In.EnvD",
                          "Source.Is.For", "Source.Is.Dom")
# P of taking over a neighbiors position
P.TakeOver <- parameters(0.4, 0, 0, 0.4, "Source.In.AppHabitat", "Source.NOT.In.AppHabitat",
                         "Target.In.AppHabitat", "Target.NOT.In.AppHabitat")


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
P.diffusion <- parameters(0.5, 0, 0, 0.5, "Target.In.EnvF", "Target.In.EnvD",
                          "Source.Is.For", "Source.Is.Dom")

# P of taking over a neighbiors position
P.TakeOver <- parameters(0.4, 0, 0.05, 0, "Source.In.AppHabitat", "Source.NOT.In.AppHabitat",
                         "Target.In.AppHabitat", "Target.NOT.In.AppHabitat")

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
save_name <- paste("allresults 2_patch transplant_",namer,".Rdata", sep="")
save(allresults,file= save_name)



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




#==================================================================
# cluster call
source("SimulationFunctions.R")
source("Auxiliary_functions.R")
library(gtools)
library(ape)
library(adephylo)
library(diversitree)



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


# lset are the landscapes that we will run

        replicate_cycle <- c(1:10)
        clusterApplyLB(cl, replicate_cycle, run_simulation) 

stopCluster(cl)


