rm(list = ls())
source("SimulationFunctions.R")

################################################################################################
################################################################################################
# Start with simple simulation 
# only vertical transmission to adjacent neighbors, colonize an empty world)

P.speciation <- as.data.frame(matrix(c(0.5, 0.5, 0.5, 0.5), 2, 2, byrow = TRUE))
P.extinction <- as.data.frame(matrix(c(0, 0.2, 0.4, 0), 2, 2, byrow = TRUE))
P.Arisal <- as.data.frame(matrix(c(0.05, 0, 0, 0.05), 2, 2, byrow = TRUE)) # P of coming up with a novel subsistence mode
names(P.extinction) <- names(P.speciation) <- names(P.Arisal) <- c("EnvF", "EnvD") 
row.names(P.extinction) <- row.names(P.speciation) <- c("For", "Dom")
row.names(P.Arisal) <- c("Evolve.For", "Evolve.Dom")

Replicates <- 1


#####################################################
# simple only vertical transmission
P.diffusion <- as.data.frame(matrix(c(0, 0, 0, 0), 2, 2, byrow = TRUE)) # P of diffusing your trait to a particular target
names(P.diffusion) <- c("Target.In.EnvF", "Target.In.EnvD") # where the target lives
row.names(P.diffusion) <- c("Source.Is.For", "Source.Is.Dom") # What the source society does

P.TakeOver <- as.data.frame(matrix(c(0, 0, 0, 0), 2, 2, byrow = TRUE)) # P of taking over a neighbiors position
names(P.TakeOver) <- c("Source.In.AppHabitat", "Source.NOT.In.AppHabitat") # How appropriate is the SOURCE's habitat to its strategy? lives
row.names(P.TakeOver) <- c("Target.In.AppHabitat", "Target.NOT.In.AppHabitat") # How appropriate is the TARGET society's habitat

for (i in 1:Replicates) {
  print(paste("Replicate", i))
  myWorld <- BuildWorld(R = 3, P = 0.5)
  myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                  P.diffusion, P.Arisal, P.TakeOver)
  
  if (i == 1) {
    all.trees <- list(myOut$mytree)
    all.Worlds <- list(myOut$myWorld)
    all.node.Data <- list(myOut$node.Data)
  } else {
    all.trees[[i]] <- myOut$mytree
    all.Worlds[[i]] <- myOut$myWorld
    all.node.Data[[i]] <- myOut$node.Data
  }
  print (paste(i, 'out of', Replicates, 'simulations'))
}

VT.Sim <- list(all.trees, all.Worlds, all.node.Data)

#####################################################
# vertical transmission and takeover
P.diffusion <- as.data.frame(matrix(c(0, 0, 0, 0), 2, 2, byrow = TRUE)) # P of diffusing your trait to a particular target
names(P.diffusion) <- c("Target.In.EnvF", "Target.In.EnvD") # where the target lives
row.names(P.diffusion) <- c("Source.Is.For", "Source.Is.Dom") # What the source society does

P.TakeOver <- as.data.frame(matrix(c(0.4, 0, 0, 0.4), 2, 2, byrow = TRUE)) # P of taking over a neighbiors position
names(P.TakeOver) <- c("Source.In.AppHabitat", "Source.NOT.In.AppHabitat") # How appropriate is the SOURCE's habitat to its strategy? lives
row.names(P.TakeOver) <- c("Target.In.AppHabitat", "Target.NOT.In.AppHabitat") # How appropriate is the TARGET society's habitat


for (i in 1:Replicates) {
  print(paste("Replicate", i))
  myWorld <- BuildWorld(R = 3, P = 0.5)
  myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                  P.diffusion, P.Arisal, P.TakeOver)
  if (i == 1) {
    all.trees <- list(myOut$mytree)
    all.Worlds <- list(myOut$myWorld)
    all.node.Data <- list(myOut$node.Data)
  }  else {
    all.trees[[i]] <- myOut$mytree
    all.Worlds[[i]] <- myOut$myWorld
    all.node.Data[[i]] <- myOut$node.Data
  }
  print (paste(i, 'out of', Replicates, 'simulations'))
}

TO.Sim <- list(all.trees, all.Worlds, all.node.Data)

#####################################################
# vertical transmission and takeover
P.diffusion <- as.data.frame(matrix(c(0.5, 0, 0, 0.5), 2, 2, byrow = T)) # P of diffusing your trait to a particular target
names(P.diffusion) <- c("Target.In.EnvF", "Target.In.EnvD") # where the target lives
row.names(P.diffusion) <- c("Source.Is.For", "Source.Is.Dom") # What the source society does

P.TakeOver <- as.data.frame(matrix(c(0.4, 0, 0.05, 0), 2, 2, byrow = T)) # P of taking over a neighbiors position
names(P.TakeOver) <- c("Source.In.AppHabitat", "Source.NOT.In.AppHabitat") # How appropriate is the SOURCE's habitat to its strategy? lives
row.names(P.TakeOver) <- c("Target.In.AppHabitat", "Target.NOT.In.AppHabitat") # How appropriate is the TARGET society's habitat

for (i in 1:Replicates) {
  print(paste("Replicate", i))
  myWorld <- BuildWorld(R = 3, P = 0.5)
  myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                  P.diffuVT.Sim[[1]][[1]], P.Arisal, P.TakeOver)
  
  if (i == 1) {
    all.trees <- list(myOut$mytree)
    all.Worlds <- list(myOut$myWorld)
    all.node.Data <- list(myOut$node.Data)
  }
  else {
    all.trees[[i]] <- myOut$mytree
    all.Worlds[[i]] <- myOut$myWorld
    all.node.Data[[i]] <- myOut$node.Data
  }
  print (paste(i, 'out of', Replicates, 'simulations'))
}

Dif.Sim <- list(all.trees, all.Worlds, all.node.Data)

save(VT.Sim, TO.Sim, Dif.Sim, file = '/Users/CarlosMacPro/Dropbox/Projects/CulturalEvoSimulations/Results.Rdata')

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
