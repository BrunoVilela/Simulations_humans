## plot the many replicate trees that are output by the simulation
setwd("~/Box Sync/colliding ranges/Simulations_humans")

library(gtools) 
library(ape) 
library(adephylo) 
library(diversitree)



test_run <- function(replicate_cycle){
	setwd("~/Box Sync/colliding ranges/Simulations_humans")
	 source("SimulationFunctions.R") 
source("Auxiliary_functions.R") 
source("Extinction_function.R") 
source("Difusion_function.R") 
source("Takeover_function.R") 
source("Speciate_function.R") 
source("Speciation_function.R") 
source("Uniform_branchs.R") 


	P.speciation <- parameters(0.5, 0.5, 0.5, 0.5, "For", "Dom",
                           "For", "Dom") 
P.extinction <- parameters(0, 0.5, 0.5, 0, "For", "Dom",
                           "For", "Dom") 

# P of coming up with a novel subsistence mode
P.Arisal <-  parameters(0, 0, 0, 0.001, "For", "Dom", "Evolve.For", "Evolve.Dom") 


#==================================================================
# Simple only vertical transmission

# P of diffusing your trait to a particular target
P.diffusion <- parameters(0, 0, 0, 0, "Target.Is.For", "Target.Is.Dom",
                          "Source.Is.For", "Source.Is.Dom")
                          
# P of taking over a neighbors position
P.TakeOver <- parameters(0, 0.1, 0.8, 0, "Target.Is.For", "Target.Is.Dom",
                         "Source.Is.For", "Source.Is.Dom")


	
	
	
	#setwd("~/Box Sync/colliding ranges/Simulations_humans")
myWorld <- BuildWorld(R = 3, P = 0.5)
  #system.time (
  myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                  P.diffusion, P.Arisal, P.TakeOver, N.steps = 20)
  #)


setwd("~/Box Sync/colliding ranges/Simulations_humans/cluster outputs")
save(myOut, file = paste("myOut", replicate_cycle,"Results.Rdata"))
    }


for(i in 1:3){
test_run(i) 
}


library(parallel)
setwd("~/Box Sync/colliding ranges/Simulations_humans")

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
clusterEvalQ( cl, source("Difusion_function.R") )
clusterEvalQ( cl, source("Takeover_function.R") )
clusterEvalQ( cl, source("Speciate_function.R") )
clusterEvalQ( cl, source("Speciation_function.R") )
clusterEvalQ( cl, source("Uniform_branchs.R") )

# lset are the landscapes that we will run

        replicate_cycle <- c(1:10)
        clusterApplyLB(cl, x = replicate_cycle, fun = test_run) 

stopCluster(cl)












library(phylobase)

setwd("~/Box Sync/colliding ranges/Simulations_humans/cluster outputs/")
list.files()

layout(matrix(c(1,2,3,4,5,6), 3,2))

#treePlot(phylo4(myOut$mytree), show.tip.label=FALSE, show.node.label=FALSE, plot.data=FALSE, edge.color="white")
for(i in list.files()){
load(i)
treePlot(phylo4(myOut$mytree), newpage=FALSE, show.tip.label=FALSE, show.node.label=FALSE, edge.color=adjustcolor("cornflowerblue", alpha=.5))
}









