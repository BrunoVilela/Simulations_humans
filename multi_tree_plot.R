## plot the many replicate trees that are output by the simulation
setwd("~/Box Sync/colliding ranges/Simulations_humans")

library(gtools) 
library(ape) 
library(adephylo) 
library(diversitree)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/Arisal_module.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/Auxiliary_functions.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/Build_world_function.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/Complete_Model.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/Difusion_module.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/Extinction_module.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/Speciate_function.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/Speciation_function.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/SpeciationTakeover_Module.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/Takeover_function.R')


test_run <- function(replicate_cycle){



	P.speciation <- parameters(0.5, 0.5, 0.5, 0.5, "For", "Dom",
                           "For", "Dom") 
P.extinction <- parameters(0, 0, 0, 0, "For", "Dom",
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
  myWorld <- BuildWorld(R = 3, P = 0.8)
  system.time (
  myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                  P.diffusion, P.Arisal, P.TakeOver, N.steps = 50)
  )
  #)


setwd("~/Box Sync/colliding ranges/Simulations_humans/cluster outputs")
save(myOut, file = paste("myOut", replicate_cycle,"Results.Rdata"))
    }


for(i in 1:3){
test_run(i) 
}


library(parallel)
setwd("~/Box Sync/colliding ranges/Simulations_humans/Functions")

# Set up cluster
cl <- makeCluster(7, type = "PSOCK")

# Push resources out to cluster
clusterEvalQ( cl, library(gtools) )
clusterEvalQ( cl, library(ape) )
clusterEvalQ( cl, library(adephylo) )
clusterEvalQ( cl, library(diversitree) )
clusterEvalQ( cl, source("Arisal_module.R") )
clusterEvalQ( cl, source("Auxiliary_functions.R") )
clusterEvalQ( cl, source("Build_world_function.R") )
clusterEvalQ( cl, source("Complete_Model.R") )
clusterEvalQ( cl, source("Difusion_module.R") )
clusterEvalQ( cl, source("Extinction_module.R") )
clusterEvalQ( cl, source("Speciate_function.R") )
clusterEvalQ( cl, source("Speciation_function.R") )
clusterEvalQ( cl, source("Takeover_function.R") )
clusterEvalQ( cl, source("SpeciationTakeover_Module.R") )



# lset are the landscapes that we will run

        replicate_cycle <- c(1:30)
        clusterApplyLB(cl, x = replicate_cycle, fun = test_run) 

stopCluster(cl)












library(phylobase)

setwd("~/Box Sync/colliding ranges/Simulations_humans/cluster outputs/")
list.files()

a <- layout(matrix(c(1,2,3,4,5,6), 3,2, byrow=TRUE), width=c(1,1), height=c(1,1,1))
layout.show(a)


#treePlot(phylo4(myOut$mytree), show.tip.label=FALSE, show.node.label=FALSE, plot.data=FALSE, edge.color="white")
for(i in list.files()){
load(i)

tree <- myOut$mytree
x<-fastBM(tree)

xx<-contMap(tree,x, mar=c(4.1,1.1,1.1,0),res=200,plot=FALSE)
plot(xx,legend=FALSE,mar=c(4.1,1.1,1.1,0))


#treePlot(phylo4(myOut$mytree), newpage=FALSE, show.tip.label=FALSE, show.node.label=FALSE, edge.color=adjustcolor("cornflowerblue", alpha=.5), layout = layout.reingold.tilford)
}


library(phytools)
## first let's simulate some tree & data to work with
tree<-pbtree(n=40)
x<-fastBM(tree)
Now let's create our plot:
## create a split plot
layout(matrix(c(1,2),1,2),c(0.7,0.3))
## plot our tree
xx<-contMap(tree,x,mar=c(4.1,1.1,1.1,0),res=200,plot=FALSE)
plot(xx,legend=FALSE,mar=c(4.1,1.1,1.1,0))
## click to add legend interactively
add.color.bar(1,cols=xx$cols,lims=xx$lims,title="")
## add bar plot
par(mar=c(4.1,0,1.1,1.1))
barplot(x[tree$tip.label],horiz=TRUE,width=1,space=0,
  ylim=c(1,length(tree$tip.label))-0.5,names="")

