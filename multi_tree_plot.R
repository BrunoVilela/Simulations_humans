## plot the many replicate trees that are output by the simulation


for(h in 1:10){
myWorld <- BuildWorld(R = 3, P = 0.5)
  system.time (
  myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                  P.diffusion, P.Arisal, P.TakeOver, N.steps = 20)
  )


setwd("~/Box Sync/colliding ranges/Simulations_humans/cluster outputs")
save(myOut, file = paste("myOut", h,"Results.Rdata"))
    }

library(phylobase)

setwd("~/Box Sync/colliding ranges/Simulations_humans/cluster outputs/")
list.files()


treePlot(phylo4(myOut$mytree), show.tip.label=FALSE, show.node.label=FALSE, plot.data=FALSE, edge.color="white")
for(i in list.files()){
load(i)
treePlot(phylo4(myOut$mytree), newpage=FALSE, show.tip.label=FALSE, show.node.label=FALSE, edge.color=adjustcolor("cornflowerblue", alpha=.5))
}









