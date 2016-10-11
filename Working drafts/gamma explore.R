hist(as.numeric(as.character(results_table$gamma)), breaks=100, xlim=c(0,60))

library(ape)
library(phytools)
library(geiger)



g<-1
rep_number <- 1000
data_gamma <- rep(NA, rep_number)
for(g in 1:rep_number){
data_tree <- sim.bdtree(b=1, d=0.1, t=3000, n=1200)
data_gamma[g] <- ltt(data_tree, plot=FALSE)$gamma
print(g)
}
data_gamma
hist(data_gamma, breaks=100)


data_tree <- read.nexus(file="~/Desktop/x.tree")
data_gamma_1 <- ltt(data_tree, plot=FALSE)$gamma
abline(v=data_gamma_1, col="green")


