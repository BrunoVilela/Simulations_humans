
library(ape)
library(phytools)
library(geiger)



g<-1
rep_number <- 60000
data_gamma <- rep(NA, rep_number)
for(g in 1:rep_number){
data_tree <- sim.bdtree(b=1, d=.1, n=1200, t=30000)
data_gamma[g] <- ltt(data_tree, plot=FALSE)$gamma
print(g)
}
data_gamma
hist(data_gamma, breaks=100)

save(data_gamma, file= "bd tees.Rdata")

setwd("~/Desktop/")
data_gamma_b4_d.1 <- data_gamma
data_gamma_b4_d3 <- data_gamma
data_gamma_b40_d1 <- data_gamma

str(data_tree)
objects()
str(myOut$mytree)
load('~/Box Sync/colliding ranges/Simulations_humans/first_63K_sim_results.Rdata')
objects()
head(results_table)


hist(as.numeric(as.character(results_table$gamma)), breaks=100, xlim=c(-15,60), col="grey")
hist(data_gamma_b4_d3, add=TRUE, breaks=100, xlim=c(-15,15), col="red")
hist(data_gamma_b4_d.1, add=TRUE, breaks=100, col="white")
hist(data_gamma_b40_d1, add=TRUE, breaks=100, col=adjustcolor("blue", 0.5))

data_tree <- read.nexus(file="~/Desktop/x.tree")
data_gamma_1 <- ltt(data_tree, plot=FALSE)$gamma
abline(v=data_gamma_1, col="green")





