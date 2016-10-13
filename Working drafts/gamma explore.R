
library(ape)
library(phytools)
library(geiger)



g<-1
rep_number <- 60000
data_gamma <- rep(NA, rep_number)
for(g in 1:rep_number) {
data_tree <- pbtree(b=1, d=.1, n=1200, scale=1, extant.only = TRUE)
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



j[[1]][,11]


   path <- "~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/BD_null_trees"
      
      setwd(path)
    myfiles_full <- list.dirs()
    analyze_this_many <- length(myfiles_full)
    
    available_files <- matrix(NA, 1, 1)
    
        
    for(i in 1: analyze_this_many){
    available_files <- rbind(available_files , as.matrix(list.files(myfiles_full[i], full.names = TRUE)))
    }
    dim(available_files)

available <- list.files()
files <- matrix(rep(NA, 16), length(available), 16)
dim(files)
i <- 10
dim(j$results_summary_of_single_value_outputs)
j$results_summary_of_single_value_outputs
for(i in 1:length(available)){
load(available[i])

files[i,] <- j$results_summary_of_single_value_outputs

}

head(files)

hist(files[,11], breaks=100, xlim=c(-15,15))

par(mfrow=c(1,2))
data_tree_pre <- read.nexus(file="~/Desktop/x.tree")
plot(data_tree_pre)
axis(1)
data_tree <- chronopl(data_tree_pre, 0)
plot(data_tree)
axis(1)
data_gamma_1 <- ltt(data_tree, plot=FALSE)$gamma
abline(v=data_gamma_1, col="green")



our_sim_tree_pre <- myOut$mytree
our_sim_tree <-  our_sim_tree_pre
our_sim_tree_pre$edge.length <- our_sim_tree_pre$edge.length / 30000
ltt(our_sim_tree, gamma = TRUE)
ltt(our_sim_tree_pre, gamma = TRUE)

ltt(data_tree_pre, gamma = TRUE)
plot(data_tree_pre)
plot(our_sim_tree)