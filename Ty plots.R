setwd("~/Box Sync/colliding ranges/Simulations_humans/cluster outputs")

available_files <- list.files()
b <- c(NULL, NULL, NULL)

split_file_name <- strsplit(available_files, split="_") 
replicate <- sapply(split_file_name, "[", 3)
combo <- sapply(split_file_name, "[", 7)
b <- cbind(available_files, replicate, combo)







#branch length
plot(0,0,xlim=c(-20,80), ylim=c(0,.5), type="n", xlab="Branch length", ylab="Frequency")
for( j in 1:length(b[,1])){
load(b[j,1])

w <- density(myOut[[1]]$edge.length)
lines(w,col=adjustcolor("cornflowerblue", alpha=.4))
}


#number of nodes
nodes_list <- NULL
plot(0,0,xlim=c(0.5,1.5), ylim=c(0,60), type="n", xlab="", ylab="Number of nodes", xaxt="n")
for( j in 1:length(b[,1])){
load(b[j,1])
nodes_list <- c(  nodes_list , myOut[[1]]$Nnode)

}

boxplot( nodes_list,border=adjustcolor("cornflowerblue", alpha=1), pch=19, add=TRUE, at=1)



## Balance factor




# tree plot


library(phytools)

tree<-myOut[[1]]
x<-fastBM(myOut[[1]])

phenogram(myOut[[1]],x,add=FALSE, colors=adjustcolor("white", alpha=0), ylim = c(-20,20))

i <- 5
for( i in 1:length(b[,1])){
	load(b[i,1])
tree<-myOut[[1]]
x<-fastBM(myOut[[1]])


phenogram(myOut[[1]],x,add=TRUE, colors=adjustcolor("cornflowerblue", alpha=.1),spread.cost=c(1,0))
}
polygon(x = c(50,50,70,70), y=c(-20,20,20,-20), col="white", border="white")




tree<-myOut[[1]]
x<-fastBM(myOut[[1]])
plotTree(tree)

nodelabels(node = as.numeric(rownames(fitER$marginal.anc)), pie = fitER$marginal.anc, 
    piecol = c("blue", "red", "yellow"), cex = 0.6)
tiplabels(pie = to.matrix(x, sort(unique(x))), piecol = c("blue", "red", "yellow"), 
    cex = 0.3)

library(diversitree)
myplot(myOut)



rem <- is.na(myOut$myWorld[, 6])
trait <- myOut$myWorld[!rem, 6]
names(trait) <- paste0("t", myOut$myWorld[!rem, 8])
phenogram(myOut$mytree, trait, ylab = "Domestication", spread.cost=c(1,0))






