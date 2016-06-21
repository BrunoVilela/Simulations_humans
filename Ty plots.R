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


tree<-pbtree(n=20,scale=2)
x<-fastBM(tree)
phenogram(tree,x)
# or, simulate a discrete character history
tree<-sim.history(tree,Q=matrix(c(-1,1,1,-1),2,2),anc="1")
# simulate in which the rate depends on the state
x<-sim.rates(tree,c(1,10))
phenogram(tree,x)
# now use spread.labels
tree<-pbtree(n=40)
x<-fastBM(tree)
phenogram(tree,x,spread.labels=TRUE,spread.cost=c(1,0))

