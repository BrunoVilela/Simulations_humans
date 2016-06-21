setwd("~/Box Sync/colliding ranges/Simulations_humans/cluster outputs")

available_files <- list.files()
b <- c(NULL, NULL, NULL)

split_file_name <- strsplit(available_files, split="_") 
replicate <- sapply(split_file_name, "[", 3)
combo <- sapply(split_file_name, "[", 7)
b <- cbind(available_files, replicate, combo)



par(mfrow=c(1,5))


# tree plot

library(diversitree)

load(b[5,1])
myplot(myOut)



## Medusa diversification breaks

for(j in 2){
load(b[j,1])
tree <- myOut[[1]]
    
    ## USING AICc as STOPPING CRITERION
    res1=medusa(tree,  warnings=FALSE)
    print(names(res1)) # output list elements
    print(res1$summary) # show 'summary' object
    summary(res1, criterion="aicc") # select best model based on AICc
    
    ## PLOTTING RESULTS
    # plot breakpoints for the best model chosen by AICc
    # invoking plot.medusa()
    plot(res1, cex=0.5,label.offset=1, edge.width=2) 
    }






#branch length
plot(0,0,xlim=c(-20,80), ylim=c(0,.5), type="n", xlab="Branch length", ylab="Frequency")
for( j in 1:length(b[,1])){
load(b[j,1])

w <- density(myOut[[1]]$edge.length)
lines(w,col=adjustcolor("cornflowerblue", alpha=.4))
}


#number of nodes
nodes_list <- NULL
plot(0,0,xlim=c(0.5,1.5), ylim=c(0,40), type="n", xlab="", ylab="Number of nodes", xaxt="n")
for( j in 1:length(b[,1])){
load(b[j,1])
nodes_list <- c(  nodes_list , myOut[[1]]$Nnode)

}

boxplot( nodes_list,border=adjustcolor("cornflowerblue", alpha=1), pch=19, add=TRUE, at=1)



## Balance factor
library(TotalCopheneticIndex)
tci_list <- NULL
plot(0,0,xlim=c(0.5,1.5), ylim=c(0,1500), type="n", xlab="", ylab="Number of nodes", xaxt="n")
for( j in 1:length(b[,1])){
load(b[j,1])
tci_list <- c(  tci_list , tci(myOut[[1]]))

}

boxplot( tci_list,border=adjustcolor("cornflowerblue", alpha=1), pch=19, add=TRUE, at=1)




dat=get(data(whales))
    phy=dat$phy
    richness=dat$richness
    
    ## USING AICc as STOPPING CRITERION
    res1=medusa(phy, richness, warnings=FALSE)
    print(names(res1)) # output list elements
    print(res1$summary) # show 'summary' object
    summary(res1, criterion="aicc") # select best model based on AICc
    
    ## PLOTTING RESULTS
    # plot breakpoints for the best model chosen by AICc
    # invoking plot.medusa()
    plot(res1, cex=0.5,label.offset=1, edge.width=2) 
combo_number <- 31
replicate_cycle <- 3
paste0("cluster outputs/myOut_replicate_", formatC(replicate_cycle, width = 2,flag = 0), 
                            "_function_combination_type_", formatC(combo_number, width = 2,flag = 0), "_",
                            "parameters_", "P.speciation_" , paste(P.speciation, collapse="_"), "P.extinction_",paste(P.extinction, collapse="_"), "P.diffusion",paste(P.diffusion, collapse="_"), "P.TakeOver_",paste(P.TakeOver, collapse="_"), "P.Arisal", paste(P.Arisal, collapse="_"),
                            as.integer(Sys.time()), " Results.Rdata")


