





##################################################################################
input_parameters_plot <- function(sea, sea.T, sea.D, sea.DT){



names(sea)
files.available <- c(sea[[2]], sea.D[[2]], sea.T[[2]], sea.DT[[2]] )

##### parse file names to retrieve simulation parameter info ########################
  split.file.name <- strsplit(files.available, split = "_")   #split file name everywhere there is and underscore
  
  
  positions <- c(3, 5, 8:11, 13:16, 18:21, 23:26, 28, 30) # 
  data.result <- data.frame(matrix(ncol = 21, nrow = length(files.available)))
  colnames(data.result) <- c("File_path", "replicate", "combo",
                             "speciation_1", "speciation_2", "speciation_3", "speciation_4",
                             "extinction_1", "extinction_2", "extinction_3", "extinction_4",
                             "diffusion_1", "diffusion_2", "diffusion_3", "diffusion_4",
                             "takeover_1", "takeover_2", "takeover_3", "takeover_4",
                             "arisal_1", "Timesteps")
  data.result[, 1] <- files.available
  #head(data.result)
  
  data.result.blank <- data.result # pass matrix to new object to be used seperatly below
  
  for (i in 1:length(positions)) {
    data.result[, i + 1] <- sapply(split.file.name, "[", positions[i])
  }
head(data.result)



data.result.S1 <- as.numeric(data.result[ , 4])
data.result.S2 <- as.numeric(data.result[ , 5])
data.result.S3 <- as.numeric(data.result[ , 6])
data.result.S4 <- as.numeric(data.result[ , 7])
data.result.E1 <- as.numeric(data.result[ , 8])
data.result.E2 <- as.numeric(data.result[ , 9])
data.result.E3 <- as.numeric(data.result[ , 10])
data.result.E4 <- as.numeric(data.result[ , 11])
data.result.S1 <- as.numeric(data.result[ , 12])
data.result.S2 <- as.numeric(data.result[ , 13])
data.result.S3 <- as.numeric(data.result[ , 14])
data.result.S4 <- as.numeric(data.result[ , 15])
data.result.S1 <- as.numeric(data.result[ , 16])
data.result.S2 <- as.numeric(data.result[ , 17])
data.result.S3 <- as.numeric(data.result[ , 18])
data.result.S4 <- as.numeric(data.result[ , 19])
data.result.A <- as.numeric(data.result[ , 20])


input_parameters <- list(data.result.S1 ,
data.result.S2 ,
data.result.S3 ,
data.result.S4 ,
data.result.E1 ,
data.result.E2 ,
data.result.E3 ,
data.result.E4 ,
data.result.S1 ,
data.result.S2 ,
data.result.S3 ,
data.result.S4 ,
data.result.S1 ,
data.result.S2 ,
data.result.S3 ,
data.result.S4 ,
data.result.A)


par(mar=c(2,0,0,0))
blankplot(c(0,1), c(0,10))

summary(data.result_sub)


	line_weight <- 3

colors <- adjustcolor(c("red", "blue", "green", "orange", "purple"), alpha=.6)

j <- 1
for(h in 1:length(input_parameters)){
abline(v=mean(na.omit(as.numeric(input_parameters[[h]]))), col = colors[j], lty=2, lwd=2)
j <- j+1

}



colors <- adjustcolor(c("red", "blue", "green", "orange", "purple"), alpha=.1)
dens <- density(na.omit(as.numeric(data.result_sub[,1])))
polygon(dens, col = colors[1], border="darkgrey")

dens <- density(na.omit(as.numeric(data.result_sub[,2])))
polygon(dens, col = colors[2], border="darkgrey")

dens <- density(na.omit(as.numeric(data.result_sub[,3])))
polygon(dens, col = colors[3], border="darkgrey")

dens <- density(na.omit(as.numeric(data.result_sub[,4])))
polygon(dens, col = colors[4], border="darkgrey")

dens <- density(na.omit(as.numeric(data.result_sub[,5])))
polygon(dens, col = colors[5], border="darkgrey")



legend(0.7, 9.5, c("Speciation rate", "Extinction rate","Diffusion rate", "Takeover rate", "Arisal rate", "mean per distribution"),col= adjustcolor(c("red", "blue", "green", "orange", "purple", "black"), alpha=.9), lwd=c(3,3,3,3,3,1.5), lty=c(1,1,1,1,1,2), seg.len=4)
mtext("Phylogenetic automata results dashboard", 3, 1.5, col="black")
mtext("probability", 1, 2, col="black", cex=.7)
mtext("frequency", 2, 2.5, col="black", cex=.7)
mtext("input parameters", 2, 5, col="purple")

axis(2)








}
