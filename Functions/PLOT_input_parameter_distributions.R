





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
data.result.D1 <- as.numeric(data.result[ , 12])
data.result.D2 <- as.numeric(data.result[ , 13])
data.result.D3 <- as.numeric(data.result[ , 14])
data.result.D4 <- as.numeric(data.result[ , 15])
data.result.T1 <- as.numeric(data.result[ , 16])
data.result.T2 <- as.numeric(data.result[ , 17])
data.result.T3 <- as.numeric(data.result[ , 18])
data.result.T4 <- as.numeric(data.result[ , 19])
data.result.A <- as.numeric(data.result[ , 20])


input_parameters <- cbind(data.result.S1 ,
data.result.S2 ,
data.result.S3 ,
data.result.S4 ,
data.result.E1 ,
data.result.E2 ,
data.result.E3 ,
data.result.E4 ,
data.result.D1 ,
data.result.D2 ,
data.result.D3 ,
data.result.D4 ,
data.result.T1 ,
data.result.T2 ,
data.result.T3 ,
data.result.T4 ,
data.result.A)

head(input_parameters)
par(mar=c(2,2,0,0))

plot(0,0,xlim=c(0,1), ylim=c(0,100))
	line_weight <- 3

colors <- adjustcolor(c(rep("red", 4), rep("blue", 4), rep("green", 4), rep("purple", 4), "orange"), alpha=.8)
h <- 17

for(h in 1:length(input_parameters[1,])){
abline(v=mean(na.omit(input_parameters[,h])), col = colors[h], lty=2, lwd=2)
if(length(unique(input_parameters[,h])) > 1){ 
dens <- hist(na.omit(input_parameters[,h]), add=TRUE, col = colors[h], breaks=50, border=NA)
#polygon(dens, col = colors[h], border="darkgrey")
} 



}





legend(0.7, 89.5, c("Speciation rate", "Extinction rate","Diffusion rate", "Takeover rate", "Arisal rate", "mean per distribution"),col= adjustcolor(c("red", "blue", "green", "purple", "orange", "black"), alpha=.9), lwd=c(3,3,3,3,3,1.5), lty=c(1,1,1,1,1,2), seg.len=4)
mtext("Distribution of values used as input parameters", 3, 1.5, col="darkgrey")
mtext("probability", 1, 2, col="black", cex=.7)
mtext("frequency", 2, 2.5, col="black", cex=.7)

axis(2)



}




#input_parameters_plot(sea, sea.T, sea.D, sea.DT)






