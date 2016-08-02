# Script for creating a half page plot of preliminary model outputs for a grant proposal. 
#
# Ty Tuff, Bruno Vilela, and Carlos Botero
# Washington University in Saint Louis
# 2 August 2016
# 
# This plot will include 4 panels: lineage through time, gamma, map, and tree.



#setwd("~/Desktop")
setwd("~/Box Sync/colliding ranges/Simulations_humans")
#####################################################################

rm(list = ls())  # remove existing objects from workspace.

# Load all the functions used in this script from a folder where they are each stored and documented seperately. 
load.files <- list.files(path = "Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}

#####################################################################


## Load results data from each of the 4 models
load("results cluster output/Results_ analysis_for_model_25_simulated_for_ 5000_timesteps_180_replicates_used.R")
sea 		<- returns  ## S + E + A 

load("results cluster output/Results_ analysis_for_model_29_simulated_for_ 5000_timesteps_168_replicates_used.R")
sea.T 	<- returns  ## S + E + A + T

load("results cluster output/Results_ analysis_for_model_28_simulated_for_ 5000_timesteps_168_replicates_used.R")
sea.D 	<- returns  ## S + E + A + D

load("results cluster output/Results_ analysis_for_model_31_simulated_for_ 5000_timesteps_168_replicates_used.R")
sea.DT 	<- returns ## S + E + A + D + T
names(returns)

#### These results are plotted across 4 different pages, each with their own pdf call

##################################################################
# PAGE  
# getwd()
pdf( file = "Figures/Half_page_graphical_summary_for_proposal.pdf", width = 11, height = 8.5) # start page one, this command ends when the command dev.off() is called.

# The layout function establishes the grid background for plots to be plotted to. 
# This layout should contain a boarder around the periphery for formatting adjustment later. 
# This layout should contain blank rows and columns between primary plot boxes for later formatting. 

# This plot needs to house 12 plot boxes (4 x 3). Adding a margin box between each of those boxes and around the periphery defines a 9 x 9. The first three plot boxes (and the two margin boxes seperating them) in the first row will be joined together into a single plot box.
# Matrix arrangement:
# 1, 1, 1, 1, 1, 1, 1, 1, 1,     						# top margin
# 2, 3, 3, 3, 3, 3, 4, 5, 6, 							# top row
# 7, 7, 7, 7, 7, 7, 7, 7, 7, 							# first internal margin
# 8, 9, 10, 11 ,12, 13, 14, 15, 16,   			# second row
# 17, 17, 17, 17, 17, 17, 17, 17, 17, 		# second internal margin
# 18, 19, 20, 21, 22, 23, 24,	 25, 26, 		# third row
# 27, 27, 27, 27, 27, 27, 27, 27, 27, 		# third internal margin
# 28, 29, 30, 31, 32, 33, 34, 35, 36, 		# bottom row
# 37, 37, 37, 37, 37, 37, 37, 37, 37, 		# bottom margin
page_one_layout_matrix <- matrix(c( rep( 1, 9 ), 2, rep( 3, 5 ), 4:6, rep( 7,  9 ), 8:16, rep( 17, 9), 18:26, rep( 27, 9 ), 28:36, rep( 37, 9 ) ), 9, 9, byrow=TRUE)

# Specify the layout. Alternate the width calls so that margins are different between plot boxes and margin boxes. 
page_one_layout <-layout(page_one_layout_matrix, width=c( 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1), height=c( 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1))    
#layout.show(page_one_layout) 
par(mar=c(0,0,0,0)) #set the default margin size within plot boxes to 0. This transfers the control of margin size to the width specified in the layout call.


sea_LTT <- unlist(sea[[22]])
sea.T_LTT <- unlist(sea.T[[22]])
sea_WT <- unlist(sea[[23]])
sea.T_WT <- unlist(sea.T[[23]])
summary(sea_WT)

plot(unlist(sea[[23]]), unlist(sea[[22]]), col=adjustcolor(color_choice[1], alpha=.8), type="l")
  

color_choice <- c("firebrick", "cornflowerblue", "limegreen", "grey")

plot(0,0, type="n", xlim=c(0,8), ylim=c(0,300), xlab="log(number of tips)", ylab="time between tips")

for(h in 1:length(sea[[3]][1,])){
  lines(sea[[23]][,h], sea[[22]][,h], col=adjustcolor(color_choice[1], alpha=.8))
  
}

for(h in 1:length(b[[3]][1,])){
  lines(b[[2]][,h], b[[3]][,h], col=adjustcolor(color_choice[2], alpha=.8))
  
}

for(h in 1:length(c[[3]][1,])){
  lines(c[[2]][,h], c[[3]][,h], col=adjustcolor(color_choice[3], alpha=.8))
  
}

for(h in 1:length(d[[3]][1,])){
  lines(d[[2]][,h], d[[3]][,h], col=adjustcolor(color_choice[4], alpha=.8))
  
}
dev.off()

h <- 5


ltt_mean_a <- rep(NA, length(sea_LTT))
ltt_SD_a <- rep(NA, length(sea_LTT))
time_avg_a <- rep(NA, length(sea_LTT))
for(h in 1:length(sea_WT)){
  ltt_mean_a[h] <- mean(sea_LTT, na.rm =TRUE)
  ltt_SD_a[h] <- sd(sea_LTT, na.rm=TRUE)
  time_avg_a[h] <- mean(sea_WT, na.rm=TRUE)
}

ltt_mean_b <- rep(NA, length(b[[3]][,8]))
ltt_SD_b <- rep(NA, length(b[[3]][,8]))
time_avg_b <- rep(NA, length(b[[3]][,8]))
for(h in 1:length(b[[3]][,8])){
  ltt_mean_b[h] <- mean(b[[3]][h,], na.rm =TRUE)
  ltt_SD_b[h] <- sd(b[[3]][h,], na.rm=TRUE)
  time_avg_b[h] <- mean(b[[2]][h,], na.rm=TRUE)
}

ltt_mean_c <- rep(NA, length(c[[3]][,1]))
ltt_SD_c <- rep(NA, length(c[[3]][,1]))
time_avg_c <- rep(NA, length(b[[3]][,8]))
for(h in 1:length(c[[3]][,1])){
  ltt_mean_c[h] <- mean(c[[3]][h,], na.rm =TRUE)
  ltt_SD_c[h] <- sd(c[[3]][h,], na.rm=TRUE)
  time_avg_c[h] <- mean(c[[2]][h,], na.rm=TRUE)
}

ltt_mean_d <- rep(NA, length(d[[3]][,1]))
ltt_SD_d <- rep(NA, length(d[[3]][,1]))
time_avg_d <- rep(NA, length(b[[3]][,8]))
for(h in 1:length(d[[3]][,1])){
  ltt_mean_d[h] <- mean(d[[3]][h,], na.rm =TRUE)
  ltt_SD_d[h] <- sd(d[[3]][h,], na.rm=TRUE)
  time_avg_d[h] <- mean(d[[2]][h,], na.rm=TRUE)
}



plot(0,0, type="n", xlim=c(0,8), ylim=c(0,300), xlab="log(number of tips)", ylab="time between tips")

alpha_level <- .1

cap <- length(na.omit(ltt_mean_d + ltt_SD_d))
xd <- c(na.omit(time_avg_d)[1:cap], rev(na.omit(time_avg_d)[1:cap]))
yd <- c(na.omit(ltt_mean_d + ltt_SD_d),rev(na.omit(ltt_mean_d - ltt_SD_d)))
polygon(x= xd, y=yd, col=adjustcolor("grey", alpha= alpha_level), border="grey", lty=2)
lines(d[[2]][,1], ltt_mean_d, col="grey", lwd=3)


cap <- length(na.omit(ltt_mean_a + ltt_SD_a))
xa <- c(na.omit(time_avg_a)[1:cap], rev(na.omit(time_avg_a)[1:cap]))
ya <- c(na.omit(ltt_mean_a + ltt_SD_a),rev(na.omit(ltt_mean_a - ltt_SD_a)))
polygon(x= xa, y=ya, col=adjustcolor("firebrick", alpha= alpha_level), border="firebrick", lty=2)
lines(a[[2]][,1], ltt_mean_a, col="firebrick", lwd=3)


cap <- length(na.omit(ltt_mean_b + ltt_SD_b))
xb <- c(na.omit(time_avg_b)[1:cap], rev(na.omit(time_avg_b)[1:cap]))
yb <- c(na.omit(ltt_mean_b + ltt_SD_b),rev(na.omit(ltt_mean_b - ltt_SD_b)))
polygon(x= xb, y=yb, col=adjustcolor("cornflowerblue", alpha= alpha_level), border="cornflowerblue", lty=2)
lines(b[[2]][,1], ltt_mean_b, col="cornflowerblue", lwd=3)


cap <- length(na.omit(ltt_mean_c + ltt_SD_c))
xc <- c(na.omit(time_avg_c)[1:cap], rev(na.omit(time_avg_c)[1:cap]))
yc <- c(na.omit(ltt_mean_c + ltt_SD_c),rev(na.omit(ltt_mean_c - ltt_SD_c)))
polygon(x= xc, y=yc, col=adjustcolor("limegreen", alpha= alpha_level), border="limegreen", lty=2)
lines(c[[2]][,1], ltt_mean_c, col="limegreen", lwd=3)


legend(0,300,legend=rev(c("S+E+A","S+E+A + Diffusion", "S+E+A + Takeover", "Full model")), col=c("firebrick", "cornflowerblue", "limegreen", "grey"), lty=1, lwd=3)












