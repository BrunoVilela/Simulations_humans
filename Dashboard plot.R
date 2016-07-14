
setwd("~/Box Sync/colliding ranges/Simulations_humans/results cluster output")

##################################################################################
blankplot <- function(xlim,ylim){
	plot(0,0,xaxt="n", yaxt="n", type="n", xlab="", ylab="",  xlim=xlim, ylim=ylim, bty="n")
}


##################################################################################
input_parameters_plot <- function(data.result_sub){

	line_weight <- 3

colors <- adjustcolor(c("red", "blue", "green", "orange", "purple"), alpha=.6)

abline(v=mean(na.omit(as.numeric(data.result_sub[,1]))), col = colors[1], lty=2, lwd=2)
abline(v=mean(na.omit(as.numeric(data.result_sub[,2]))), col = colors[2], lty=2, lwd=2)
abline(v=mean(na.omit(as.numeric(data.result_sub[,3]))), col = colors[3], lty=2, lwd=2)
abline(v=mean(na.omit(as.numeric(data.result_sub[,4]))), col = colors[4], lty=2, lwd=2)
abline(v=mean(na.omit(as.numeric(data.result_sub[,5]))), col = colors[5], lty=2, lwd=2)

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

}


##################################################################################
predominant_cover <- function(data.result_sub){
	#data.result_sub <-cbind(a$Difference, b$Difference, c$Difference, d$Difference)
	blankplot(xlim=c(.5,4.5), ylim=c(-350,350))
	colors <-  c("cornflowerblue", "firebrick", "darkgreen", "orange")
	
	abline(h=0, lwd=1, col=adjustcolor("black", alpha=1), lty=1)
	

	sd <- sd(data.result_sub[,1], na.rm=TRUE)
	polygon(x=c(0.75,0.75,1.25,1.25), y=c(mean(data.result_sub[,1], na.rm=TRUE)-sd, mean(data.result_sub[,1], na.rm=TRUE)+sd, mean(data.result_sub[,1], na.rm=TRUE)+sd,mean(data.result_sub[,1], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	sd <- sd(data.result_sub[,2], na.rm=TRUE)
	polygon(x=c(1.75,1.75,2.25,2.25), y=c(mean(data.result_sub[,2], na.rm=TRUE)-sd, mean(data.result_sub[,2], na.rm=TRUE)+sd, mean(data.result_sub[,2], na.rm=TRUE)+sd,mean(data.result_sub[,2], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	sd <- sd(data.result_sub[,3], na.rm=TRUE)
	polygon(x=c(2.75,2.75,3.25,3.25), y=c(mean(data.result_sub[,3], na.rm=TRUE)-sd, mean(data.result_sub[,3], na.rm=TRUE)+sd, mean(data.result_sub[,3], na.rm=TRUE)+sd,mean(data.result_sub[,3], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	sd <- sd(data.result_sub[,4], na.rm=TRUE)
	polygon(x=c(3.75,3.75,4.25,4.25), y=c(mean(data.result_sub[,4], na.rm=TRUE)-sd, mean(data.result_sub[,4], na.rm=TRUE)+sd, mean(data.result_sub[,4], na.rm=TRUE)+sd,mean(data.result_sub[,4], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	
	lines(x=c(0.50,1.50), y=rep(mean(data.result_sub[,1], na.rm=TRUE),2), col=colors[1], lwd=2)
	lines(x=c(1.50,2.50), y= rep(mean(data.result_sub[,2], na.rm=TRUE),2), col=colors[2], lwd=2)
	lines(x=c(2.50,3.50), y= rep(mean(data.result_sub[,3], na.rm=TRUE),2), col=colors[3], lwd=2)
	lines(x=c(3.50,4.50), y= rep(mean(data.result_sub[,4], na.rm=TRUE),2), col=colors[4], lwd=2)
	
	points(jitter(rep(1, length(data.result_sub[,1]))), data.result_sub[,1], col=adjustcolor(colors[1], alpha=.8))
	points(jitter(rep(2, length(data.result_sub[,2]))), data.result_sub[,2], col=adjustcolor(colors[2], alpha=.8))
	points(jitter(rep(3, length(data.result_sub[,3]))), data.result_sub[,3], col=adjustcolor(colors[3], alpha=.8))
	points(jitter(rep(4, length(data.result_sub[,4]))), data.result_sub[,4], col=adjustcolor(colors[4], alpha=.8))
	
}






##################################################################################
spatial_coverage <- function(data.result_sub){
	#data.result_sub <-cbind(a$DF, b$DF, c$DF, d$DF)
	blankplot(xlim=c(.5,4.5), ylim=c(-25,25))
	colors <-  c("cornflowerblue", "firebrick", "darkgreen", "orange")
	
	abline(h=0, lwd=1, col=adjustcolor("black", alpha=1), lty=1)
	

	sd <- sd(data.result_sub[,1], na.rm=TRUE)
	polygon(x=c(0.75,0.75,1.25,1.25), y=c(mean(data.result_sub[,1], na.rm=TRUE)-sd, mean(data.result_sub[,1], na.rm=TRUE)+sd, mean(data.result_sub[,1], na.rm=TRUE)+sd,mean(data.result_sub[,1], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	sd <- sd(data.result_sub[,2], na.rm=TRUE)
	polygon(x=c(1.75,1.75,2.25,2.25), y=c(mean(data.result_sub[,2], na.rm=TRUE)-sd, mean(data.result_sub[,2], na.rm=TRUE)+sd, mean(data.result_sub[,2], na.rm=TRUE)+sd,mean(data.result_sub[,2], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	sd <- sd(data.result_sub[,3], na.rm=TRUE)
	polygon(x=c(2.75,2.75,3.25,3.25), y=c(mean(data.result_sub[,3], na.rm=TRUE)-sd, mean(data.result_sub[,3], na.rm=TRUE)+sd, mean(data.result_sub[,3], na.rm=TRUE)+sd,mean(data.result_sub[,3], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	sd <- sd(data.result_sub[,4], na.rm=TRUE)
	polygon(x=c(3.75,3.75,4.25,4.25), y=c(mean(data.result_sub[,4], na.rm=TRUE)-sd, mean(data.result_sub[,4], na.rm=TRUE)+sd, mean(data.result_sub[,4], na.rm=TRUE)+sd,mean(data.result_sub[,4], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	
	lines(x=c(0.50,1.50), y=rep(mean(data.result_sub[,1], na.rm=TRUE),2), col=colors[1], lwd=2)
	lines(x=c(1.50,2.50), y= rep(mean(data.result_sub[,2], na.rm=TRUE),2), col=colors[2], lwd=2)
	lines(x=c(2.50,3.50), y= rep(mean(data.result_sub[,3], na.rm=TRUE),2), col=colors[3], lwd=2)
	lines(x=c(3.50,4.50), y= rep(mean(data.result_sub[,4], na.rm=TRUE),2), col=colors[4], lwd=2)
	
	points(jitter(rep(1, length(data.result_sub[,1]))), data.result_sub[,1], col=adjustcolor(colors[1], alpha=.8))
	points(jitter(rep(2, length(data.result_sub[,2]))), data.result_sub[,2], col=adjustcolor(colors[2], alpha=.8))
	points(jitter(rep(3, length(data.result_sub[,3]))), data.result_sub[,3], col=adjustcolor(colors[3], alpha=.8))
	points(jitter(rep(4, length(data.result_sub[,4]))), data.result_sub[,4], col=adjustcolor(colors[4], alpha=.8))
	
}



##################################################################################
weibull_plots <- function(data.result_sub){
	#data.result_sub <-cbind(a$DF, b$DF, c$DF, d$DF)
	
	colors <-  c("cornflowerblue", "firebrick", "darkgreen", "orange")
	
	
	

	sd <- sd(data.result_sub[,1], na.rm=TRUE)
	polygon(x=c(0.75,0.75,1.25,1.25), y=c(mean(data.result_sub[,1], na.rm=TRUE)-sd, mean(data.result_sub[,1], na.rm=TRUE)+sd, mean(data.result_sub[,1], na.rm=TRUE)+sd,mean(data.result_sub[,1], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	sd <- sd(data.result_sub[,2], na.rm=TRUE)
	polygon(x=c(1.75,1.75,2.25,2.25), y=c(mean(data.result_sub[,2], na.rm=TRUE)-sd, mean(data.result_sub[,2], na.rm=TRUE)+sd, mean(data.result_sub[,2], na.rm=TRUE)+sd,mean(data.result_sub[,2], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	sd <- sd(data.result_sub[,3], na.rm=TRUE)
	polygon(x=c(2.75,2.75,3.25,3.25), y=c(mean(data.result_sub[,3], na.rm=TRUE)-sd, mean(data.result_sub[,3], na.rm=TRUE)+sd, mean(data.result_sub[,3], na.rm=TRUE)+sd,mean(data.result_sub[,3], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	sd <- sd(data.result_sub[,4], na.rm=TRUE)
	polygon(x=c(3.75,3.75,4.25,4.25), y=c(mean(data.result_sub[,4], na.rm=TRUE)-sd, mean(data.result_sub[,4], na.rm=TRUE)+sd, mean(data.result_sub[,4], na.rm=TRUE)+sd,mean(data.result_sub[,4], na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	
	lines(x=c(0.50,1.50), y=rep(mean(data.result_sub[,1], na.rm=TRUE),2), col=colors[1], lwd=2)
	lines(x=c(1.50,2.50), y= rep(mean(data.result_sub[,2], na.rm=TRUE),2), col=colors[2], lwd=2)
	lines(x=c(2.50,3.50), y= rep(mean(data.result_sub[,3], na.rm=TRUE),2), col=colors[3], lwd=2)
	lines(x=c(3.50,4.50), y= rep(mean(data.result_sub[,4], na.rm=TRUE),2), col=colors[4], lwd=2)
	
	points(jitter(rep(1, length(data.result_sub[,1]))), data.result_sub[,1], col=adjustcolor(colors[1], alpha=.8))
	points(jitter(rep(2, length(data.result_sub[,2]))), data.result_sub[,2], col=adjustcolor(colors[2], alpha=.8))
	points(jitter(rep(3, length(data.result_sub[,3]))), data.result_sub[,3], col=adjustcolor(colors[3], alpha=.8))
	points(jitter(rep(4, length(data.result_sub[,4]))), data.result_sub[,4], col=adjustcolor(colors[4], alpha=.8))
	
}















# Plot Results analysis
load("Results_for_25_simulated_for_ 300_time_steps_analysis.R")
a <- data.result
load("Results_for_29_simulated_for_ 300_time_steps_analysis.R")
b <- data.result
load("Results_for_28_simulated_for_ 300_time_steps_analysis.R")
c <- data.result
load("Results_for_31_simulated_for_ 300_time_steps_analysis.R")
d <- data.result

13*13
pdf(file="~/Desktop/Phylogenetic Automaton Dashboard.pdf", height=11, width=8.5)
length(c(1:14,rep(15,11),16:159))
w <-layout(matrix(c(1:14,rep(15,11),16:82, rep(83,3), rep(84,3), 85:155),13,13, byrow=TRUE), width=c(1, 1, 0.4, 1,1, 0.2, 1 , .2, 1, .4, 1, 1, 0.8), height=c(0.4,1,0.6,1,0.6, 1, 0.6 ,1,0.4,1,1,1, 0.2))    
#layout.show(w) 
par(mar=c(0,0,0,0))


for(i in 1:14){
blankplot(c(0,0),c(0,0))
}




data.result <- rbind(a,b,c,d)
par(mar=c(2,0,0,0))
blankplot(c(0,1), c(0,10))
input_parameters_plot(cbind(data.result$speciation_1, data.result$extinction_1, data.result$diffusion_1, data.result$takeover_1, data.result$arisal_1))
input_parameters_plot(cbind(data.result$speciation_2, data.result$extinction_2, data.result$diffusion_2, data.result$takeover_2, data.result$arisal_2))
input_parameters_plot(cbind(data.result$speciation_3, data.result$extinction_3, data.result$diffusion_3, data.result$takeover_3, data.result$arisal_3))
input_parameters_plot(cbind(data.result$speciation_4, data.result$extinction_4, data.result$diffusion_4, data.result$takeover_4, data.result$arisal_4))
axis(1)

legend(0.7, 9.5, c("Speciation rate", "Extinction rate","Diffusion rate", "Takeover rate", "Arisal rate", "mean per distribution"),col= adjustcolor(c("red", "blue", "green", "orange", "purple", "black"), alpha=.9), lwd=c(3,3,3,3,3,1.5), lty=c(1,1,1,1,1,2), seg.len=4)
mtext("Phylogenetic automata results dashboard", 3, 1.5, col="black")
mtext("probability", 1, 2, col="black", cex=.7)
mtext("frequency", 2, 2.5, col="black", cex=.7)
mtext("input parameters", 2, 5, col="purple")

axis(2)

par(mar=c(0,0,0,0))
for(i in 1:15){
blankplot(c(0,0),c(0,0))
}

par(mar=c(0,0,0,0))
####################
predominant_cover(cbind(a$Difference, b$Difference, c$Difference, d$Difference))
#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
axis(2, labels = c("foragering", "domestication"), at=c(300,-300))
axis(4)
mtext("spatial dominance", 3, 1.5, col="darkgrey")
mtext("spatial patterns", 2, 5, col="purple")
mtext("cells occupied", 4, 2.5, col="black", cex=.7)


########################
blankplot(c(0,0),c(0,0))


########################
blankplot(c(0,0),c(0,0))
spatial_coverage(cbind(cbind(a$DF, b$DF, c$DF, d$DF)))
axis(2, labels = c("clumped", "random","dispersed"), at=c(20,0,-20))
mtext("D vs. F", 3, 1.5, col="darkgrey")

########################
blankplot(c(0,0),c(0,0))
spatial_coverage(cbind(cbind(a$FF, b$FF, c$FF, d$FF)))
#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
mtext("F vs. F", 3, 1.5, col="darkgrey")

########################
blankplot(c(0,0),c(0,0))
spatial_coverage(cbind(cbind(a$DD, b$DD, c$DD, d$DD)))
#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
axis(4)
mtext("D vs. D", 3, 1.5, col="darkgrey")
mtext("z-value", 4, 2.5, col="black", cex=.7)

######################## Global extinction rate
blankplot(c(0,0),c(0,0))

blankplot(c(0,0),c(0,0))
a_trim <- a[which(is.na(a[,1]) != TRUE ),]
b_trim <- b[which(is.na(b[,1]) != TRUE ),]
c_trim <- c[which(is.na(c[,1]) != TRUE ),]
d_trim <- d[which(is.na(d[,1]) != TRUE ),]
names(a_trim)

length(a_trim)
length(b_trim)
length(c_trim)
length(d_trim)

length(a[,1])

a_extinct <- (100-(length(na.omit(a[,31]))/length(a[,31]))*100)
b_extinct <- (100-(length(na.omit(b[,31]))/length(b[,31]))*100)
c_extinct <- (100-(length(na.omit(c[,31]))/length(c[,31]))*100)
d_extinct <- (100-(length(na.omit(d[,31]))/length(d[,31]))*100)



barplot(c(a_extinct, b_extinct, c_extinct, d_extinct), ylim=c(0,100), col = adjustcolor(c("cornflowerblue", "firebrick", "darkgreen", "orange"), alpha=.6), yaxt="n", xaxt="n")
axis(4)
mtext("global extinction", 3, 1.5, col="darkgrey")
mtext("% of runs that went extinct", 4, 2.5, col="black", cex=.7)


for(i in 1:15){
blankplot(c(0,0),c(0,0))
}



blankplot(xlim=c(.5,4.5), ylim=c(-35,35))
weibull_plots(cbind(cbind(a$gamma, b$gamma, c$gamma, d$gamma)))
#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
axis(4)
axis(2, labels = c("late \n diversification", "early \n diversification"), at=c(30,-30))
abline(h=0, lwd=1, col=adjustcolor("black", alpha=1), lty=1)
mtext("branch length patterns", 2, 5, col="purple")


blankplot(c(0,0),c(0,0))
mtext("gamma distribution", 3, 1.5, col="darkgrey")

colors <- adjustcolor(c("cornflowerblue", "firebrick", "darkgreen", "orange"), alpha=.6)
blankplot(c(0,100),c(0,2))
x  <- seq(0,5,length.out=100)
hx <- dgamma(x, shape=mean(a$gamma, na.rm=TRUE) , rate=mean(a$gamma, na.rm=TRUE))
lines(hx, col=colors[1])
hx <- dgamma(x, shape=mean(b$gamma, na.rm=TRUE), rate=mean(a$gamma, na.rm=TRUE))
lines(hx, col=colors[2])
hx <- dgamma(x, shape=mean(c$gamma, na.rm=TRUE), rate=mean(a$gamma, na.rm=TRUE))
lines(hx, col=colors[3])
hx <- dgamma(x, shape=mean(d$gamma, na.rm=TRUE), rate=mean(a$gamma, na.rm=TRUE))
lines(hx, col=colors[4])
axis(1, labels=FALSE)
axis(4)

blankplot(c(0,100),c(0,1))
blankplot(c(0,100),c(0,1))
blankplot(c(0,100),c(0,1))
blankplot(c(0,100),c(0,1))

blankplot(c(0,100),c(0,0.5))
x  <- seq(0,2.5,length.out=100)
hx <- dweibull(x, shape=mean(a$shape, na.rm=TRUE), scale= mean(a$scale, na.rm=TRUE))
lines(hx, col=colors[1])
hx <- dweibull(x, shape=mean(b$shape, na.rm=TRUE), scale= mean(b$scale, na.rm=TRUE))
lines(hx, col=colors[2])
hx <- dweibull(x, shape=mean(c$shape, na.rm=TRUE), scale= mean(c$scale, na.rm=TRUE))
lines(hx, col=colors[3])
hx <- dweibull(x, shape=mean(d$shape, na.rm=TRUE), scale= mean(d$scale, na.rm=TRUE))
lines(hx, col=colors[4])
axis(1, labels=FALSE)
axis(2)

blankplot(c(0,100),c(0,1))

blankplot(xlim=c(.5,4.5), ylim=c(0,1))
weibull_plots(cbind(cbind(a$shape, b$shape, c$shape, d$shape)))
#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
axis(2)
mtext("weibull distribution", 3, 1.5, col="darkgrey")


blankplot(xlim=c(.5,4.5), ylim=c(0,35))
weibull_plots(cbind(cbind(a$scale, b$scale, c$scale, d$scale)))
#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
axis(4)

blankplot(c(0,0),c(0,0))




########################
blankplot(c(0,0),c(0,0))
blankplot(c(0,0),c(0,0))

#####################
for(i in 1:10){
blankplot(c(0,0),c(0,0))
}

blankplot(c(0,0),c(0,0))
blankplot(c(0,0),c(0,0))


blankplot(xlim=c(.5,4.5), ylim=c(0,3000))
weibull_plots(cbind(cbind(a$Colless, b$Colless, c$Colless, d$Colless)))
#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
axis(2)
mtext("Colless metric", 2, 2.5, col="black", cex=.7)
mtext("tree balance", 2, 5, col="purple")

blankplot(xlim=c(.5,4.5), ylim=c(0,200000))
weibull_plots(cbind(cbind(a$TCI, b$TCI, c$TCI, d$TCI)))
#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
axis(4)
mtext("TCI metric", 4, 2.5, col="black", cex=.7)

#####################
for(i in 1:23){
blankplot(c(0,0),c(0,0))
}

blankplot(xlim=c(.5,4.5), ylim=c(-10,10))
weibull_plots(cbind(cbind(a$Phy_Signal, b$Phy_Signal, c$Phy_Signal, d$Phy_Signal)))
axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
axis(2)

blankplot(c(0,0),c(0,0))

blankplot(xlim=c(.5,4.5), ylim=c(0,1))
weibull_plots(cbind(cbind(a$KM, b$KM, c$KM, d$KM)))
axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
axis(2)

blankplot(c(0,0),c(0,0))

blankplot(xlim=c(.5,4.5), ylim=c(0,1))
weibull_plots(cbind(cbind(a$Trasition.rates, b$Trasition.rates, c$Trasition.rates, d$Trasition.rates)))
axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
axis(2)




dev.off()




