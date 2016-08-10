# NOT done!!!


##################################################################################
FarmWeibullPlots <- function(which_model1, which_model2, which_model3, which_model4 ){
		
	colors <-  c("cornflowerblue", "firebrick", "darkgreen", "orange")
	
	data.result_sub_1 <- unlist(which_model1[[9]])
	data.result_sub_2 <- unlist(which_model2[[9]])
	data.result_sub_3 <- unlist(which_model3[[9]])
	data.result_sub_4 <- unlist(which_model4[[9]])
	
	
	blankplot(c(0,5), c(-100, 5000))

	sd <- sd(data.result_sub_1, na.rm=TRUE)
	polygon(x=c(0.75,0.75,1.25,1.25), y=c(mean(data.result_sub_1, na.rm=TRUE)-sd, mean(data.result_sub_1, na.rm=TRUE)+sd, mean(data.result_sub_1, na.rm=TRUE)+sd,mean(data.result_sub_1, na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	
	sd <- sd(data.result_sub_2, na.rm=TRUE)
	polygon(x=c(1.75,1.75,2.25,2.25), y=c(mean(data.result_sub_2, na.rm=TRUE)-sd, mean(data.result_sub_2, na.rm=TRUE)+sd, mean(data.result_sub_2, na.rm=TRUE)+sd,mean(data.result_sub_2, na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	
	sd <- sd(data.result_sub_3, na.rm=TRUE)
	polygon(x=c(2.75,2.75,3.25,3.25), y=c(mean(data.result_sub_3, na.rm=TRUE)-sd, mean(data.result_sub_3, na.rm=TRUE)+sd, mean(data.result_sub_3, na.rm=TRUE)+sd,mean(data.result_sub_3, na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	
	sd <- sd(data.result_sub_4, na.rm=TRUE)
	polygon(x=c(3.75,3.75,4.25,4.25), y=c(mean(data.result_sub_4, na.rm=TRUE)-sd, mean(data.result_sub_4, na.rm=TRUE)+sd, mean(data.result_sub_4, na.rm=TRUE)+sd,mean(data.result_sub_4, na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	
	
	lines(x=c(0.50,1.50), y=rep(mean(data.result_sub_1, na.rm=TRUE),2), col=colors[1], lwd=2)
	lines(x=c(1.50,2.50), y= rep(mean(data.result_sub_2, na.rm=TRUE),2), col=colors[2], lwd=2)
	lines(x=c(2.50,3.50), y= rep(mean(data.result_sub_3, na.rm=TRUE),2), col=colors[3], lwd=2)
	lines(x=c(3.50,4.50), y= rep(mean(data.result_sub_4, na.rm=TRUE),2), col=colors[4], lwd=2)
	
	points(jitter(rep(1, length(data.result_sub_1)),7), data.result_sub_1, col=adjustcolor(colors[1], alpha=.05), pch=20)
	points(jitter(rep(2, length(data.result_sub_2)),4), data.result_sub_2, col=adjustcolor(colors[2], alpha=.05), pch= 20)
	points(jitter(rep(3, length(data.result_sub_3)),3), data.result_sub_3, col=adjustcolor(colors[3], alpha=.05), pch= 20)
	points(jitter(rep(4, length(data.result_sub_4)),2), data.result_sub_4, col=adjustcolor(colors[4], alpha=.05), pch= 20)
	
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



#FarmWeibullPlots(sea, sea.D, sea.T, sea.DT)
#max(data.result_sub)