

FarmGammaPlotBoxTwo <- function(which_model1, which_model2, which_model3, which_model4){
		
	colors <-  c("limegreen", "orange")
	
	data.result_sub_1 <- unlist(which_model1[[24]])
	data.result_sub_2 <- unlist(which_model2[[24]])
	data.result_sub_3 <- unlist(which_model3[[24]])
	data.result_sub_4 <- unlist(which_model4[[24]])



par(mar=c(0,6,0,0))
	plot(0,0, xlim=c(0.5,2.5), ylim=c(0, 60), type="n", bty="n", xaxt="n", ylab="Gamma statistic")
	#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)

	sd <- sd(data.result_sub_1, na.rm=TRUE)
	polygon(x=c(0.75,0.75,1.25,1.25), y=c(mean(data.result_sub_1, na.rm=TRUE)-sd, mean(data.result_sub_1, na.rm=TRUE)+sd, mean(data.result_sub_1, na.rm=TRUE)+sd,mean(data.result_sub_1, na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	
	#sd <- sd(data.result_sub_2, na.rm=TRUE)
	#polygon(x=c(1.75,1.75,2.25,2.25), y=c(mean(data.result_sub_2, na.rm=TRUE)-sd, mean(data.result_sub_2, na.rm=TRUE)+sd, mean(data.result_sub_2, na.rm=TRUE)+sd,mean(data.result_sub_2, na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	
	sd <- sd(data.result_sub_3, na.rm=TRUE)
	polygon(x=c(1.75,1.75,2.25,2.25), y=c(mean(data.result_sub_3, na.rm=TRUE)-sd, mean(data.result_sub_3, na.rm=TRUE)+sd, mean(data.result_sub_3, na.rm=TRUE)+sd,mean(data.result_sub_3, na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	
	#sd <- sd(data.result_sub_4, na.rm=TRUE)
	#polygon(x=c(3.75,3.75,4.25,4.25), y=c(mean(data.result_sub_4, na.rm=TRUE)-sd, mean(data.result_sub_4, na.rm=TRUE)+sd, mean(data.result_sub_4, na.rm=TRUE)+sd,mean(data.result_sub_4, na.rm=TRUE)-sd), col=adjustcolor("grey",alpha=.9), border=adjustcolor("grey",alpha=.9))
	
	
	lines(x=c(0.50,1.50), y=rep(mean(data.result_sub_1, na.rm=TRUE),2), col=colors[1], lwd=2)
	#lines(x=c(1.50,2.50), y= rep(mean(data.result_sub_2, na.rm=TRUE),2), col=colors[2], lwd=2)
	lines(x=c(1.50,2.50), y= rep(mean(data.result_sub_3, na.rm=TRUE),2), col=colors[2], lwd=2)
	#lines(x=c(3.50,4.50), y= rep(mean(data.result_sub_4, na.rm=TRUE),2), col=colors[4], lwd=2)
	
	points(jitter(rep(1, length(data.result_sub_1)),7), data.result_sub_1, col=adjustcolor(colors[1], alpha=.5), pch=20)
	#points(jitter(rep(2, length(data.result_sub_2)),4), data.result_sub_2, col=adjustcolor(colors[2], alpha=.5), pch= 20)
	points(jitter(rep(2, length(data.result_sub_3)),3), data.result_sub_3, col=adjustcolor(colors[2], alpha=.5), pch= 20)
	#points(jitter(rep(4, length(data.result_sub_4)),2), data.result_sub_4, col=adjustcolor(colors[4], alpha=.5), pch= 20)
	
	axis(1, labels=c("base model", "base model + takeover"), at=c(1,2), lwd=0.01)
}




FarmGammaPlotDist <- function(which_model1, which_model2, which_model3, which_model4){
	require(MASS)
	
	colors <-  c("cornflowerblue", "firebrick", "darkgreen", "orange")
	
	data.result_sub_1 <- unlist(which_model1[[24]])
	data.result_sub_2 <- unlist(which_model2[[24]])
	data.result_sub_3 <- unlist(which_model3[[24]])
	data.result_sub_4 <- unlist(which_model4[[24]])



par(mar=c(0,4,0,0))


	plot(0,0, xlim=c(0,60), ylim=c(0, .5), type="n", bty="n", xaxt="n", yaxt="n", ylab="Branch lengths")
	#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)


	
	gamma_parameters_1 <- fitdistr(data.result_sub_1[-141], "gamma")
	gamma_parameters_2 <- fitdistr(data.result_sub_2, "gamma")
	gamma_parameters_3 <- fitdistr(data.result_sub_3, "gamma")
	gamma_parameters_4 <- fitdistr(data.result_sub_4, "gamma")
	
	
	colors <- adjustcolor(c("cornflowerblue", "firebrick", "darkgreen", "orange"), alpha=.6)

x  <- seq(0,60,length.out=100)
hx <- dgamma(x, shape= gamma_parameters_1[[1]][1], rate= gamma_parameters_1[[1]][2])
lines(x, hx, col=colors[1])
hx <- dgamma(x, shape=gamma_parameters_2[[1]][1], rate=gamma_parameters_2[[1]][2])
lines(x, hx, col=colors[2])
hx <- dgamma(x, shape=gamma_parameters_3[[1]][1], rate=gamma_parameters_3[[1]][2])
lines(x, hx, col=colors[3])
hx <- dgamma(x, shape=gamma_parameters_4[[1]][1], rate=gamma_parameters_4[[1]][2])
lines(x, hx, col=colors[4])
#axis(1, labels=FALSE)
axis(4)

	
	
}




	
	
