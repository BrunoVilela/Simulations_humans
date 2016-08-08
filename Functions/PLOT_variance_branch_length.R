


##################################################################################
FarmVarianceBranchLength <- function(which_model1, which_model2, which_model3, which_model4 ){
		
	colors <-  c("cornflowerblue", "firebrick", "darkgreen", "orange")
	
	data.result_sub_1 <- unlist(which_model1[[12]])
	data.result_sub_2 <- unlist(which_model2[[12]])
	data.result_sub_3 <- unlist(which_model3[[12]])
	data.result_sub_4 <- unlist(which_model4[[12]])
	
	par(mar=c(2,2,0,0))
	plot(0,0, xlim=c(0,5), ylim=c(0, 900000), type="n", bty="n", xaxt="n", ylab="Branch lengths")
	#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)


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
	
	points(jitter(rep(1, length(data.result_sub_1)),7), data.result_sub_1, col=adjustcolor(colors[1], alpha=.5), pch=20)
	points(jitter(rep(2, length(data.result_sub_2)),4), data.result_sub_2, col=adjustcolor(colors[2], alpha=.5), pch= 20)
	points(jitter(rep(3, length(data.result_sub_3)),3), data.result_sub_3, col=adjustcolor(colors[3], alpha=.5), pch= 20)
	points(jitter(rep(4, length(data.result_sub_4)),2), data.result_sub_4, col=adjustcolor(colors[4], alpha=.5), pch= 20)
	
}

#FarmRawBranchLength(sea, sea.D, sea.T, sea.DT)
