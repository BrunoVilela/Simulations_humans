
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


