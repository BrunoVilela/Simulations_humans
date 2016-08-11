#Not finished! 

which_model1 <- sea
which_model2 <- sea.D
which_model3 <- sea.T
which_model4 <- sea.DT


##################################################################################
LineageThroughTimeTV <- function(which_model1, which_model2, which_model3, which_model4 ){
		
	colors <-  c("cornflowerblue", "firebrick", "darkgreen", "orange")
	alpha_level <- .1
	
	data.result_sub_1_x <- unlist(which_model1[[22]])
	data.result_sub_2_x <- unlist(which_model2[[22]])
	data.result_sub_3_x <- unlist(which_model3[[22]])
	data.result_sub_4_x <- unlist(which_model4[[22]])
	
	data.result_sub_1_y <- unlist(which_model1[[23]])
	data.result_sub_2_y <- unlist(which_model2[[23]])
	data.result_sub_3_y <- unlist(which_model3[[23]])
	data.result_sub_4_y <- unlist(which_model4[[23]])
	
	par(mar=c(2,2,0,0))
	#plot(0,0, type="n", xlim=c(0,1300), ylim=c(0,300), xlab="log(number of tips)",
    # ylab="time between tips")
	#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)
data.result_input_x <- data.result_sub_1_x
data.result_input_y <- data.result_sub_1_y
h<- 100
PlotLTT <- function(data.result_input_x, data.result_input_y, colors){
mat <- as.matrix(data.result_input_x)
rownames(mat) <- seq(1:length(mat))
matr <- as.matrix(data.result_input_y)
rownames(matr) <- seq(1:length(matr))

length(mat) == length(matr)
mat[which(matr == 0)] <- NA
matr[which(matr == 0)] <- NA
mat <- mat[order(matr)]
matr <- matr[order(matr)]
both <- cbind(mat, matr)

ltt_mean_a <- rep(NA, length(unique(both[,1])))
ltt_SD_a <- rep(NA, length(unique(both[,1])))
ltt_CI_a <- rep(NA, length(unique(both[,1])))
for(h in 1: length(unique(both[,1]))){
one_step <- which(unique(both[,1])[h] == both[,1])
ltt_mean_a[h] <- mean(both[one_step, 2], na.rm=TRUE)
  ltt_SD_a[h] <- sd(both[one_step, 2], na.rm=TRUE)
  ltt_CI_a[h] <- try(qt(0.975,df=length(both[one_step, ])-1) * sd(both[one_step, 2])/sqrt(length(both[one_step, 2])), silent=TRUE)
}






time_avg_a <- unique(both[,1])
cap <- length(na.omit(ltt_mean_a + ltt_SD_a))
xd <- c(na.omit(time_avg_a)[1:cap], rev(na.omit(time_avg_a)[1:cap]))
yd <- c(na.omit(ltt_mean_a + ltt_SD_a),rev(na.omit(ltt_mean_a - ltt_SD_a)))
polygon(x= yd, y=xd, col=adjustcolor(colors, alpha= alpha_level), border="grey", lty=2)
#lines(ltt_mean_a - ltt_SD_a, unique(both[,1]), lty=2, col=colors)
#lines(ltt_mean_a + ltt_SD_a, unique(both[,1]), lty=2, col=colors)
lines(ltt_mean_a, unique(both[,1]), lwd=3, col=adjustcolor(colors, alpha= .5))


}


plot(data.result_sub_1_x~ data.result_sub_1_y, type="n", xlim=c(1,6000))

PlotLTT(data.result_sub_1_x, data.result_sub_1_y, colors[1])
PlotLTT(data.result_sub_2_x, data.result_sub_2_y, colors[2])
PlotLTT(data.result_sub_3_x, data.result_sub_3_y, colors[3])
PlotLTT(data.result_sub_4_x, data.result_sub_4_y, colors[4])


legend(0,1100,legend=rev(c("S+E+A","S+E+A + Diffusion", "S+E+A + Takeover", "Full model")), col=rev(c("cornflowerblue", "firebrick", "darkgreen", "orange")), lty=1, lwd=3)




}

#LineageThroughTimeTV(sea, sea.D, sea.T, sea.DT)

