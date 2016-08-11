#Not finished! 

which_model1 <- sea
which_model2 <- sea.D
which_model3 <- sea.T
which_model4 <- sea.DT


##################################################################################
LineageThroughTimeTV <- function(which_model1, which_model2, which_model3, which_model4 ){
		
	colors <-  c("cornflowerblue", "firebrick", "darkgreen", "orange")
	
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
     ylab="time between tips")
	#axis(1, labels = c("S+E+A", "S+E+A+T", "S+E+A+D", "S+E+A+T+D"), at=c(1,2,3,4), las=3)

plot(data.result_sub_1_x, data.result_sub_1_y, type="l")
order(as.matrix(data.result_sub_1_x)) %in% order(as.matrix(data.result_sub_1_y))

mean(as.numeric(rownames(as.matrix(data.result_sub_1_x))), na.rm=TRUE)

mat <- as.matrix(data.result_sub_1_x)
rownames(mat) <- seq(1:length(mat))

matr <- as.matrix(data.result_sub_1_y)
rownames(matr) <- seq(1:length(matr))

length(mat) == length(matr)
mat[which(matr == 0)] <- NA
matr[which(matr == 0)] <- NA
mat <- mat[order(matr)]
matr <- matr[order(matr)]
both <- cbind(mat, matr)

ltt_mean_a <- rep(NA, length(unique(both[,1])))
ltt_SD_a <- rep(NA, length(unique(both[,1])))
for(h in 1: length(unique(both[,1]))){
one_step <- which(unique(both[,1])[h] == both[,1])

ltt_mean_a[h] <- mean(both[one_step, ], na.rm=TRUE)
  ltt_SD_a[h] <- sd(both[one_step, ], na.rm=TRUE)
}
plot(ltt_mean_a)


ltt_mean_a <- rep(NA, length(a[[3]][,1]))
ltt_SD_a <- rep(NA, length(a[[3]][,1]))
time_avg_a <- rep(NA, length(b[[3]][,8]))
for(h in 1:length(a[[3]][,1])){
  ltt_mean_a[h] <- mean(a[[3]][h,], na.rm =TRUE)
  ltt_SD_a[h] <- sd(a[[3]][h,], na.rm=TRUE)
  time_avg_a[h] <- mean(a[[2]][h,], na.rm=TRUE)
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




}

#FarmRawBranchLength(sea, sea.D, sea.T, sea.DT)

