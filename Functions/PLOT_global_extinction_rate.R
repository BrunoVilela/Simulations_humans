
##################################################################################
GlobalExtinctionRates <- function(which_model1, which_model2, which_model3, which_model4 ){
		
	colors <-  c("cornflowerblue", "firebrick", "darkgreen", "orange")
	
	data.result_sub_1 <- unlist(which_model1[[7]])
	data.result_sub_2 <- unlist(which_model2[[7]])
	data.result_sub_3 <- unlist(which_model3[[7]])
	data.result_sub_4 <- unlist(which_model4[[7]])
	
	par(mar=c(2,2,0,0))

	barplot(c(data.result_sub_1, data.result_sub_2, data.result_sub_3, data.result_sub_4),
		ylim=c(0,100), col=colors)

	axis(4)
	mtext("global extinction", 3, 1.5, col="darkgrey")
	mtext("% of runs that went extinct", 4, 2.5, col="black", cex=.7)

	
}

#GlobalExtinctionRates(sea, sea.D, sea.T, sea.DT)
