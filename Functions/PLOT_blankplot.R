##################################################################################
blankplot <- function(xlim,ylim){
	par(mar=c(0,0,0,0))
	plot(0,0,xaxt="n", yaxt="n", type="n", xlab="", ylab="",  xlim=xlim, ylim=ylim, bty="n")
}
