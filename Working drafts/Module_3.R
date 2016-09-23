# Refer to Module_3_Collapse_sim_files_into_table.R to collapse all the new cluster files into a new table. 

# Ty Tuff and Bruno Vilela
# 23 August 2016
# Washington University in St. Loius


## Subset those results across different models

# We need to find marginal distribution estimates for each parameter value first and then use those estimates to find the joint parameter estimates. 

# no diffusion
no_diffusion <- subset(files, P.diffusion_Target_forager == "00" & P.diffusion_Target_domesticator == "00" & P.diffusion_Source_forager == "00" & P.diffusion_Source_domesticator == "00")

no_TO <- subset(files, P.diffusion_Target_forager == "00" & P.diffusion_Target_domesticator == "00" & P.diffusion_Source_forager == "00" & P.diffusion_Source_domesticator == "00")

no_TO_or_diff <- subset(files, P.diffusion_Target_forager == "00" & P.diffusion_Target_domesticator == "00" & P.diffusion_Source_forager == "00" & P.diffusion_Source_domesticator == "00")


head(sub_diff)

files <- sub_diff





## Now plot a panal with all the stats against a single parameter range


par(mfrow=c(7,4), mar=c(2,2,3,0))

ys <- matrix(c(0,5000), 62, 2)

ys[36,] <- c(0, 2600)
ys[37,] <- c(0, 50)
ys[38,] <- c(0, 0.08)
ys[39,] <- c(0, 0.025)
ys[40,] <- c(0, 5589260)
ys[41,] <- c(0, 3.5)
ys[42,] <- c(0, 3.5)
ys[43,] <- c(0, 50)
ys[44,] <- c(0, 0.16)
ys[45,] <- c(0, 0.015)
ys[46,] <- c(0, 60)
ys[47,] <- c(0, 2.3e-121)
ys[48,] <- c(0, 10000)
ys[49,] <- c(0, 10000)
ys[50,] <- c(0.9, 1.1)
ys[51,] <- c(0, 2)
ys[52,] <- c(0, 13500)
ys[53,] <- c(0, 13500)
ys[54,] <- c(0, 50500)
ys[55,] <- c(0, 30500)
ys[56,] <- c(0, 6000)
ys[57,] <- c(0, 6000)
ys[58,] <- c(0, 50)
ys[59,] <- c(-0.2, 1.8)
ys[60,] <- c(0, 70)
ys[61,] <- c(0, 100)
ys[62,] <- c(-1, 1.1)


#"speciation_of_Env_NonD",
#	"speciation_of_Env_D",
#	"speciation_of_For",
#	"speciation_of_Dom",
#	NA,
#	"extinction_of_Env_NonD",
#	"extinction_of_Env_D",
#	"extinction_of_For",
#	"extinction_of_Dom",
#	NA,
#	"P.diffusion_Target_forager",
#	"P.diffusion_Target_domesticator",
#	"P.diffusion_Source_forager",
#	"P.diffusion_Source_domesticator",
#	NA,
#	"P.takeover_Target_forager",
#	"P.takeover_Target_domesticator",
#	"P.takeover_Source_forager",
#	"P.takeover_Source_domesticator",
#	NA,
#	"arisal_of_Env_NonD",
#	"arisal_of_Env_D",
#	"arisal_of_For",
#	"arisal_of_Dom",
	
take <- which(as.numeric(as.character(files[,56])) >5000)
files[take,56] <- NA

take <- which(as.numeric(as.character(files[,57])) >5000)
files[take,57] <- NA

parameter <- as.numeric(as.character(files$extinction_of_For))
par(mfrow=c(7,4), mar=c(2,2,3,0))
for(i in 36:62){
plot(parameter, as.numeric(as.character(files[,i])), main=colnames(files)[i], ylim= c(ys[i,1], ys[i,2]), type="p", xlim=c(0,1))
model<- lm(as.numeric(as.character(files[,i])) ~ parameter)
abline(model, col="red")
}
summary(model)
names(files)
stat <- as.numeric(as.character(files[,46]))
mean(stat, na.rm=TRUE)






 hist(as.numeric(as.character(files[,56][-take])))



lay <- layout(matrix(c(1,2),2,1), height=c(1,.5))
par(mar=c(0,4,0,2))
#x <- seq(1,1000,by=1)
#y <- (x*8)+ rnorm(100, mean=10, sd=100)

parameter <- as.numeric(as.character(files$extinction_of_For))
stat <- as.numeric(as.character(files[,46]))

x <- parameter
y <- stat


plot(x,y, type="n", xlab="parameter value", ylab="statistic value", xlim=c(0,1), ylim=c(20,65))
model <- lm(y~x)

abline(v=0)
abline(v=1)
abline(model, col="blue")

choosen_y <- 52.2

abline(h= choosen_y, col="red")

Intercept <- summary(model)[[4]][1]
slope <- summary(model)[[4]][2] 


y=mx+b

dens <- rep(NA, length(x))
for(h in 1:length(x)){
new_b <- -1 * (slope*x[h] - y[h])
new_x <- (choosen_y-new_b)/slope
dens[h] <- new_x
#lines(c(new_x, x[h]), c(choosen_y, y[h]), col=adjustcolor("grey", alpha=.5))
#points(x,y, col=adjustcolor("blue", alpha=.1), pch=20, cex=.5)

}



points(dens, rep(choosen_y, length(dens)), col="firebrick", pch=20, cex=.5)

out <- hist(dens, plot=FALSE, breaks=100)




for(k in 1:length(out$breaks)){
polygon(c(out$breaks[k-1], out$breaks[k-1], out$breaks[k] , out$breaks[k]), c(choosen_y, out$density[k-1] + choosen_y , out$density[k-1] + choosen_y, choosen_y), col=adjustcolor("limegreen", alpha=.3), border="limegreen")
}

#abline(v=0)
#abline(v=1)

#lines(out$mids, out$counts + choosen_y, col="cornflowerblue", lwd=2)

plot(x,y, type="n", ylim=c(0, choosen_y), xaxt="n", yaxt="n", bty="n", ylab="", xlab="", xlim=c(-5,5))

for(k in 1:length(out$breaks)){
polygon(c(out$breaks[k-1], out$breaks[k-1], out$breaks[k] , out$breaks[k]), c(choosen_y, choosen_y - out$density[k-1], choosen_y - out$density[k-1], choosen_y), col=adjustcolor("limegreen", alpha=.8), border="limegreen")
}

test.tree

