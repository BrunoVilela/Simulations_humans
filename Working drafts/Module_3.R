# Collapse the individual files produced by the first simulation module into one list with all the trees and another list with all the worlds. 

# Ty Tuff and Bruno Vilela
# 23 August 2016
# Washington University in St. Loius


    
      path <- "~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/Module_2_outputs"
      
      setwd(path)
    myfiles_full <- list.dirs()
    analyze_this_many <- length(myfiles_full)
    
    available_files <- matrix(NA, 1, 1)
    
        
    for(i in 1: analyze_this_many){
    available_files <- rbind(available_files , as.matrix(list.files(myfiles_full[i], full.names = TRUE)))
    }
    dim(available_files)
    
    split.file.name <- strsplit(available_files[10], split = "_") 
    
    
    
 
available <- list.files()
files <- matrix(rep(NA, 62), length(available), 62)
dim(files)
i <- 10


for(i in 1:length(available)){
load(available[i])
name <- unlist(strsplit(available[i], split="_"))
files[i,] <- c(as.vector(matrix(name, 1,35)),matrix(Sim_statistics[[1]], 1, 27))

}


colnames(files) <-  c(

	rep(NA,3), 
	"replicate",
	rep(NA,4),
	"speciation_of_Env_NonD",
	"speciation_of_Env_D",
	"speciation_of_For",
	"speciation_of_Dom",
	NA,
	"extinction_of_Env_NonD",
	"extinction_of_Env_D",
	"extinction_of_For",
	"extinction_of_Dom",
	NA,
	"P.diffusion_Target_forager",
	"P.diffusion_Target_domesticator",
	"P.diffusion_Source_forager",
	"P.diffusion_Source_domesticator",
	NA,
	"P.takeover_Target_forager",
	"P.takeover_Target_domesticator",
	"P.takeover_Source_forager",
	"P.takeover_Source_domesticator",
	NA,
	"arisal_of_Env_NonD",
	"arisal_of_Env_D",
	"arisal_of_For",
	"arisal_of_Dom",
	
	NA, 
	"timestep", 
	NA,
        
    "number_of_branches",
	"Pylo_diversity_is_sum_of_BL",
	"average_phylogenetic_diversity_is_mean_of_BL",
	"variance_Pylo_diversity_is_variance_of_BL",

	"F_quadratic_entropy_is_sum_of_PD",
	"Mean_pairwise_distance",
	"variance_pairwise_distance",

	"Evolutionary_distinctiveness_sum",
	"mean_Phylogenetic_isolation",
	"variance_Phylogenetic_isolation",

	"gamma",
	"gamma_p_value",
	"speciation_rate",
	"extinction_rate",
	"extinction_per_speciation",
	"speciation_minus_extinction",
	"trait_1_speciation",
  	"trait_2_speciation" ,
  	"trait_1_extinction" ,
  	"trait_2_extinction" ,
  	"transition_from_trait_1_to_2" ,
  	"transition_from_trait_2_to_1" ,
  	"transition_rate_ratio_1to2_over_2to1" ,
  	"Phylogenetic_signal",
  	"spatial.tests.fora",
  	"spatial.tests.dom",
  	"prevalence"
  	
    
  )

files <- as.data.frame(files)
head(files)


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
ys[55,] <- c(0, 18500)
ys[56,] <- c(0, 23500)
ys[57,] <- c(0, 15000)
ys[58,] <- c(0, 50)
ys[59,] <- c(-0.2, 1.8)
ys[60,] <- c(0, 70)
ys[61,] <- c(0, 100)
ys[62,] <- c(-1, 1.1)


"speciation_of_Env_NonD",
	"speciation_of_Env_D",
	"speciation_of_For",
	"speciation_of_Dom",
	NA,
	"extinction_of_Env_NonD",
	"extinction_of_Env_D",
	"extinction_of_For",
	"extinction_of_Dom",
	NA,
	"P.diffusion_Target_forager",
	"P.diffusion_Target_domesticator",
	"P.diffusion_Source_forager",
	"P.diffusion_Source_domesticator",
	NA,
	"P.takeover_Target_forager",
	"P.takeover_Target_domesticator",
	"P.takeover_Source_forager",
	"P.takeover_Source_domesticator",
	NA,
	"arisal_of_Env_NonD",
	"arisal_of_Env_D",
	"arisal_of_For",
	"arisal_of_Dom",
	


parameter <- as.numeric(as.character(files$extinction_of_For))
par(mfrow=c(7,4), mar=c(2,2,3,0))
for(i in 36:62){
plot(parameter, as.numeric(as.character(files[,i])), main=colnames(files)[i], ylim= c(ys[i,1], ys[i,2]), type="p")
model<- lm(as.numeric(as.character(files[,i])) ~ parameter)
abline(model, col="red")
}
summary(model)
names(files)
stat <- as.numeric(as.character(files[,46]))
mean(stat, na.rm=TRUE)









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

