setwd("~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/50000 timesteps with a file printed every 250 steps/model 31/Module_2_outputs")

available <- list.files()
files <- matrix(rep(NA, 60), length(available), 60)
dim(files)
i <- 10


for(i in 1:length(available)){
load(available[i])
name <- unlist(strsplit(available[i], split="_"))
files[i,] <- c(as.vector(matrix(name, 1,33)),matrix(Sim_statistics[[1]], 1, 27))

}


colnames(files) <-  c(rep(NA,3), "replicate",rep(NA,27), "timestep", NA,
        
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
attach(files)
pdf(file="~/Desktop/long_time_stats_plot_lines.pdf", width=8.5, height=11)
par(mfrow=c(7,4), mar=c(2,2,3,0))

ys <- matrix(c(0,5000), 60, 2)

ys[34,] <- c(2000,2600)
ys[35,] <- c(0, 200)
ys[36,] <- c(0, 0.08)
ys[37,] <- c(0, 0.02)
ys[38,] <- c(0, 5589260)
ys[39,] <- c(0, 3.5)
ys[40,] <- c(0, 3.5)
ys[41,] <- c(0, 200)
ys[42,] <- c(0, 0.16)
ys[43,] <- c(0, 0.015)
ys[44,] <- c(20, 60)
ys[45,] <- c(0, 5.3e-121)
ys[46,] <- c(0, 10000)
ys[47,] <- c(0, 10000)
ys[48,] <- c(0.9, 1)
ys[49,] <- c(0, 2)
ys[50,] <- c(0, 13500)
ys[51,] <- c(0, 13500)
ys[52,] <- c(0, 13500)
ys[53,] <- c(0, 13500)
ys[54,] <- c(0, 2500)
ys[55,] <- c(0, 2500)
ys[56,] <- c(0, 30)
ys[57,] <- c(-0.2, 0.8)
ys[58,] <- c(0, 100)
ys[59,] <- c(0, 100)
ys[60,] <- c(-1, 0.6)

i <- 56
max(as.numeric(as.character(files[,i])), na.rm=TRUE)


for(i in 34:60){
plot(as.numeric(files$timestep)*250, as.numeric(as.character(files[,i])), main=colnames(files)[i], ylim= c(ys[i,1], ys[i,2]), type="n")

for(j in unique(files$replicate)){
	subed <- subset(files, replicate==j)
lines(as.numeric(subed$timestep)*250, as.numeric(as.character(subed[,i])), type="l", col=adjustcolor(col=rainbow(100)[as.numeric(j)], alpha.f=.8), lwd=.1)
}
}

dev.off()



class(files[,i])

as.numeric(as.character(files[,i]))


