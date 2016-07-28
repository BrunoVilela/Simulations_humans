
####
setwd("~/Box Sync/colliding ranges/Simulations_humans")

######Read in R libraries##############################
#source("Plot_output.R")
library(ape)
library(phytools)
library(picante)
library(apTreeshape)
library(caper)

######Read in R functions##############################
start_time <- Sys.time()

# Required packages and functions
load.files <- list.files(path = "Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}

load_functions <- Sys.time()

###################################################
combo_pass <- 25    #These are for testing the function. Do not use in actual model runs.
analyze_this_many <- 4000  
Timesteps_pass <- 5000
#i <- 99

cluster_results_analysis <- function(combo_pass, analyze_this_many , Timesteps_pass, line_color) {
  
  start_functions <- Sys.time()
  
  ##### Load the results ########################
  myfiles_full <- list.files("big world cluster outputs/5000 timesteps",
                             full.names = TRUE)
  
  ##### parse file names to retrieve simulation parameter info ########################
  split.file.name <- strsplit(myfiles_full, split = "_")   #split file name everywhere there is and underscore
  
  
  positions <- c(3, 5, 8:11, 13:16, 18:21, 23:26, 28, 30) # 
  data.result <- data.frame(matrix(ncol = 21, nrow = length(myfiles_full)))
  colnames(data.result) <- c("File_path", "replicate", "combo",
                             "speciation_1", "speciation_2", "speciation_3", "speciation_4",
                             "extinction_1", "extinction_2", "extinction_3", "extinction_4",
                             "diffusion_1", "diffusion_2", "diffusion_3", "diffusion_4",
                             "takeover_1", "takeover_2", "takeover_3", "takeover_4",
                             "arisal_1",  "Timesteps")
  data.result[, 1] <- myfiles_full
  #head(data.result)
  
  data.result.blank <- data.result # pass matrix to new object to be used seperatly below
  
  for (i in 1:length(positions)) {
    data.result[, i + 1] <- sapply(split.file.name, "[", positions[i])
  }
  
  ##### Subset matrix of file information to pull out the files we want to analyze together ########################
  cluster_input_files <- subset(data.result, combo == as.character(combo_pass) &
                                  Timesteps == as.character(Timesteps_pass))
  n.cluster <- nrow(cluster_input_files)
  rownames(cluster_input_files) <- 1:n.cluster
  
  ##### assign each divided section of the file name to a column of a matrix ########################
  if(analyze_this_many > n.cluster) {
    analyze_this_many <- n.cluster
  }
  sub <- 1:analyze_this_many
  myfiles <- cluster_input_files[sub, 1]  ## temporary parameter to start index vector
  data.result <- cluster_input_files[sub, ]
  
  parse_file_names <- Sys.time()  
  
  
  # some of these are different lengths. Need to make a list or fix otherwise.
  pos <- 25:41
  data.result[, pos] <- rep(NA, analyze_this_many)
  colnames(data.result)[pos] <- c("Branch_Lengths", 
                                  "Pairwise_dist", 
                                  "Evolutionary_distinctiveness", 
                                  "Pylo_diversity",
                                  "F_quadratic_entropy",
                                  "Phylogenetic_isolation",
                                  "Average_Pylo_diversity",
                                  "Mean_pairwise_distance",
                                  "mean_Phylogenetic_isolation",
                                  "variance_Pylo_diversity",
                                  "Ic",
                                  "lineages_through_time",
                                  "time_steps",
                                  "gamma",
                                  "gamma_p_value",
                                  "variance_pairwise_distance",
                                  "variance_Phylogenetic_isolation")
  
  ##### Load the file specified by each row for independent analysis ###################
  #for (i in 1:l.myfiles) {
  
  all_trees <- vector("list", 2)
  class(all_trees) <- "multiPhylo"
  for(i in 1:analyze_this_many) {
    if (file.exists(myfiles[i])) {
      load(myfiles[i])
    }
    if (any(!is.na(myOut))) {
      all_trees[[i]] <- myOut$mytree
    }
  }
  
  # Subset the trees
  keep <- !sapply(all_trees, is.null)
  all_trees <- all_trees[keep]
  
  # Number of world extinctions
  extinctions <- sum(!keep)
  
  
  load_files <- Sys.time()
  ##### Calculate each metric from the parameters provided by the file name and add them to the matrix  ###########
  ############################################################################################
  
  ##### (0) Pull necessary variables from simulated trees and organize into a single object for all the tests below to pull from.
  
  #str(all_trees)
  
  ## 0a) Branch lengths
  Branch_Lengths <- lapply(all_trees, "[[", 4)
  
  extract_branch_length <- Sys.time()
  
  ## 0b) Pairwise distance between tips
  
  Pairwise_dist <- lapply(all_trees, cophenetic)  
  
  calc_pairwise_dist <- Sys.time()	
  
  ## 0c) Phylogenetic isolation
  
  # Using equal.splits method, faster computation
  Evolutionary_distinctiveness <- lapply(all_trees, evol.distinct2, 
                                         type = "equal.splits") 
  
  calc_evolutionary_distinctiveness <- Sys.time()
  
  ## 0d) tree topology
  
  all_trees_as_treeshape <- lapply(all_trees, as.treeshape)
  
  
  ##### (1) Spatial metrics ###################
  #######################################
  
  
  calc_spatial_metrics <- Sys.time()
  
  ##### (2) Tree metric -- Richness - Sum ##################
  ##################################################
  
  
  ## 2a) Branch lengths -- Amount of evolutionary history
  
  ## 2a.1 Across species
  
  # Anchor test = PD (Faith's phylogenetic diversity) 
  
  Pylo_diversity <- sapply(Branch_Lengths, sum)
  
  ## 2a.2 Across individuals
  
  #delta nPD #IGNORED BECAUSE IT USES ABUNDANCE
  
  ## 2a.3 Effective (q=0)
  
  #oD(T)*  #IGNORED BECAUSE IT USES ABUNDANCE
  
  #oPD(T)*  #IGNORED BECAUSE IT USES ABUNDANCE
  
  
  ## 2a.4 Per species PDab
  
  #PE  #IGNORED BECAUSE IT USES RANGE SIZE 
  
  
  
  ## 2b) Pairwise distance -- Sum of pairwise distances
  
  
  # F -- Extensive quadratic entropy 
  
  F_quadratic_entropy <- sapply(Pairwise_dist, sum)
  
  
  ## 2c) Phylogenetic isolation -- Sum of evolutionary distinctiveness
  
  # ED - Summed evolutionary distinctiveness
  
  Evolutionary_distinctiveness_sum <- sapply(Evolutionary_distinctiveness, sum)
  
  
  calc_richness_metrics <- Sys.time()
  
  ##### (3) Tree metric -- Divergence - Mean ###############
  ##################################################
  
  
  
  
  ## 3a) Branch lengths 
  
  ## 3a.1 -- Sum of branch lengths divided by species richness
  
  # avPD -- Average phylogenetic diversity
  
  average_phylogenetic_diversity <- sapply(Branch_Lengths, mean)
  
  # avPDab #IGNORED BECAUSE IT USES ABUNDANCE
  
  
  ## 3a.2 -- Effective number of species given phylogenetic balance and abundance evenness (q>0)
  
  #qD(t)*
  
  
  #qPD(T)*
  
  
  ## 3a.3 -- associated entropies
  
  #Hp*
  
  
  #Lq*
  
  ## 3b) Pairwise distance/ similarities of all
  
  ## 3b.1 -- Effective number of species given phylogenetic balance and abundance evenness 
  
  #qD^z(p)*
  
  ## 3b.2 -- Mean of all distances including zero intra-species distances
  
  #Rao's QE
  
  #MPDab
  
  # PSE
  
  #J
  
  ## 3b.3 -- Mean inter-species distances
  
  # Anchor test = MPD (mean pairwise distance)
  
  Mean_pairwise_distance <- sapply(Pairwise_dist, mean)
  
  # AvTD
  
  # PSV
  
  #interMPDab #IGNORED BECAUSE IT USES ABUNDANCE
  
  
  ## 3c) Pairwise distance/ similarities of nearest neighbors -- Mean shortest distance between a species and all others
  
  #MNTD
  
  #MNTDab #IGNORED BECAUSE IS USES ABUNDANCE
  
  ## 3d) Phylogenetic isolation -- Mean of species evolutionary distinctiveness
  
  # mean(ED)
  
  mean_Phylogenetic_isolation <- sapply(Evolutionary_distinctiveness, mean)
  
  
  
  calc_divergence_metrics <- Sys.time()
  
  ##### (4) Tree metric -- Regularity - Variance ##############
  ##################################################
  
  variance_Pylo_diversity <- sapply(Branch_Lengths, var)
  
  
  
  ## 4a) Tree topology -- Branching symmetry and distribution
  
  #Ic -- Colles test
  
  Ic <- sapply(all_trees_as_treeshape, colless, norm = NULL)
  
  #Iw - Fusco and Cronk 1995 suggested by Simon Greenhill
  
  plot((fusco.test(all_trees[[14]], tipsAsSpecies=TRUE)))
  fusco_out <- (fusco.test(all_trees[[4]], tipsAsSpecies=TRUE))
  summary(fusco_out)
  hist(fusco_out[1,])
  
  hist(fusco_out$observed$I)
  hist(fusco_out$observed$I.prime)
  hist(fusco_out$observed$I.w)
  
  
  
  #Gamma index
  
  ltts <- lapply(all_trees, ltt, gamma = TRUE, plot = FALSE)
  lineages_through_time <- sapply(ltts, "[[", 1)
  time_steps <- sapply(ltts, "[[", 2)
  gamma <- sapply(ltts, "[[", 3)
  gamma_p_value <- sapply(ltts, "[[", 4)
  
  #log(gamma_list[[k]]$ltt)
  
  #str(gamma_list)
  #ltt.custom(gamma_list[[k]]$tree)
  #branching.times(gamma_list[[k]]$tree)
  
  # str(gamma_list)
  #class(all_trees)
  
  #IAC
  
  ## 4b) Pairwise distance/all distances -- Variance of pairwise distances 
  
  # Anchor test = VPD (variation of pairwise distance)
  
  variance_pairwise_distance <- sapply(Pairwise_dist, function(x) var(as.numeric(x)))
  
  # A+
  
  # VarTD
  
  # VPDab #IGNORED BECAUSE IT USES ABUNDANCE
  
  # interVPDab #IGNORED BECAUSE IT USES ABUNDANCE
  
  
  ## 4c) Pairwise distance/nearest neighbor -- Variance of nearest neighbour distance
  
  # VNTD
  
  # VNTDab #IGNORED BECAUSE IT USES ABUNDANCE
  
  # PEve
  
  ## 4d) Phylogenetic isolation -- Variance of species isolation metrics
  
  #var(ED)
  
  variance_Phylogenetic_isolation <- sapply(Evolutionary_distinctiveness, var)
  
  
  #Eed
  
  #Hed
  
  # Haed
  
  #qD(P)*
  
  #qD(AP)*
  
  
  calc_regularity_metrics <- Sys.time()
  
  ##### (5) Tree metric -- Macroevolutionary - Rate and rate changes ###############
  ##################################################
  
  
  ## Phylogenetic signal
  
  
  ## Speciation vs extinction rates
  
  
  ## Transistion rates (variable rates)
  
  
  ## Instantaneous rate or speciation and extinction from BAMM 
  
  
  
  calc_macroevolution_metrics <- Sys.time()
  
  
  # save(data.result, file=paste0("results cluster output/", "Results_for_",combo_pass, "_" , "simulated_for_ ",Timesteps_pass , "_time_steps_analysis.R"))
  
  
  save_time <- Sys.time()
  
  ### Calculate and return time stamps
  time_vect <- c(start_time, load_functions, start_functions, parse_file_names, load_files, extract_branch_length, calc_pairwise_dist, calc_evolutionary_distinctiveness, calc_spatial_metrics, calc_richness_metrics, calc_divergence_metrics, calc_regularity_metrics, calc_macroevolution_metrics, save_time)
  calc_times <- as.data.frame(difftime(time_vect[-1], time_vect[-(length(time_vect))], ))
  rownames(calc_times) <-  c("load R functions", "start results calculation function", "parse file names", "load files from simulation", "extract branch lengths", "calculate pairwise distance", "calculate evolutionary distinctiveness", "calculate spatial metrics", "calculate richness metrics", "calculate divergence metrics", "calculate regularity metrics", "calculate macroevolution metrics", "save output file")
  # calc_times
  return(list(calc_times,  lineages_through_time , time_steps , gamma , gamma_p_value))
  
}

#rm(data.result)
#load(paste0(combo_type, "_" , Timesteps_pass , "_analysis.R") )

## This section is just for making plots for texting and understanding metrics
##############################################################
a <- cluster_results_analysis(31, 10, 300, 1)
b <-cluster_results_analysis(29, 10, 300, 2)
c <-cluster_results_analysis(28, 10, 300, 3)
d <-cluster_results_analysis(25, 10, 300, 4)



#setwd("~/Desktop")
pdf(file="Figures/time through lineage plot.pdf", width=11, height=8.5)
color_choice <- c("firebrick", "cornflowerblue", "limegreen", "grey")

plot(0,0, type="n", xlim=c(0,8), ylim=c(0,300), xlab="log(number of tips)", ylab="time between tips")

for(h in 1:length(a[[3]][1,])){
  lines(a[[2]][,h], a[[3]][,h], col=adjustcolor(color_choice[1], alpha=.8))
  
}

for(h in 1:length(b[[3]][1,])){
  lines(b[[2]][,h], b[[3]][,h], col=adjustcolor(color_choice[2], alpha=.8))
  
}

for(h in 1:length(c[[3]][1,])){
  lines(c[[2]][,h], c[[3]][,h], col=adjustcolor(color_choice[3], alpha=.8))
  
}

for(h in 1:length(d[[3]][1,])){
  lines(d[[2]][,h], d[[3]][,h], col=adjustcolor(color_choice[4], alpha=.8))
  
}
dev.off()

h <- 5


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


pdf(file="Figures/polytest.pdf")
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


dev.off()


####End of plot test code -- to be moved to dashboard plot script


aaaa <- Sys.time()


#library(parallel)

# Set up cluster
#cl <- makeCluster(detectCores() , type = "PSOCK")

# Push resources out to cluster
#clusterEvalQ(cl, library(gtools))
#clusterEvalQ(cl, library(ape))
#clusterEvalQ(cl, library(adephylo))
#clusterEvalQ(cl, library(diversitree))
#clusterEvalQ(cl, library( TotalCopheneticIndex))
#clusterEvalQ(cl, library(phytools ))
#clusterEvalQ(cl, library(apTreeshape ))
#clusterEvalQ(cl, library( plyr))
#clusterEvalQ(cl, library( fitdistrplus))
#clusterEvalQ(cl, library(geiger))
#clusterEvalQ(cl, library(caper))
#clusterEvalQ(cl, library(spdep))
#clusterEvalQ(cl, library(survival))
#clusterEvalQ(cl, library(maps))
#clusterEvalQ(cl, library(spdep))

#setwd("~/Box Sync/colliding ranges/Simulations_humans")
#clusterEvalQ(cl, source("Functions/Arisal_module.R"))
#clusterEvalQ(cl, source("Functions/Auxiliary_functions.R"))
#clusterEvalQ(cl, source("Functions/Build_world_function.R"))
#clusterEvalQ(cl, source("Functions/Complete_Model.R"))
#clusterEvalQ(cl, source("Functions/Diffusion_module.R"))
#clusterEvalQ(cl, source("Functions/Extinction_module.R"))
#clusterEvalQ(cl, source("Functions/Speciate_function.R"))
#clusterEvalQ(cl, source("Functions/Speciation_function.R"))
#clusterEvalQ(cl, source("Functions/Takeover_function.R"))
#clusterEvalQ(cl, source("Functions/SpeciationTakeover_Module.R"))
#clusterEvalQ(cl, source("Functions/Possible_combinations_of_movement_function.R"))
#clusterEvalQ(cl, source("Functions/Ultimate_run_simulations.R"))
#clusterEvalQ(cl, source("Functions/Plot_output.R"))
#clusterEvalQ(cl, source("Functions/spatial_join.R"))

# lset are the landscapes that we will run

# setwd("~/Desktop")
combo_type <- c(25,28,29,31)


analyze_this_many <- 10000

b <- Sys.time()
#clusterApplyLB(cl, x = combo_type, fun = cluster_results_analysis, analyze_this_many = analyze_this_many,  Timesteps_pass = 100, nbs) 

c <- Sys.time()
#clusterApplyLB(cl, x = combo_type, fun = cluster_results_analysis, analyze_this_many = analyze_this_many,  Timesteps_pass = 300) 

d <- Sys.time()
#clusterApplyLB(cl, x = combo_type, fun = cluster_results_analysis, analyze_this_many = analyze_this_many,  Timesteps_pass = 600, nbs) 

e <- Sys.time()
#clusterApplyLB(cl, x = combo_type, fun = cluster_results_analysis, analyze_this_many = analyze_this_many,  Timesteps_pass = 25, nbs) 

f <- Sys.time()


difftime(b, aaaa)
# Time to load packages

difftime(c, b)
# Time to run combo 31

difftime(d, c)
# Time to run combo 29

difftime(e, d)
# Time to run combo 28

difftime(f, e)
# Time to run combo 25

difftime(f, a)
# Total time

#stopCluster(cl)

