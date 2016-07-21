

######Read in R libraries##############################
#source("Plot_output.R")
library(ape)
library(phytools)
library(picante)

######Read in R functions##############################

 
# Required packages and functions
load.files <- list.files(path = "~/Box Sync/colliding ranges/Simulations_humans/Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}



###################################################
#combo_pass <- 25    #These are for testing the function. Do not use in actual model runs.
#analyze_this_many <- 4000  
#Timesteps_pass <- 300
#i <- 99

cluster_results_analysis <- function(combo_pass, analyze_this_many , Timesteps_pass) {

##### Load the results ########################
myfiles_full <- list.files("~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs", full.names = TRUE)

##### parse file names to retrieve simulation parameter info ########################
split.file.name <- strsplit(myfiles_full, split = "_")   #split file name everywhere there is and underscore


positions <- c(3, 6, 8:11, 13:16, 18:21, 23:26, 28:31, 34) #
data.result <- data.frame(matrix(ncol = 24, nrow = length(myfiles_full)))
colnames(data.result) <- c("File_path", "replicate", "combo",
                           "speciation_1", "speciation_2", "speciation_3", "speciation_4",
                           "extinction_1", "extinction_2", "extinction_3", "extinction_4",
                           "diffusion_1", "diffusion_2", "diffusion_3", "diffusion_4",
                           "takeover_1", "takeover_2", "takeover_3", "takeover_4",
                           "arisal_1", "arisal_2", "arisal_3", "arisal_4",
                           "Timesteps")
data.result[, 1] <- myfiles_full
head(data.result)

data.result.blank <- data.result # pass matrix to new object to be used seperatly below

for (i in 1:length(positions)) {
  data.result[, i + 1] <- sapply(split.file.name, "[", positions[i])
}

##### Subset matrix of file information to pull out the files we want to analyze together ########################
which(data.result$Timesteps == '300')
which(data.result$combo == '25')
cluster_input_files <- subset(data.result, combo == as.character(combo_pass) & Timesteps == as.character(Timesteps_pass) )
rownames(cluster_input_files) <- 1:length(cluster_input_files[,1])

##### assign each divided section of the file name to a column of a matrix ########################
if(analyze_this_many > length(cluster_input_files)){analyze_this_many <- length(cluster_input_files[,1])}
myfiles <- cluster_input_files[1:analyze_this_many, 1]  ## temporary parameter to start index vector
data.result <- cluster_input_files[1:analyze_this_many, ]
  
 ##### Reserve locations in the matrix for the analysis outputs ##################### 
  # Empty results
  l.myfiles <- length(myfiles)
  ## Data metrics
  difference <- rep(NA, l.myfiles)
  ## Spatial metrics
  spatial <- matrix(ncol = 3, nrow = l.myfiles)
  colnames(spatial) <- c("DF", "FF", "DD")
  ## Phylogenetic metrics
  signal <- rep(NA, l.myfiles)
  N.nodes <- rep(NA, l.myfiles)
  N.tips <- rep(NA, l.myfiles)
  gamma <- rep(NA, l.myfiles)
  Colless <- rep(NA, l.myfiles)
  KM <- rep(NA, l.myfiles)
  MS <- rep(NA, l.myfiles)
  TCI <- rep(NA, l.myfiles)
  Trasition.rates <- rep(NA, l.myfiles)
  weibull <- matrix(ncol = 2, nrow = l.myfiles)
  colnames(weibull) <- c("shape", "scale")
  
  
 ##### Load the file specified by each row for independent analysis ###################
#for (i in 1:l.myfiles) {
	
	all_trees <- vector("list",2)
	class(all_trees) <- "multiPhylo"
	
	for(i in 1:length(myfiles)){
    	if(file.exists(myfiles[i])){load(myfiles[i])}
    		
    	if(length(myOut) != 1){
  	 		all_trees[[i]] <- myOut$mytree
    	}
    }
    
##### Calculate each metric from the parameters provided by the file name and add them to the matrix  ###########
############################################################################################

##### (0) Pull necessary variables from simulated trees and organize into a single object for all the tests below to pull from.

	str(all_trees)

	## 0a) Branch lengths
	
		Pre_Branch_Lengths <- unlist(all_trees, use.names=TRUE, recursive=FALSE)
   		Branch_Lengths <- Pre_Branch_Lengths[which(names(Pre_Branch_Lengths) == "edge.length")]
   	
	## 0b) Pairwise distance between tips
	
		Pairwise_dist <- list(NULL)
	
		for(h in 1:length(myfiles)){
			Pairwise_dist <- c(Pairwise_dist,list(cophenetic(all_trees[[h]])))
			}
			
		Pairwise_dist <- Pairwise_dist[-1]
				
	## 0c) Phylogenetic isoloation
		
		Evolutionary_distinctiveness <- list(NULL)
	
		for(h in 1:length(myfiles)){
			try(Evolutionary_distinctiveness <- c(Evolutionary_distinctiveness, list(evol.distinct(all_trees[[h]], type="fair.proportion"))), silent=TRUE)
			}
			
		Evolutionary_distinctiveness <- Evolutionary_distinctiveness[-1]
		summary(Evolutionary_distinctiveness)
	
	## 0d) tree topology

## NOTE: need to deal with NAs here so we don't have to deal with them later with each function


##### (1) Spatial metrics ###################
#######################################



##### (2) Tree metric -- Richness - Sum ##################
##################################################


## 2a) Branch lengths -- Amount of evolutionary history

	## 2a.1 Across species
	
		# Anchor test = PD (Faith's phylogenetic diversity) 

			Pylo_diversity <- as.vector(unlist(lapply(Branch_Lengths, sum)))

	## 2a.2 Across individuals
		
		#delta nPD #IGNORED BECAUSE IT USES ABUNDANCE
		
	## 2a.3 Effective (q=0)
	
		#oD(T)*  #IGNORED BECAUSE IT USES ABUNDANCE
		
		#oPD(T)*  #IGNORED BECAUSE IT USES ABUNDANCE
	
	
	## 2a.4 Per species PDab
	
		#PE  #IGNORED BECAUSE IT USES RANGE SIZE 



## 2b) Pairwise distance -- Sum of pairwise distances


	# F -- Extensive quadratic entropy 
	
	F_quadratic_entropy <- as.vector(unlist(lapply(Pairwise_dist, sum)))


## 2c) Phylogenetic isolation -- Sum of evolutionary distinctiveness

	# ED - Summed evolutionary distinctiveness

		Phylogenetic_isolation <- as.vector(unlist(lapply(Evolutionary_distinctiveness, sum)))


##### (3) Tree metric -- Divergence - Mean ###############
##################################################




## 3a) Branch lengths 

	## 3a.1 -- Sum of branch lengths divided by species richness

		# avPD -- Average phylogenetic diversity
		
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
		
		# AvTD
		
		# PSV
		
		#interMPDab #IGNORED BECAUSE IT USES ABUNDANCE
		

## 3c) Pairwise distance/ similarities of nearest neighbors -- Mean shortest distance between a species and all others

	#MNTD
	
	#MNTDab #IGNORED BECAUSE IS USES ABUNDANCE
	
## 3d) Phylogenetic isolation -- Mean of species evolutionary distinctiveness

	# mean(ED)
	


##### (4) Tree metric -- Regularity - Variance ##############
##################################################



 
## 4a) Tree topology -- Branching symmetry and distribution

	#Ic -- Colles test
	
	#Iw - Fusco and Cronk 1995 suggested by Simon Greenhill
	
	#Gamma index
	
	#IAC

## 4b) Pairwise distance/all distances -- Variance of pairwise distances 

	# Anchor test = VPD (variation of pairwise distance)
	
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
	
	#Eed
	
	#Hed
	
	# Haed
	
	#qD(P)*
	
	#qD(AP)*
	
	


##### (5) Tree metric -- Macroevolutionary  ###############
##################################################


## Phylogenetic signal


## Speciation vs extinction rates


## Transistion rates (variable rates)


## Instantaneous rate or speciation and extinction from BAMM 





  for (i in 1:l.myfiles) {
#i <- 1
    if(file.exists(myfiles[i])){load(myfiles[i])}else{myOut <- NA}
    
    if (!is.na(myOut)[1]) {
      rem <- is.na(myOut$myWorld[, 6])
      myWorld <- myOut$myWorld[!rem, ]
      # Data metrics
      equals2 <- sum(myWorld[, 6] == 2)
      equals1 <- sum(myWorld[, 6] == 1)
      difference[i] <- equals1 - equals2
      
      # Tree metrics
      N.nodes[i] <- Nnode(myOut$mytree)
      N.tips[i] <- Ntip(myOut$mytree)
      gamma[i] <- ltt(myOut$mytree, plot = FALSE)$gamma
      Colless[i] <- colless(as.treeshape(myOut$mytree))
      MS[i] <- bd.ms(myOut$mytree)
      KM[i] <- bd.km(myOut$mytree)
      TCI[i] <- tci(myOut$mytree)

      weibull[i, ] <- fitdist(myOut$mytree$edge.length, "weibull")$estimate
      
      
      # Metrics that requires both states of the trait
      if (equals1 != 0 & equals2 != 0) {
        traits <- data.frame("trait" = myWorld[, 6], 
                             "tips" = paste0("t", myWorld[, 8]))
        compdata <- comparative.data(myOut$mytree, traits, tips)
        # Phylogenetic signal for binary traits (D of Fritz and Purvis 2010)
        signal[i] <- phylo.d(compdata, binvar = trait)$DEstimate
        # Spatial signal
        spatial[i, ] <- JoinCount(myWorld, repetitions = 100)
        # Trasition rates
        traits <- traits[match(myOut$mytree$tip.label, traits[, 2]), ]
        Trasition.rates[i] <- ace(x = traits[, 1], phy = myOut$mytree, type = "discrete")$rates
      }
    }
  }
  
  # Combine with the result data frame
  data.result$Difference <- difference
  data.result <- cbind(data.result, spatial)
  data.result$Phy_Signal <- signal
  data.result$N.nodes <- N.nodes
  data.result$N.tips <- N.tips
  data.result$Colless <- Colless
  data.result$gamma <- gamma
  data.result$MS <- MS
  data.result$KM <- KM
  data.result$TCI <- TCI


  data.result$Trasition.rates <- Trasition.rates
  data.result <- cbind(data.result, weibull)
  #return(data.result)
  
  save(data.result, file=paste0("results cluster output/", "Results_for_",combo_pass, "_" , "simulated_for_ ",Timesteps_pass , "_time_steps_analysis.R"))
}

#rm(data.result)
#load(paste0(combo_type, "_" , Timesteps_pass , "_analysis.R") )

#cluster_results_analysis(31, 10, 100)


a <- Sys.time()

library(gtools)
library(ape)
library(adephylo)
library(diversitree)
library(TotalCopheneticIndex)
library(phytools)
library(apTreeshape)
library(plyr)
library(fitdistrplus)
library(geiger)
library(caper)

library(parallel)

# Set up cluster
cl <- makeCluster(detectCores() , type = "PSOCK")

# Push resources out to cluster
clusterEvalQ(cl, library(gtools))
clusterEvalQ(cl, library(ape))
clusterEvalQ(cl, library(adephylo))
clusterEvalQ(cl, library(diversitree))
clusterEvalQ(cl, library( TotalCopheneticIndex))
clusterEvalQ(cl, library(phytools ))
clusterEvalQ(cl, library(apTreeshape ))
clusterEvalQ(cl, library( plyr))
clusterEvalQ(cl, library( fitdistrplus))
clusterEvalQ(cl, library(geiger))
clusterEvalQ(cl, library(caper))
clusterEvalQ(cl, library(spdep))
clusterEvalQ(cl, library(survival))
clusterEvalQ(cl, library(maps))
clusterEvalQ(cl, library(spdep))

setwd("~/Box Sync/colliding ranges/Simulations_humans")
clusterEvalQ(cl, source("Functions/Arisal_module.R"))
clusterEvalQ(cl, source("Functions/Auxiliary_functions.R"))
clusterEvalQ(cl, source("Functions/Build_world_function.R"))
clusterEvalQ(cl, source("Functions/Complete_Model.R"))
clusterEvalQ(cl, source("Functions/Diffusion_module.R"))
clusterEvalQ(cl, source("Functions/Extinction_module.R"))
clusterEvalQ(cl, source("Functions/Speciate_function.R"))
clusterEvalQ(cl, source("Functions/Speciation_function.R"))
clusterEvalQ(cl, source("Functions/Takeover_function.R"))
clusterEvalQ(cl, source("Functions/SpeciationTakeover_Module.R"))
clusterEvalQ(cl, source("Functions/Possible_combinations_of_movement_function.R"))
clusterEvalQ(cl, source("Functions/Ultimate_run_simulations.R"))
clusterEvalQ(cl, source("Functions/Plot_output.R"))
clusterEvalQ(cl, source("Functions/spatial_join.R"))

# lset are the landscapes that we will run

 setwd("~/Desktop")
combo_type <- c(25,28,29,31)


analyze_this_many <- 10000

b <- Sys.time()
#clusterApplyLB(cl, x = combo_type, fun = cluster_results_analysis, analyze_this_many = analyze_this_many,  Timesteps_pass = 100, nbs) 

c <- Sys.time()
clusterApplyLB(cl, x = combo_type, fun = cluster_results_analysis, analyze_this_many = analyze_this_many,  Timesteps_pass = 300) 

d <- Sys.time()
#clusterApplyLB(cl, x = combo_type, fun = cluster_results_analysis, analyze_this_many = analyze_this_many,  Timesteps_pass = 600, nbs) 

e <- Sys.time()
#clusterApplyLB(cl, x = combo_type, fun = cluster_results_analysis, analyze_this_many = analyze_this_many,  Timesteps_pass = 25, nbs) 

 f <- Sys.time()


difftime(b, a)
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

stopCluster(cl)

