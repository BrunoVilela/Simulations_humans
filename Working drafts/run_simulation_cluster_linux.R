

#####################################################################

# Run the full model in a cluster. This version writes files to a cluster output folder.
rm(list = ls())



install.packages("~/Desktop/FARM_1.0.tar.gz", repos=NULL, type="source")


#####################################################################
## need to document which functions we use from each of these libraries. 
library(ape)
library(spdep)
library(parallel)
library(Rcpp)
library(msm)
library(FARM)

######Read in R functions##############################

setwd("~/Box Sync/colliding ranges/Simulations_humans")
# Required packages and functions
load.files <- list.files(path = "Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}




sim_run_cluster <- function(replicate_cycle, combo_number, myWorld, number_of_time_steps, nbs, number_of_tips ) {
  # Calls the full simulation script 
  #	 
  # Purpose: Need to wrap the entire simulation script into a function so it can be called in parallel from a cluster call 	
  #
  # Args:
  #    replicate_cycle: An integer indicating the replicate number of a simulation. This variable is used in this function to label        
  #			the saved output file and control the number of replicates run by the cluster.
  #
  #    combo_number: An interger between 1 and 31 indicating the combinations of S, E, A, D, and T modules to be included 
  #			in the simulation. The full list of these combinations can be printed using the function combo_of_choice(28, TRUE).
  # 		We are currently using combinations 25,28,29,and 31 as our four competing models for the spread of agriculture.  
  #
  #    myWorld: Matrix that defines the scope of the available world and acts as a data hub for organizing and reporting 	  
  #			results from the different elements of the simulation. 
  #
  #    number_of_time_steps: An integer indicating how many iterations the simulation will calculated before writing the data 
  #			file. 
  #
  #    nbs: A list of the available neighbors for each spatial point. This is passed to the function for calculating the interaction 
  #			of neighbors through time. 
  #
  #    number_of_tips: An interger indicating the number of tree tips the simulation should be truncated to. The default is to 
  #			include all the available tips (e.g. 1254 for human languages). 
  #
  # Returns: 
  #    myOut: A list object containing a 'phylo' tree object called mytree in the first position and the myWorld matrix of 
  #      	spatial and tree data in the second position 
  #		
  
  
  chosen_combo <- combo_of_choice(combo_number, FALSE)
  independent <- 1 # Always do independent, unless you the combo includes takeover dependent
  if (any(chosen_combo[[2]] == "Speciate")) {
    prob_choose <- as.numeric(formatC(rtnorm(1, mean = .5, sd =.05, lower = 0, upper = 1), width = 3,flag = 0, digits=2))  #prob speciation
    P.speciation <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "Env_NonD", "Env_D", "For", "Dom")
  } else {
    P.speciation <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (any(chosen_combo[[2]] == "Extinct")) {
    prob_choose <- as.numeric(formatC(rtnorm(1, mean = .05, sd =.05, lower = 0, upper = 1), width = 3,flag = 0, digits=2)) #prob of extinction
    P.extinction  <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "Env_NonD", "Env_D", "For", "Dom")     
  } else {
    P.extinction  <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  
  if (any(chosen_combo[[2]] == "Random_new_origin")) {
    prob_choose <- as.numeric(formatC(rtnorm(1, mean = .05, sd =.01, upper=1, lower=0), width = 3,flag = 0)) # prob of Arisal
    prob_choose_a <- prob_choose
    P.Arisal0  <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "Env_NonD", "Env_D", "Evol_to_F", "Evol_to_D")
    # P.Arisal0 is the one you should change the parameters
    P.Arisal <- matrix(NA, ncol = 2, nrow = nrow(myWorld)) # probability per cell
    colnames(P.Arisal) <- c("Evolve_to_F", "Evolve_to_D")
    Env.Dom <- myWorld[, 7] == 2
    P.Arisal[Env.Dom, 1] <- P.Arisal0[1, 2]
    P.Arisal[!Env.Dom, 1] <- P.Arisal0[1, 1]
    P.Arisal[Env.Dom, 2] <- P.Arisal0[2, 2]
    P.Arisal[!Env.Dom, 2] <- P.Arisal0[2, 1]
    
  } else {
    P.Arisal <- matrix(0, ncol = 2, nrow = nrow(myWorld))
    prob_choose_a <- 0
  }
  colnames(P.Arisal) <- c("Prob_of_Foraging", "Porb_of_Domestication")
  
  
  if (any(chosen_combo[[2]] == "Diffusion")) {
    prob_choose <- as.numeric(formatC(rtnorm(1, mean = .2, sd =.2, upper=1, lower=0.05), width = 3,flag = 0, digits=2)) #prob of diffusion
    P.diffusion <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "Target_For", "Target_Dom", "Source_For", "Source_Dom")
    diag(P.diffusion)<- NA
  } else {
    P.diffusion <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (any(chosen_combo[[2]] == "Takeover")) {
    prob_choose <- as.numeric(formatC(rtnorm(1, mean = .2, sd =.2, upper=1, lower=0.05), width = 3,flag = 0, digits=2)) #prob of takeover
    P.TakeOver <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "Target_For", "Target_Dom", "Source_For", "Source_Dom")
    independent <- rtnorm(1, mean = .5, sd = .1, upper = .7, lower = .3)
  } else {
    prob_choose <- as.numeric(formatC(rtnorm(1, mean = .2, sd =.2, upper=1, lower=0.05), width = 3,flag = 0, digits=2)) #prob of takeover
    P.TakeOver <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "Target_For", "Target_Dom", "Source_For", "Source_Dom")
  }
  
  multiplier <- rtnorm(1, mean = 2, sd = .5, upper = 4, lower = 1)
  myOut <- RunSimUltimate(myWorld, P.extinction, P.speciation, 
                          P.diffusion, P.Arisal, P.TakeOver, nbs, independent,
                          N.steps = number_of_time_steps, silent = F, 
                          multiplier = multiplier)

  save(myOut, file= paste0("./Module_1_outputs/myOut_replicate_", 
                           formatC(replicate_cycle, width = 2,flag = 0),
                           "_combination_",
                           formatC(combo_number, width = 2,flag = 0),
                           "_","parameters", "_P.speciation_",
                           paste(P.speciation, collapse="_"), "_P.extinction_",
                           paste(P.extinction, collapse="_"), "_P.diffusion_",
                           paste(P.diffusion, collapse="_"), "_P.TakeOver_",
                           paste(P.TakeOver, collapse="_"),"_P.Arisal_",
                           prob_choose_a,
                           "_timesteps_", number_of_time_steps, "_.Rdata"))
  
  Sim_statistics <- Module_2(myOut)
  
  save(Sim_statistics, file= paste0("./Module_2_outputs/Sim_statistics_replicate_", 
                           formatC(replicate_cycle, width = 2,flag = 0),
                           "_combination_",
                           formatC(combo_number, width = 2,flag = 0),
                           "_","parameters", "_P.speciation_",
                           paste(P.speciation, collapse="_"), "_P.extinction_",
                           paste(P.extinction, collapse="_"), "_P.diffusion_",
                           paste(P.diffusion, collapse="_"), "_P.TakeOver_",
                           paste(P.TakeOver, collapse="_"),"_P.Arisal_",
                           prob_choose_a,
                           "_timesteps_", number_of_time_steps, "_.Rdata"))
  
}




#####################################################################
coords <- coords
conds <- suitability
conds <- ifelse(conds <= 21, 1, 2)
conds[is.na(conds)] <- sample(c(1, 2), sum(is.na(conds)), replace = TRUE) 


##### Specify simulation parameters #################################

number_of_tips <- length(coords[,1])
number_of_time_steps_a <- 5000
#replicate_cycle <- c(1)  #number of replicates
#####################################################################


sub <- sample(1:nrow(coords), 1253) # subsample (remove when running for all)
system.time(
  myWorld <- BuildWorld(coords[sub, ], conds[sub, ])
)
nbs <- knn2nb(knearneigh(coords[sub, ], k = 7, longlat = TRUE),
              sym = TRUE) # 7 symmetric neighbors
n.obs <- sapply(nbs, length)
seq.max <- seq_len(max(n.obs))
nbs <- t(sapply(nbs, "[", i = seq.max))

dim(myWorld)


args <- commandArgs(trailingOnly = FALSE)

NAI <- 1

#NAI <- as.numeric(args[7])
setwd("~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs")

sim_run_cluster(replicate_cycle = NAI, 
                 combo_number = 31,
                 myWorld, number_of_time_steps = number_of_time_steps_a, 
                 nbs, number_of_tips )
