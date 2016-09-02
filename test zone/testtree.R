

#####################################################################

# Run the full model in a cluster. This version writes files to a cluster output folder.
rm(list = ls())
# install.packages("~/Desktop/FARM_1.0.tar.gz", repos=NULL, type="source")


#####################################################################
## need to document which functions we use from each of these libraries. 
library(ape)
library(spdep)
library(Rcpp)
library(msm)
library(FARM)


sim_run_cluster <- function(replicate_cycle, myWorld, number_of_time_steps, nbs,
                            number_of_tips, parameters.table) {
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
  
  independent <- 0 # Never do independent, unless you the combo includes takeover dependent
  
  
  # Probability of Arisal
  prob_choose_a <- rexp(4, rate = 9)
  # prob_choose_a <- rep(0, 4)
  
  P.Arisal0  <- parameters(prob_choose_a[1], prob_choose_a[2],
                           prob_choose_a[3], prob_choose_a[4],
                           "Env_NonD", "Env_D",
                           "Evol_to_F", "Evol_to_D")
  # P.Arisal0 is the one you should change the parameters
  P.Arisal <- matrix(NA, ncol = 2, nrow = nrow(myWorld)) # probability per cell
  colnames(P.Arisal) <- c("Evolve_to_F", "Evolve_to_D")
  Env.Dom <- myWorld[, 7] == 2
  P.Arisal[Env.Dom, 1] <- P.Arisal0[1, 2]
  P.Arisal[!Env.Dom, 1] <- P.Arisal0[1, 1]
  P.Arisal[Env.Dom, 2] <- P.Arisal0[2, 2]
  P.Arisal[!Env.Dom, 2] <- P.Arisal0[2, 1]
  
  colnames(P.Arisal) <- c("Prob_of_Foraging", "Porb_of_Domestication")
  #####
  
  
  # Other parameters
  prob_choose <- as.numeric(parameters.table[replicate_cycle, ])
  P.speciation <- parameters(prob_choose[1], prob_choose[1],
                             prob_choose[2], prob_choose[3],
                             "Env_NonD", "Env_D", "For", "Dom")
  
  P.extinction  <- parameters(prob_choose[4], prob_choose[4],
                              prob_choose[5], prob_choose[6],
                              "Env_NonD", "Env_D", "For", "Dom")
  P.diffusion <- parameters(0, prob_choose[7],
                            prob_choose[8], 0,
                            "Target_For", "Target_Dom",
                            "Source_For", "Source_Dom")
  
  P.TakeOver <- parameters(prob_choose[9], prob_choose[10],
                           prob_choose[11], prob_choose[12],
                           "Target_For", "Target_Dom",
                           "Source_For", "Source_Dom")
  multiplier <- 1 # always 1 now.
  
  myOut <- RunSimUltimate(myWorld, P.extinction, P.speciation, 
                          P.diffusion, P.Arisal, P.TakeOver, nbs, independent,
                          N.steps = number_of_time_steps, silent = F, 
                          multiplier = multiplier)
  
  Sim_statistics <- Module_2(myOut)
  return(list(myOut, Sim_statistics))
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
data("parameters.table")


sub <- sample(1:nrow(coords), nrow(coords)) # subsample (remove when running for all)
system.time(
  myWorld <- BuildWorld(coords[sub, ], conds[sub, ])
)
nbs <- knn2nb(knearneigh(coords[sub, ], k = 7, longlat = TRUE),
              sym = TRUE) # 7 symmetric neighbors
n.obs <- sapply(nbs, length)
seq.max <- seq_len(max(n.obs))
nbs <- t(sapply(nbs, "[", i = seq.max))

dim(myWorld)



NAI <- 1
# args <- commandArgs(trailingOnly = FALSE)
# NAI <- as.numeric(args[7])
# setwd("~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs")
parameters.table[1, ] <- c(.3, .01, .3, .1, .8, .1, .1, .1, .2, .005, .7, .3)
test.tree <- sim_run_cluster(replicate_cycle = NAI,
                             myWorld, number_of_time_steps = 30000, 
                             nbs, number_of_tips = nrow(myWorld),
                             parameters.table = parameters.table)
test.tree[[2]]$results_summary_of_single_value_outputs

# Adjusting the parameters
library(diversitree)
myWorld = test.tree[[1]]$myWorld
mytree = test.tree[[1]]$mytree
traits <- setNames(myWorld[, 6], myWorld[, 8])
pos <- na.omit(traits == 2)
traits[pos] <- traits[pos] + myWorld[pos, 7] - 1
musse <- make.musse(mytree, traits, 3)
p <- starting.point.musse(mytree, k = 3)
fit.musse <- find.mle(musse, x.init = p[argnames(musse)])

spec <- fit.musse$par[1:3]
spec <- spec/max(spec)
ext <- fit.musse$par[4:6]
ext <- ext/max(ext)
x

test.tree[[1]]$myWorld
test.tree[[2]]$results_summary_of_single_value_outputs
test.tree <- list(test.tree, parameters.table[1, ], c(spec, ext))
save(test.tree, file = "test.tree")