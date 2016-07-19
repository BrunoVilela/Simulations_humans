setwd("~/Desktop")

# Run the full model in a cluster. This version writes files to a cluster output folder.
rm(list = ls())
load.files <- list.files(path = "Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}

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
library(msm)
library(spdep)

coords <- as.matrix(read.csv("Functions/coords.csv", row.names = 1))
conds <- as.matrix(read.csv("Functions/suitability.csv", row.names = 1))
conds <- ifelse(conds <= 21, 1, 2)
conds[is.na(conds)] <- sample(c(1, 2), sum(is.na(conds)), replace = TRUE) 
sub <- sample(1:nrow(coords), 200) # subsample (remove when running for all)
system.time(
myWorld <- BuildWorld(coords[sub, ], conds[sub, ])
)
nbs <- knn2nb(knearneigh(coords[sub, ], k = 7, longlat = TRUE),
              sym = TRUE) # 7 symmetric neighbors


dim(myWorld)

number_of_time_steps <- 100
replicate_cycle <- 3
combo_number <- 31

sim_run_cluster <- function(replicate_cycle, combo_number, myWorld, number_of_time_steps, nbs) {
  
  chosen_combo <- combo_of_choice(combo_number, FALSE)
  
  if (any(chosen_combo[[2]] == "Speciate")) {
  	prob_choose <- as.numeric(formatC(rtnorm(1, mean = .2, sd =.05, lower = 0, upper = 1), width = 3,flag = 0, digits=2))  #prob speciation
    P.speciation <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom")
  } else {
    P.speciation <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (any(chosen_combo[[2]] == "Extinct")) {
  	prob_choose <- as.numeric(formatC(rtnorm(1, mean = .05, sd =.05, lower = 0, upper = 1), width = 3,flag = 0, digits=2)) #prob of extinction
    P.extinction  <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom")
    P.extinction["For", "Dom"] <- 0.4
  } else {
    P.extinction  <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (any(chosen_combo[[2]] == "Diffusion")) {
  	prob_choose <- as.numeric(formatC(rtnorm(1, mean = .2, sd =.2, upper=1, lower=0.05), width = 3,flag = 0, digits=2)) #prob of diffusion
    P.diffusion <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom") 
  } else {
    P.diffusion <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (any(chosen_combo[[2]] == "Takeover")) {
  	prob_choose <- as.numeric(formatC(rtnorm(1, mean = .2, sd =.2, upper=1, lower=0.05), width = 3,flag = 0, digits=2)) #prob of takeover
    P.TakeOver <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom")
  } else {
    P.TakeOver <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (any(chosen_combo[[2]] == "Random_new_origin")) {
  	prob_choose <- as.numeric(formatC(rtnorm(1, mean = .05, sd =.01, upper=1, lower=0), width = 3,flag = 0)) # prob of Arisal
    P.Arisal <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom") 
  } else {
    P.Arisal <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  myOut <- RunSimUltimate(myWorld, P.extinction, P.speciation, 
                          P.diffusion, P.Arisal, P.TakeOver, nbs,
                          N.steps = number_of_time_steps, silent = TRUE, 
                          multiplier = 2)
  

 save(myOut, file= paste0("big world cluster outputs/myOut_replicate_", 
                          formatC(replicate_cycle, width = 2,flag = 0),
                          "_combination_",
                          formatC(combo_number, width = 2,flag = 0),
                          "_","parameters", "_P.speciation_",
                          paste(P.speciation, collapse="_"), "_P.extinction_",
                          paste(P.extinction, collapse="_"), "_P.diffusion_",
                          paste(P.diffusion, collapse="_"), "_P.TakeOver_",
                          paste(P.TakeOver, collapse="_"),"_P.Arisal_",
                          paste(P.Arisal, collapse="_"),
                          "_timesteps_", number_of_time_steps, ".Rdata"))

}

#sim_run_cluster(1, 31, myWorld, 100, nbs)

map()
plot(nbs, coords[sub, ], add = TRUE, col = "gray80", lty = 3)
points(coords[sub, ], col = c("blue", "red")[conds[sub, ]])
points(coords[sub, ], col = c("blue", "red")[myOut$myWorld[, 6]], pch = 20)


a <- Sys.time()
library(parallel)


# Set up cluster
cl <- makeCluster(detectCores() , type = "PSOCK")

# Push resources out to cluster'
clusterEvalQ(cl, library(msm))
clusterEvalQ(cl, library(gtools))
clusterEvalQ(cl, library(ape))
clusterEvalQ(cl, library(adephylo))
clusterEvalQ(cl, library(diversitree))
clusterEvalQ(cl, source("Functions/Arisal_module.R"))
clusterEvalQ(cl, source("Functions/Auxiliary_functions.R"))
clusterEvalQ(cl, source("Functions/Build_world_function.R"))
clusterEvalQ(cl, source("Functions/Complete_Model.R"))
clusterEvalQ(cl, source("Functions/Diffusion_module.R"))
clusterEvalQ(cl, source("Functions/Extinction_module.R"))
clusterEvalQ(cl, source("Functions/Possible_combinations_of_movement_function.R"))
clusterEvalQ(cl, source("Functions/spatial_join.R"))
clusterEvalQ(cl, source("Functions/Speciate_function.R"))
clusterEvalQ(cl, source("Functions/Speciation_function.R"))
clusterEvalQ(cl, source("Functions/SpeciationTakeover_Module.R"))
clusterEvalQ(cl, source("Functions/Takeover_function.R"))
clusterEvalQ(cl, source("Functions/Ultimate_run_simulations.R"))


# lset are the landscapes that we will run
b <- Sys.time()
replicate_cycle <- c(1:15)
number_of_time_steps_a <- 300
#number_of_time_steps_b <- 300


clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 31, number_of_time_steps = number_of_time_steps_a,
               myWorld = myWorld, nbs=nbs) 

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 29, number_of_time_steps = number_of_time_steps_a,
               myWorld = myWorld, nbs=nbs) 
               
clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 28, number_of_time_steps = number_of_time_steps_a,
               myWorld = myWorld, nbs=nbs) 

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 25, number_of_time_steps = number_of_time_steps_a,
               myWorld = myWorld, nbs=nbs) 
             
c <- Sys.time()



difftime(b, a)
# Time to load packages

difftime(c, b)
# Time to run 



stopCluster(cl)

