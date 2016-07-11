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


system.time(
myWorld <- BuildWorld(R = 10, P = 0.5)
)
dim(myWorld)


number_of_time_steps <- 100
replicate_cycle <- 3
combo_number <- 31

sim_run_cluster <- function(replicate_cycle, combo_number, myWorld, number_of_time_steps) {
  
  chosen_combo <- combo_of_choice(combo_number, FALSE)
  
  if (any(chosen_combo[[2]] == "Speciate")) {
  	prob_choose <- as.numeric(formatC(rnorm(1, mean = .2, sd =.1), width = 3,flag = 0, digits=2))  #prob speciation
    P.speciation <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom")
  } else {
    P.speciation <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (any(chosen_combo[[2]] == "Extinct")) {
  	prob_choose <- as.numeric(formatC(rtnorm(1, mean = .1, sd =.3, lower=0, upper=1), width = 3,flag = 0, digits=2)) #prob of extinction
    P.extinction  <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom")
  } else {
    P.extinction  <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (any(chosen_combo[[2]] == "Diffusion")) {
  	prob_choose <- as.numeric(formatC(rnorm(1, mean = .3, sd =.1), width = 3,flag = 0, digits=2)) #prob of diffusion
    P.diffusion <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom") 
  } else {
    P.diffusion <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (any(chosen_combo[[2]] == "Takeover")) {
  	prob_choose <- as.numeric(formatC(rnorm(1, mean = .3, sd =.1), width = 3,flag = 0, digits=2)) #prob of takeover
    P.TakeOver <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom")
  } else {P.TakeOver <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (any(chosen_combo[[2]] == "Random_new_origin")) {
  	prob_choose <- as.numeric(formatC(rtnorm(1, mean = .01, sd =.1, upper=1, lower=0), width = 3,flag = 0)) # prob of Arisal
    P.Arisal <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom") 
  } else {
    P.Arisal <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  myOut <- RunSimUltimate(myWorld, P.extinction, P.speciation, 
                          P.diffusion, P.Arisal, P.TakeOver,
                          N.steps = number_of_time_steps)
  

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
                          "_timesteps_", number_of_time_steps,
                          "_", formatC(as.integer(Sys.time())/10000000000, width = 3) ,
                          ".Rdata"))

}

system.time(
#sim_run_cluster(1, 31, myWorld, 100)
	)
	
	
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
clusterEvalQ(cl, source("Functions/Speciate_function.R"))
clusterEvalQ(cl, source("Functions/Speciation_function.R"))
clusterEvalQ(cl, source("Functions/Takeover_function.R"))
clusterEvalQ(cl, source("Functions/SpeciationTakeover_Module.R"))
clusterEvalQ(cl, source("Functions/Possible_combinations_of_movement_function.R"))
clusterEvalQ(cl, source("Functions/Ultimate_run_simulations.R"))


# lset are the landscapes that we will run
b <- Sys.time()
replicate_cycle <- c(1:10)
number_of_time_steps_a <- 100
number_of_time_steps_b <- 300
number_of_time_steps_c <- 600
number_of_time_steps_d <- 1000

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 31, number_of_time_steps = number_of_time_steps_a,
               myWorld = myWorld) 

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 29, number_of_time_steps = number_of_time_steps_a,
               myWorld = myWorld) 
               
clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 28, number_of_time_steps = number_of_time_steps_a,
               myWorld = myWorld) 

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 25, number_of_time_steps = number_of_time_steps_a,
               myWorld = myWorld) 
             
c <- Sys.time()

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 31, number_of_time_steps = number_of_time_steps_b,
               myWorld = myWorld) 

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 29, number_of_time_steps = number_of_time_steps_b,
               myWorld = myWorld) 
               
clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 28, number_of_time_steps = number_of_time_steps_b,
               myWorld = myWorld) 

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 25, number_of_time_steps = number_of_time_steps_b,
               myWorld = myWorld) 

d <- Sys.time()

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 31, number_of_time_steps = number_of_time_steps_c,
               myWorld = myWorld) 

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 29, number_of_time_steps = number_of_time_steps_c,
               myWorld = myWorld) 
               
clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 28, number_of_time_steps = number_of_time_steps_c,
               myWorld = myWorld) 

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 25, number_of_time_steps = number_of_time_steps_c,
               myWorld = myWorld) 
 
e <- Sys.time()

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 31, number_of_time_steps = number_of_time_steps_d,
               myWorld = myWorld) 

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 29, number_of_time_steps = number_of_time_steps_d,
               myWorld = myWorld) 
               
clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 28, number_of_time_steps = number_of_time_steps_d,
               myWorld = myWorld) 

clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 25, number_of_time_steps = number_of_time_steps_d,
               myWorld = myWorld) 

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

stopCluster(cl)

