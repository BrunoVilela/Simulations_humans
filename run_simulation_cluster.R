# Run the full model in a cluster. This version writes files to a cluster output folder.
rm(list = ls())
source("Functions/Build_world_function.R")
source("Functions/Auxiliary_functions.R")
myWorld <- BuildWorld(R = 3, P = 0.8)
source('Functions/Possible_combinations_of_movement_function.R')

sim_run_cluster <- function(replicate_cycle, combo_number, myWorld) {
  
  chosen_combo <- combo_of_choice(combo_number, FALSE)
  
  if (chosen_combo[[2]] == "Speciate") {
    P.speciation <- parameters(0.3, 0.3, 0.3, 0.3, "For", "Dom", "For", "Dom")
  } else {
    P.speciation <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (chosen_combo[[2]] == "Extinct") {
    P.extinction  <- parameters(0.1, 0.1, 0.1, 0.1, "For", "Dom", "For", "Dom")
  } else {
    P.extinction  <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (chosen_combo[[2]] == "Diffusion") {
    P.diffusion <- parameters(0.1, 0.1, 0.1, 0.1, "For", "Dom", "For", "Dom") 
  } else {
    P.diffusion <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (chosen_combo[[2]] == "Takeover") {
    P.TakeOver <- parameters(0.1, 0.1, 0.1, 0.1, "For", "Dom", "For", "Dom")
  } else {P.TakeOver <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  if (chosen_combo[[2]] == "Random_new_origin") {
    P.Arisal <- parameters(0.1, 0.1, 0.1, 0.1, "For", "Dom", "For", "Dom") 
  } else {
    P.Arisal <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
  }
  
  myOut <- RunSimUltimate(myWorld, P.extinction, P.speciation, 
                          P.diffusion, P.Arisal, P.TakeOver,
                          N.steps = 50)
  
 save(myOut, file= paste0("cluster outputs/myOut_replicate_", 
                          formatC(replicate_cycle, width = 2,flag = 0),
                          "_function_combination_type_",
                          formatC(combo_number, width = 2,flag = 0),
                          "_","parameters", "_P.speciation_",
                          paste(P.speciation, collapse="_"), "_P.extinction_",
                          paste(P.extinction, collapse="_"), "_P.diffusion_",
                          paste(P.diffusion, collapse="_"), "_P.TakeOver_",
                          paste(P.TakeOver, collapse="_"),"_P.Arisal_",
                          paste(P.Arisal, collapse="_"),
                          "_", as.integer(Sys.time()),
                          " Results.Rdata"))

}


a <- Sys.time()
library(parallel)


# Set up cluster
cl <- makeCluster(detectCores() , type = "PSOCK")

# Push resources out to cluster
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
replicate_cycle <- c(1:20)
clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, 
               combo_number = 31, myWorld = myWorld) 
c <- Sys.time()


difftime(b, a)
# Time to load packages

difftime(c, b)
# Time to run cluster

stopCluster(cl)

