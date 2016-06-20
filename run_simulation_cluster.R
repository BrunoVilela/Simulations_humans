# Run the full model in a cluster. This version writes files to a cluster output folder.


sim_run_cluster <- function(replicate_cycle, combo_number){

chosen_combo <- combo_of_choice(combo_number, FALSE)

if(chosen_combo[[2]] == "Speciate" ) {P.speciation <- parameters(0.1, 0.1, 0.1, 0.1, "For", "Dom", "For", "Dom") } else {P.speciation <- parameters(0,0,0,0, "For", "Dom", "For", "Dom")}

if(chosen_combo[[2]] == "Extinct" ) {P.extinction  <- parameters(0, 0, 0.1, 0, "For", "Dom", "For", "Dom") } else {P.extinction  <- parameters(0,0,0,0, "For", "Dom", "For", "Dom")}

if(chosen_combo[[2]] == "Diffusion" ) {P.diffusion <- parameters(0.1, 0.1, 0.1, 0.1, "For", "Dom", "For", "Dom") } else {P.speciation <- parameters(0,0,0,0, "For", "Dom", "For", "Dom")}

if(chosen_combo[[2]] == "Takeover" ) {P.TakeOver <- parameters(0.1, 0.1, 0.1, 0.1, "For", "Dom", "For", "Dom") } else {P.TakeOver <- parameters(0,0,0,0, "For", "Dom", "For", "Dom")}

if(chosen_combo[[2]] == "Random_new_origin" ) {P.Arisal <- parameters(0.1, 0.1, 0.1, 0.1, "For", "Dom", "For", "Dom") } else {P.Arisal <- parameters(0,0,0,0, "For", "Dom", "For", "Dom")}

	#setwd("~/Box Sync/colliding ranges/Simulations_humans")
  myWorld <- BuildWorld(R = 3, P = 0.8)
  system.time (
  myOut <- RunSim(myWorld, P.extinction, P.speciation, 
                  P.diffusion, P.Arisal, P.TakeOver, N.steps = 50)
  )
  #)


setwd("~/Box Sync/colliding ranges/Simulations_humans/cluster outputs")
save(myOut, file = paste("myOut replicate_", replicate_cycle, " function_combination_type_", combo_number," Results.Rdata", sep=""))
    }



library(parallel)
setwd("~/Box Sync/colliding ranges/Simulations_humans/Functions")

# Set up cluster
cl <- makeCluster(7, type = "PSOCK")

# Push resources out to cluster
clusterEvalQ( cl, library(gtools) )
clusterEvalQ( cl, library(ape) )
clusterEvalQ( cl, library(adephylo) )
clusterEvalQ( cl, library(diversitree) )
clusterEvalQ( cl, source("Arisal_module.R") )
clusterEvalQ( cl, source("Auxiliary_functions.R") )
clusterEvalQ( cl, source("Build_world_function.R") )
clusterEvalQ( cl, source("Complete_Model.R") )
clusterEvalQ( cl, source("Difusion_module.R") )
clusterEvalQ( cl, source("Extinction_module.R") )
clusterEvalQ( cl, source("Speciate_function.R") )
clusterEvalQ( cl, source("Speciation_function.R") )
clusterEvalQ( cl, source("Takeover_function.R") )
clusterEvalQ( cl, source("SpeciationTakeover_Module.R") )
clusterEvalQ( cl, source("Possible combinations of movement function.R") )

# lset are the landscapes that we will run

        replicate_cycle <- c(1:12)
        clusterApplyLB(cl, x = replicate_cycle, fun = sim_run_cluster, combo_number=30) 

stopCluster(cl)


