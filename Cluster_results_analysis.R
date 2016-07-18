i <- 99

# Code to evaluate the outputs
 setwd("~/Desktop")
# Required packages and functions
load.files <- list.files(path = "~/Box Sync/colliding ranges/Simulations_humans/Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}
#source("Plot_output.R")
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
library(survival)
library(maps)
library(spdep)

combo_pass <- 31 

analyze_this_many <-99  
Timesteps_pass <- 300

cluster_results_analysis <- function(combo_pass, analyze_this_many , Timesteps_pass) {


# Load the results
myfiles <- list.files("~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs", full.names = TRUE)
split.file.name <- strsplit(myfiles, split = "_") 

positions <- c(3, 6, 8:11, 13:16, 18:21, 23:26, 28:31, 34) #should be 35 next once underscore is fixed

data.result <- data.frame(matrix(ncol = 24, nrow = length(myfiles)))
colnames(data.result) <- c("File_path", "replicate", "combo",
                           "speciation_1", "speciation_2", "speciation_3", "speciation_4",
                           "extinction_1", "extinction_2", "extinction_3", "extinction_4",
                           "diffusion_1", "diffusion_2", "diffusion_3", "diffusion_4",
                           "takeover_1", "takeover_2", "takeover_3", "takeover_4",
                           "arisal_1", "arisal_2", "arisal_3", "arisal_4",
                           "Timesteps")
data.result[, 1] <- myfiles
head(data.result)
data.result.blank <- data.result

for (i in 1:length(positions)) {
  data.result[, i + 1] <- sapply(split.file.name, "[", positions[i])
}

which(data.result$Timesteps == '300')
which(data.result$combo == '31')
cluster_input_files <- subset(data.result, combo == as.character(combo_pass) & Timesteps == as.character(Timesteps_pass) )

if(analyze_this_many > length(myfiles)){analyze_this_many <- length(myfiles)}
myfiles <- cluster_input_files[1:analyze_this_many, 1]  ## temporary parameter to start index vector
data.result <- cluster_input_files[1:analyze_this_many, ]
  
  
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

