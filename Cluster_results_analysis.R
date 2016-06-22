# Code to evaluate the outputs
# setwd("~/Box Sync/colliding ranges/Simulations_humans")
# Required packages and functions
load.files <- list.files(path = "Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}
source("Plot_output.R")
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



cluster_results_analysis <- function( analyze_this_many, combo, Timesteps_pass) {

# Load the results
myfiles <- list.files("cluster outputs", full.names = TRUE)
split.file.name <- strsplit(myfiles, split = "_") 
positions <- c(3, 7, 10:13, 15:18, 20:23, 25:28, 30:33, 34) #should be 35 next once underscore is fixed


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

for (i in 1:length(positions)) {
  data.result[, i + 1] <- sapply(split.file.name, "[", positions[i])
}

cluster_input_files <- subset(data.result, combo == combo & Timesteps == Timesteps_pass )

if(analyze_this_many > length(myfiles)){analyze_this_many <- length(myfiles)}
myfiles <- cluster_input_files[1:analyze_this_many,1]
data.result <- cluster_input_files[1:analyze_this_many,]
  
  
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
  Medusa.BP <- rep(NA, l.myfiles)
  Trasition.rates <- rep(NA, l.myfiles)
  weibull <- matrix(ncol = 2, nrow = l.myfiles)
  colnames(weibull) <- c("shape", "scale")
  
  # Loop
  if (!"tools:rstudio" %in% search()) {
    dev.new(width = 2, height = 2, pointsize = 12)
    par(mar = c(0, 0, 0, 0))
  }
  
  for (i in 1:l.myfiles) {
    
    load(myfiles[i])
    
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
      temp.medusa <- try(nrow(medusa(myOut$mytree, warnings = FALSE)$summary), silent = TRUE)
      if (class(temp.medusa) == "try-error") {
        Medusa.BP[i] <- 1
      } else {
        Medusa.BP[i] <- temp.medusa
      }
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
  data.result$Medusa.BP <- Medusa.BP
  data.result$Trasition.rates <- Trasition.rates
  data.result <- cbind(data.result, weibull)
  #return(data.result)
  
  save(data.result, file=paste0("results cluster output/", "Results_for_",combo, "_" , "simulated_for_ ",Timesteps_pass , "_time_steps_analysis.R"))
}

#rm(data.result)
#load(paste0(combo_type, "_" , Timesteps_pass , "_analysis.R") )


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
clusterEvalQ(cl, library(geiger ))
clusterEvalQ(cl, library(caper ))
clusterEvalQ(cl, library(spdep ))

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
b <- Sys.time()

combo_type <- c(25,28,29,31)
Timesteps_pass <-50
analyze_this_many <- 1


clusterApplyLB(cl, x = combo_type, fun = cluster_results_analysis, analyze_this_many = analyze_this_many,  Timesteps_pass= Timesteps_pass) 
c <- Sys.time()




difftime(b, a)
# Time to load packages

difftime(c, b)
# Time to run combo 31

stopCluster(cl)


load('~/Box Sync/colliding ranges/Simulations_humans/results cluster output/Results_for_25_simulated_for_ 50_time_steps_analysis.R')
