# Collapse the individual files produced by the first simulation module into one list with all the trees and another list with all the worlds. 

# Ty Tuff and Bruno Vilela
# 23 August 2016
# Washington University in St. Loius


CollapseSimulationFiles <- function(rep_number, path) {
    setwd(path)
      
    myfiles_full <- list.dirs()
    analyze_this_many <- length(myfiles_full)
    
    available_files <- matrix(NA, 1, 1)
    
        
    for(i in 1: analyze_this_many){
    available_files <- rbind(available_files , as.matrix(list.files(myfiles_full[i], full.names = TRUE)))
    }
    dim(available_files)
    
    
    
    
    
    
    install.packages("~/Desktop/FARM_1.0.tar.gz", repos=NULL, type="source")


#####################################################################
## need to document which functions we use from each of these libraries. 
library(ape)
library(spdep)
library(parallel)
library(Rcpp)
library(msm)
library(FARM)
library(phytools)
library(diversitree)
   
    #source('~/Box Sync/colliding ranges/Simulations_humans/Functions/Serial result analysis.R', chdir = TRUE)
   
   load(available_files[rep_number])
    
    Sim_statistics <- Module_2(myOut)
    
    split.file.name <- strsplit(available_files[rep_number], split = "_")  #split file name everywhere there is an underscore
    split.file.name_2 <- strsplit(split.file.name[[1]][30], split = "/") 
    split.file.name_3 <- strsplit(split.file.name_2[[1]][2], split = ".Rdata")
    
    
    
    save(myOut, file= paste0("./Module_1_outputs/myOut_replicate_", 
                           formatC(split.file.name[[1]][3], width = 2,flag = 0),
                           "_combination_",
                           formatC(split.file.name[[1]][5], width = 2,flag = 0),
                           "_","parameters", "_P.speciation_",
                           paste(split.file.name[[1]][8:11], collapse="_"), "_P.extinction_",
                           paste(split.file.name[[1]][13:16], collapse="_"), "_P.diffusion_",
                           paste(split.file.name[[1]][18:21], collapse="_"), "_P.TakeOver_",
                           paste(split.file.name[[1]][23:26], collapse="_"),"_P.Arisal_",
                           split.file.name[[1]][28],
                           "_timesteps_", split.file.name_2[[1]][1], "_", split.file.name_3  ,"_.Rdata"))
  
    save(Sim_statistics, file= paste0("./Module_2_outputs/Sim_statistics_replicate_", 
                           formatC(split.file.name[[1]][3], width = 2,flag = 0),
                           "_combination_",
                           formatC(split.file.name[[1]][5], width = 2,flag = 0),
                           "_","parameters", "_P.speciation_",
                           paste(split.file.name[[1]][8:11], collapse="_"), "_P.extinction_",
                           paste(split.file.name[[1]][13:16], collapse="_"), "_P.diffusion_",
                           paste(split.file.name[[1]][18:21], collapse="_"), "_P.TakeOver_",
                           paste(split.file.name[[1]][23:26], collapse="_"),"_P.Arisal_",
                           split.file.name[[1]][28],
                           "_timesteps_", split.file.name_2[[1]][1], "_", split.file.name_3  ,"_.Rdata"))
    
    
    
    
    }



     
# CollapseSimulationFiles(1, path =  "~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/50000 timesteps with a file printed every 250 steps/model 31")






