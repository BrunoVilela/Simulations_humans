path <- "~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/Module_1_sequence_for_movie"
      
      setwd(path)
    myfiles_full <- list.dirs()
    analyze_this_many <- length(myfiles_full)
    
    available_files <- matrix(NA, 1, 1)
    
        
    for(i in 1: analyze_this_many){
    available_files <- rbind(available_files , as.matrix(list.files(myfiles_full[i], full.names = TRUE)))
    }
    dim(available_files)
    
    split.file.name <- strsplit(available_files[10], split = "_") 
    
    available <- list.files()
files <- matrix(rep(NA, 35), length(available), 35)
dim(files)
i <- 10


for(i in 1:length(available)){
#load(available[i])

split.file.name <- unlist(strsplit(available[i], split="_"))
    split.file.name_timestep <-  unlist(strsplit(split.file.name[34], split=".R"))
       
    
files[i,] <- c(split.file.name[1:33], split.file.name_timestep)

}

head(files)


colnames(files) <-  c(
NA,
		NA,
	"replicate",
	NA,
	"Model_type",
	rep(NA,2),
	"speciation_of_Env_NonD",
	"speciation_of_Env_D",
	"speciation_of_For",
	"speciation_of_Dom",
	NA,
	"extinction_of_Env_NonD",
	"extinction_of_Env_D",
	"extinction_of_For",
	"extinction_of_Dom",
	NA,
	"P.diffusion_Target_forager",
	"P.diffusion_Target_domesticator",
	"P.diffusion_Source_forager",
	"P.diffusion_Source_domesticator",
	NA,
	"P.takeover_Target_forager",
	"P.takeover_Target_domesticator",
	"P.takeover_Source_forager",
	"P.takeover_Source_domesticator",
	NA,
	"arisal_of_Env_NonD",
	"arisal_of_Env_D",
	"arisal_of_For",
	"arisal_of_Dom",
	
	NA, 
	"timesteps", 
	NA,
	NA
        
      
  )

results_table <- as.data.frame(files)
head(results_table)
dim(results_table)

