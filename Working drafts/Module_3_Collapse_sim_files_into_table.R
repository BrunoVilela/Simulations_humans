## First consolidate the available files into a single table
    
      path <- "~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/Four_model_compare_Module2"
      
      
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
files <- matrix(rep(NA, 62), length(available), 62)
dim(files)
i <- 10


for(i in 1:length(available)){
load(available[i])
name <- unlist(strsplit(available[i], split="_"))
files[i,] <- c(as.vector(matrix(name, 1,35)),matrix(Sim_statistics[[1]], 1, 27))

}


colnames(files) <-  c(

	rep(NA,3), 
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
        
    "number_of_branches",
	"Pylo_diversity_is_sum_of_BL",
	"average_phylogenetic_diversity_is_mean_of_BL",
	"variance_Pylo_diversity_is_variance_of_BL",

	"F_quadratic_entropy_is_sum_of_PD",
	"Mean_pairwise_distance",
	"variance_pairwise_distance",

	"Evolutionary_distinctiveness_sum",
	"mean_Phylogenetic_isolation",
	"variance_Phylogenetic_isolation",

	"gamma",
	"gamma_p_value",
	"speciation_rate",
	"extinction_rate",
	"extinction_per_speciation",
	"speciation_minus_extinction",
	"trait_1_speciation",
  	"trait_2_speciation" ,
  	"trait_1_extinction" ,
  	"trait_2_extinction" ,
  	"transition_from_trait_1_to_2" ,
  	"transition_from_trait_2_to_1" ,
  	"transition_rate_ratio_1to2_over_2to1" ,
  	"Phylogenetic_signal",
  	"spatial.tests.fora",
  	"spatial.tests.dom",
  	"prevalence"
  	
    
  )

results_table <- as.data.frame(files)
head(results_table)
dim(results_table)


save(results_table, file="~/Box Sync/colliding ranges/Simulations_humans/first_2200_sim_results_four_model_compare.Rdata")
