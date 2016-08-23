CollapseSimulationFiles <- function(path , combo_pass, Timesteps_pass) {
    
    #setwd("~/Box Sync/colliding ranges/Simulations_humans/")
    
    myfiles_full <- list.files(path, full.names = TRUE)
    analyze_this_many <- length(myfiles_full)
    
    # Required packages and functions
    #load.files <- list.files(path = "Functions", pattern = ".R", full.names = TRUE)
    #for (i in 1:length(load.files)) {
    #    source(load.files[i])
    #}
    
    ##### parse file names to retrieve simulation parameter info
    ##### ########################
    split.file.name <- strsplit(myfiles_full, split = "_")  #split file name everywhere there is and underscore
    
    
    positions <- c(3, 5, 8:11, 13:16, 18:21, 23:26, 28, 30) +1 # 
    data.result <- data.frame(matrix(ncol = 21, nrow = length(myfiles_full)))
    colnames(data.result) <- c("File_path", "replicate", "combo", "speciation_1", 
        "speciation_2", "speciation_3", "speciation_4", "extinction_1", 
        "extinction_2", "extinction_3", "extinction_4", "diffusion_1", 
        "diffusion_2", "diffusion_3", "diffusion_4", "takeover_1", "takeover_2", 
        "takeover_3", "takeover_4", "arisal_1", "Timesteps")
    data.result[, 1] <- myfiles_full
    # head(data.result)
    
    data.result.blank <- data.result  # pass matrix to new object to be used seperatly below
    
    for (i in 1:length(positions)) {
        data.result[, i + 1] <- sapply(split.file.name, "[", positions[i])
    }
    
    ##### Subset matrix of file information to pull out the files we want to
    ##### analyze together ########################
    cluster_input_files <- subset(data.result, combo == as.character(combo_pass) & 
        Timesteps == as.character(Timesteps_pass))
    n.cluster <- nrow(cluster_input_files)
    rownames(cluster_input_files) <- 1:n.cluster
    
    ##### assign each divided section of the file name to a column of a matrix
    ##### ########################
    if (analyze_this_many > n.cluster) {
        analyze_this_many <- n.cluster
    }
    sub <- 1:analyze_this_many
    myfiles <- cluster_input_files[sub, 1]  ## temporary parameter to start index vector
    data.result <- cluster_input_files[sub, ]
    
    parse_file_names <- Sys.time()
    
    
    ##### Load the file specified by each row for independent analysis
    ##### ################### for (i in 1:l.myfiles) {
    
    all_trees <- vector("list", 2)
    class(all_trees) <- "multiPhylo"
    
    all_worlds <- list()
    
    for (i in 1:analyze_this_many) {
        if (file.exists(myfiles[i])) {
            load(myfiles[i])
        }
        if (any(!is.na(myOut))) {
            all_trees[[i]] <- myOut$mytree
            all_worlds[[i]] <- myOut$myWorld
        }
    }
    
    # Subset the trees and world
    keep <- !sapply(all_trees, is.null)
    all_trees <- all_trees[keep]
    all_worlds <- all_worlds[keep]
    # Number of world extinctions
    extinctions <- sum(!keep)
    
    # Transform branch lenghts
    #all_trees <- lapply(all_trees, adjBranch)
    
    
      save(all_trees, file=paste0("~/Box Sync/colliding ranges/Simulations_humans/results cluster output/Collapsed sim results/Collapsed_simulation_outputs_containing_", analyze_this_many , "_all_trees_objects.R"))
    
     save(all_trees, file=paste0("~/Box Sync/colliding ranges/Simulations_humans/results cluster output/Collapsed sim results/Collapsed_simulation_outputs_containing_", analyze_this_many , "_all_worlds_objects.R"))
    
}



     
CollapseSimulationFiles(path = "~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/5000 timesteps"  , combo_pass = 31, Timesteps_pass = 5000)






