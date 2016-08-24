####
setwd("~/Box Sync/colliding ranges/Simulations_humans")

######Read in R libraries##############################
#source("Plot_output.R")
library(ape)
library(phytools)
library(picante)
library(apTreeshape)
library(caper)
library(geiger)
library(diversitree)
library(spdep)

######Read in R functions##############################
start_time <- Sys.time()

# Required packages and functions
load.files <- list.files(path = "Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}



in_path <- "~/Box Sync/colliding ranges/Simulations_humans/results cluster output/Collapsed sim results/"


cluster_results_analysis <- function(in_path) {
  
  load.files <- list.files(path = in_path, pattern = ".Rdata",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  load(load.files[i])
}

  
  ##### Calculate each metric from the parameters provided by the file name and add them to the matrix  ###########
  ############################################################################################
  
  ##### (0) Pull necessary variables from simulated trees and organize into a single object for all the tests below to pull from.
  
  #str(all_trees)
  
  ## 0a) Branch lengths
  Branch_Lengths <- lapply(all_trees, "[[", 4)
  
  extract_branch_length <- Sys.time()
  
  ## 0b) Pairwise distance between tips
  
  Pairwise_dist <- lapply(all_trees, cophenetic)  
  
  calc_pairwise_dist <- Sys.time()	
  
  ## 0c) Phylogenetic isolation
  
  # Using equal.splits method, faster computation
  Evolutionary_distinctiveness <- lapply(all_trees, evol.distinct2, 
                                         type = "equal.splits") 
  
  calc_evolutionary_distinctiveness <- Sys.time()
  
  ## 0d) tree topology
  
  all_trees_as_treeshape <- lapply(all_trees, as.treeshape)
  
  
  ##### (1) Spatial metrics ###################
  #######################################
  # Spatial
  nbs0 <- lapply(all_worlds, function(x) knearneigh(as.matrix(x[, 2:3]),
                                                    k = 7, longlat = TRUE))
  nbs <- lapply(nbs0, knn2nb, sym = TRUE) # 7 symmetric neighbors
  nbs.listw <- lapply(nbs, nb2listw)
  factors.nbs <- lapply(all_worlds, function(x) as.factor(ifelse(is.na(x[, 6]),
                                                                 3, x[, 6])))
  spatial_tests <- mapply(joincount.test, fx = factors.nbs, listw = nbs.listw, 
                          SIMPLIFY = FALSE)
  
  
  
  calc_spatial_metrics <- Sys.time()
  
  ##### (2) Tree metric -- Richness - Sum ##################
  ##################################################
  
  
  ## 2a) Branch lengths -- Amount of evolutionary history
  
  ## 2a.1 Across species
  
  # Anchor test = PD (Faith's phylogenetic diversity) 
  
  Pylo_diversity <- sapply(Branch_Lengths, sum)
  
  ## 2a.2 Across individuals
  
  #delta nPD #IGNORED BECAUSE IT USES ABUNDANCE
  
  ## 2a.3 Effective (q=0)
  
  #oD(T)*  #IGNORED BECAUSE IT USES ABUNDANCE
  
  #oPD(T)*  #IGNORED BECAUSE IT USES ABUNDANCE
  
  
  ## 2a.4 Per species PDab
  
  #PE  #IGNORED BECAUSE IT USES RANGE SIZE 
  
  
  
  ## 2b) Pairwise distance -- Sum of pairwise distances
  
  
  # F -- Extensive quadratic entropy 
  
  F_quadratic_entropy <- sapply(Pairwise_dist, sum)
  
  
  ## 2c) Phylogenetic isolation -- Sum of evolutionary distinctiveness
  
  # ED - Summed evolutionary distinctiveness
  
  Evolutionary_distinctiveness_sum <- sapply(Evolutionary_distinctiveness, sum)
  
  
  calc_richness_metrics <- Sys.time()
  
  ##### (3) Tree metric -- Divergence - Mean ###############
  ##################################################
  
  
  
  
  ## 3a) Branch lengths 
  
  ## 3a.1 -- Sum of branch lengths divided by species richness
  
  # avPD -- Average phylogenetic diversity
  
  average_phylogenetic_diversity <- sapply(Branch_Lengths, mean)
  
  # avPDab #IGNORED BECAUSE IT USES ABUNDANCE
  
  
  ## 3a.2 -- Effective number of species given phylogenetic balance and abundance evenness (q>0)
  
  #qD(t)*
  
  
  #qPD(T)*
  
  
  ## 3a.3 -- associated entropies
  
  #Hp*
  
  
  #Lq*
  
  ## 3b) Pairwise distance/ similarities of all
  
  ## 3b.1 -- Effective number of species given phylogenetic balance and abundance evenness 
  
  #qD^z(p)*
  
  ## 3b.2 -- Mean of all distances including zero intra-species distances
  
  #Rao's QE
  
  #MPDab
  
  # PSE
  
  #J
  
  ## 3b.3 -- Mean inter-species distances
  
  # Anchor test = MPD (mean pairwise distance)
  
  Mean_pairwise_distance <- sapply(Pairwise_dist, mean)
  
  # AvTD
  
  # PSV
  
  #interMPDab #IGNORED BECAUSE IT USES ABUNDANCE
  
  
  ## 3c) Pairwise distance/ similarities of nearest neighbors -- Mean shortest distance between a species and all others
  
  #MNTD
  
  #MNTDab #IGNORED BECAUSE IS USES ABUNDANCE
  
  ## 3d) Phylogenetic isolation -- Mean of species evolutionary distinctiveness
  
  # mean(ED)
  
  mean_Phylogenetic_isolation <- sapply(Evolutionary_distinctiveness, mean)
  
  
  
  calc_divergence_metrics <- Sys.time()
  
  ##### (4) Tree metric -- Regularity - Variance ##############
  ##################################################
  
  variance_Pylo_diversity <- sapply(Branch_Lengths, var)
  
  
  
  ## 4a) Tree topology -- Branching symmetry and distribution
  
  #Ic -- Colles test
  
  Ic <- sapply(all_trees_as_treeshape, colless, norm = NULL)
  
  #Iw - Fusco and Cronk 1995 suggested by Simon Greenhill
  
  
  
  #  S <- fusco_tester$observed$S 
  #  I.prime <- fusco_tester$observed$I.prime
  #  fuscoDist <- hist(fusco_tester $observed$I.prime, breaks = 10, plot = TRUE)
  
  #  allPossI <- function(S, I.prime) {
  #            m <- ceiling(S/2)
  #            RET <- (seq(from = m, to = S - 1) - m)/((S - 1) - 
  #                m)
  #            if (I.prime & (S%%2) == 1) 
  #                RET <- RET * (S - 1)/S
  #            return(RET)
  #        }
  
  #for(w in 1:length(fusco_tester$observed$S)){
  #  hist(allPossI(fusco_tester$observed$S[w], fusco_tester$observed$I.prime[w]))
  #  }
  
  #str(fusco.test(all_trees[[14]]))
  # fusco_tester <- fusco.test(all_trees[[14]])
  # str(fusco_tester)
  # class(fusco_tester[9])
  # hist(unlist(fusco_tester[9]), breaks=10)
  #hist(unlist(fusco_tester[10]), breaks=10)
  #  fusco_tester[1]$I.prime
  
  # plot(fusco.test(all_trees[[14]], tipsAsSpecies=TRUE))
  #plot.fusco
  #  plot.fusco(fusco_tester $observed$I.prime)
  
  # hist(fusco_tester$observed$I)
  # hist(fusco_tester $observed$I.prime)
  # hist(fusco_tester $observed$I.w)
  
  
  
  #Gamma index
  
  ltts <- lapply(all_trees, ltt, gamma = TRUE, plot = FALSE)
  lineages_through_time <- sapply(ltts, "[[", 1)
  time_steps <- sapply(ltts, "[[", 2)
  gamma <- sapply(ltts, "[[", 3)
  gamma_p_value <- sapply(ltts, "[[", 4)
  
  #log(gamma_list[[k]]$ltt)
  
  #str(gamma_list)
  #ltt.custom(gamma_list[[k]]$tree)
  #branching.times(gamma_list[[k]]$tree)
  
  # str(gamma_list)
  #class(all_trees)
  
  #IAC
  
  ## 4b) Pairwise distance/all distances -- Variance of pairwise distances 
  
  # Anchor test = VPD (variation of pairwise distance)
  
  variance_pairwise_distance <- sapply(Pairwise_dist,
                                       function(x) var(as.numeric(x)))
  
  # A+
  
  # VarTD
  
  # VPDab #IGNORED BECAUSE IT USES ABUNDANCE
  
  # interVPDab #IGNORED BECAUSE IT USES ABUNDANCE
  
  
  ## 4c) Pairwise distance/nearest neighbor -- Variance of nearest neighbour distance
  
  # VNTD
  
  # VNTDab #IGNORED BECAUSE IT USES ABUNDANCE
  
  # PEve
  
  ## 4d) Phylogenetic isolation -- Variance of species isolation metrics
  
  #var(ED)
  
  variance_Phylogenetic_isolation <- sapply(Evolutionary_distinctiveness, var)
  
  
  #Eed
  
  #Hed
  
  # Haed
  
  #qD(P)*
  
  #qD(AP)*
  
  
  calc_regularity_metrics <- Sys.time()
  
  ##### (5) Tree metric -- Macroevolutionary - Rate and rate changes ###############
  ##################################################
  
  ## Speciation vs extinction rates and Net diversification
  bds <- sapply(all_trees, bd)
  
  ## Speciation vs extinction rates and Net diversification dependent on trait
  par.div.dep <- mapply(DivDep, mytree = all_trees, myWorld = all_worlds)
  
  ## Instantaneous rate or speciation and extinction from BAMM 
  # ????
  
  ## Phylogenetic signal (D)
  phy.sig.D <- mapply(D, mytree = all_trees, myWorld = all_worlds)
  
  ## Transistion rates (variable rates)
  #Transition.rates <- mapply(transitions, mytree = all_trees, myWorld = all_worlds)
  #q12 <- Transition.rates[1, ] # transition from foraging to farming
  #q21 <- Transition.rates[2, ] # transition from farming to foraging
  #rates.ratio <- q12/q21 # the ratio between both transition rates
  
  calc_macroevolution_metrics <- Sys.time()
  
  
  
  ### Calculate and return time stamps
  time_vect <- format(c(start_functions, parse_file_names, load_files,
                        extract_branch_length, calc_pairwise_dist,
                        calc_evolutionary_distinctiveness, calc_spatial_metrics,
                        calc_richness_metrics, calc_divergence_metrics,
                        calc_regularity_metrics, calc_macroevolution_metrics))
  calc_times <- as.data.frame(difftime(time_vect[-1], 
                                       time_vect[-(length(time_vect))]))
  calc_times <- rbind(calc_times, difftime( time_vect[length(time_vect)],
                                            time_vect[1]))
  colnames(calc_times) <- c("walltime")
  rownames(calc_times) <-  c( "parse file names", "load files from simulation", "extract branch lengths", "calculate pairwise distance", "calculate evolutionary distinctiveness", "calculate spatial metrics", "calculate richness metrics", "calculate divergence metrics", "calculate regularity metrics", "calculate macroevolution metrics", "total time")
  
  
  ### Returns from function in list form
  returns <- list(
    calc_times,
    myfiles, 
    all_trees,
    all_trees_as_treeshape,
    all_worlds, 
    keep, 
    extinctions, 
    
    spatial_tests,
    
    Branch_Lengths,
    Pylo_diversity,
    average_phylogenetic_diversity,
    variance_Pylo_diversity,
    
    Pairwise_dist,
    F_quadratic_entropy,
    Mean_pairwise_distance,
    variance_pairwise_distance,
    
    Evolutionary_distinctiveness,
    Evolutionary_distinctiveness_sum,
    mean_Phylogenetic_isolation,
    variance_Phylogenetic_isolation,
    
    
    Ic,
    lineages_through_time , 
    time_steps, 
    gamma, 
    gamma_p_value, 
    
    bds,
    par.div.dep,
    phy.sig.D #,
    #Transition.rates,
    #q12,
    #q21,
    #rates.ratio
  )
  
  names(returns) <- c(
    "calc_times", 
    "files_used_in_analysis",
    "all_trees_as_phylo_object",
    "all_trees_as_treeshape_object",
    "landscapes_from_all_replicates",
    "extant_landscapes", 
    "global_extinctions",  
    "spatial_tests",
    
    #unit: branch lengths
    "branch_lengths",
    "pylogenetic_diversity_is_sum_branch_lengths",
    "mean_phylogenetic_diversity",
    "variance_in_pylogenetic_diversity",
    
    #unit: pairwise distance 
    "pairwise distance between tips",
    "F_quadratic_entropy_is_sum_of_pairwise_distance_between_tips",
    "mean_pairwise_distance_between_tips",
    "variance_in_pairwise_distance_between_tips",
    
    #unit: evolutionary distinctiveness
    "evolutionary_distinctiveness",
    "evolutionary_distinctiveness_sum",
    "mean_phylogenetic_isolation_is_mean_evolutionary_distinctiveness",
    "variance_phylogenetic_isolation_is_variance_in_evolutionary_distinctiveness",
    
    # tree topology
    "Colless_statistic",
    "lineages_through_time_by_number_of_tips", 
    "waiting_time_corresponding_to_lineages_through_time_by_number_of_tips", 
    "gamma_parameter", 
    "gamma_parameter_P_value", 
    
    # Evolutionary rates
    "speciation_and_extinction_rates_as_birth_death",
    "speciation_vs_extinction_rates_and_net_diversification_dependent_on_trait",
    "phylogenetic_signal" #,
    #"Evolutionary transition rates",
    #"transition from foraging to farming",
    #"transition from farming to foraging",
    #"ratio between both transition rates"
    
  )
  
  
  print(calc_times)
  
  save(returns, file=paste0("results cluster output/", 
                            "Results_ analysis_for_model_",
                            combo_pass, "_", "simulated_for_ ",
                            Timesteps_pass,"_timesteps_", analyze_this_many,
                            "_replicates_used.R"))
  
  return(returns)
  
}



starter_time <- Sys.time()


library(parallel)

# Set up cluster
cl <- makeCluster(detectCores() , type = "PSOCK")


# Push resources out to cluster

clusterEvalQ(cl, library(ape))
clusterEvalQ(cl, library(phytools))
clusterEvalQ(cl, library(picante))
clusterEvalQ(cl, library(apTreeshape))
clusterEvalQ(cl, library(caper))
clusterEvalQ(cl, library(geiger))
clusterEvalQ(cl, library(diversitree))
clusterEvalQ(cl, library(spdep))



setwd("~/Box Sync/colliding ranges/Simulations_humans")
clusterExport(cl, varlist=ls())

# lset are the landscapes that we will run

# setwd("~/Desktop")
combo_type <- c(25,28,29,31)

analyze_this_many <- 200

b <- Sys.time()
clusterApplyLB(cl, x = combo_type, fun = cluster_results_analysis, analyze_this_many = analyze_this_many,  Timesteps_pass = 10000) 

c <- Sys.time()


difftime(b, starter_time)
# Time to load packages

difftime(c, b)
# Time to run combo 31


stopCluster(cl)

