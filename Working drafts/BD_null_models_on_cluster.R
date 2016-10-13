library(ape)
library(phytools)
library(geiger)
library(FARM)
ls("package:FARM")

bd_null <- function(g){
	#g<-1
#rep_number <- 60000
#data_gamma <- rep(NA, rep_number)
#for(g in 1:rep_number){
a_tree <- sim.bdtree(b=1, d=0, n=1200, t=30000)
this_tree <- chronopl(a_tree, 1)

 ##### (0) Pull necessary variables from simulated trees and organize into a single object for all the tests below to pull from.

    #str(all_trees)
    #str(this_tree)


    ## 0a) Branch lengths
    Branch_Lengths <- this_tree$edge.length
    number_of_branches <- length(Branch_Lengths)

    # Anchor test = PD (Faith's phylogenetic diversity)
    Pylo_diversity_is_sum_of_BL <- sum(Branch_Lengths)

    # avPD -- Average phylogenetic diversity
    average_phylogenetic_diversity_is_mean_of_BL <- mean(Branch_Lengths)

    variance_Pylo_diversity_is_variance_of_BL <- var(Branch_Lengths)
    cat("-")
    ## 0b) Pairwise distance between tips
    Pairwise_dist <- cophenetic(this_tree)
    cat("-")
    # 2b) Pairwise distance -- Sum of pairwise distances

    # F -- Extensive quadratic entropy
    F_quadratic_entropy_is_sum_of_PD <- sum(Pairwise_dist)

    #Mean inter-species distances

    # Anchor test = MPD (mean pairwise distance)

    Mean_pairwise_distance <- mean(Pairwise_dist)

    cat("-")
    #Pairwise distance/all distances -- Variance of pairwise distances

    # Anchor test = VPD (variation of pairwise distance)

    variance_pairwise_distance <- var(as.vector(Pairwise_dist))




    ## 0c) Phylogenetic isolation

    # Using equal.splits method, faster computation
    Evolutionary_distinctiveness <- evol.distinct2(this_tree, type = "equal.splits")
    cat("-")
    # ED - Summed evolutionary distinctiveness

    Evolutionary_distinctiveness_sum <- sum(Evolutionary_distinctiveness)

    ## 3d) Phylogenetic isolation -- Mean of species evolutionary distinctiveness

    # mean(ED)

    mean_Phylogenetic_isolation <- mean(Evolutionary_distinctiveness)

    ## 4d) Phylogenetic isolation -- Variance of species isolation metrics

    #var(ED)

    variance_Phylogenetic_isolation <- var(Evolutionary_distinctiveness)
    cat("-")

    ## Tree topology

    #Gamma index

    ltts <- ltt(this_tree, gamma = TRUE, plot = FALSE)
    lineages_through_time <- as.numeric(ltts[[1]])
    time_steps <- as.numeric(ltts[[2]])
    gamma <- ltts[[3]]
    gamma_p_value <- ltts[[4]]
    cat("-")

    ##### (5) Tree metric -- Macroevolutionary - Rate and rate changes ###############
    ##################################################

    ## Speciation vs extinction rates and Net diversification
    bds <- bd(this_tree)
    speciation_rate <- bds[1]
    extinction_rate <- bds[2]
    extinction_per_speciation <- bds[3]
    speciation_minus_extinction <- bds[4]
    cat("-")


      

    results_summary_matrix_1 <- cbind(

      number_of_branches,
      Pylo_diversity_is_sum_of_BL,
      average_phylogenetic_diversity_is_mean_of_BL,
      variance_Pylo_diversity_is_variance_of_BL,

      F_quadratic_entropy_is_sum_of_PD,
      Mean_pairwise_distance,
      variance_pairwise_distance,

      Evolutionary_distinctiveness_sum,
      mean_Phylogenetic_isolation,
      variance_Phylogenetic_isolation,

      gamma,
      gamma_p_value,
      speciation_rate,
      extinction_rate,
      extinction_per_speciation,
      speciation_minus_extinction
     
   
    )
    rownames(results_summary_matrix_1) <- 1

    results_summary_matrix_2 <- cbind(
      Evolutionary_distinctiveness,
      lineages_through_time,
      time_steps
    )
    colnames(results_summary_matrix_2) <- c("Evolutionary_distinctiveness", "lineages_through_time", "time_steps")
    head(results_summary_matrix_2)

    ### Returns from function in list form
    returns <- list(
      #Branch_Lengths,
      #Pairwise_dist,
      results_summary_matrix_1,
      results_summary_matrix_2

    )

    names(returns) <- c(
      #"Branch_Lengths",
      #"Pairwise_distance",
      "results_summary_of_single_value_outputs",
      "results_summary_matrix_of_multi_value_outputs"
    )
    cat("] 100%")

    return(returns)
}


	



# NAI <- 1000
args <- commandArgs(trailingOnly = FALSE)
NAI <- as.numeric(args[7])
# setwd("~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs")
j <- bd_null(NAI)


save(j, file= paste0("./BD_null/bdtrees", NAI ,".Rdata"))
	
