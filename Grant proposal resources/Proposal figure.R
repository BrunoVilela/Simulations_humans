# Script for creating a half page plot of preliminary model outputs for a grant proposal. 
#
# Ty Tuff, Bruno Vilela, and Carlos Botero
# Washington University in Saint Louis
# 2 August 2016
# 
# This plot will include 4 panels: lineage through time, gamma, map, and tree.



#setwd("~/Desktop")
setwd("~/Box Sync/colliding ranges/Simulations_humans")
#####################################################################

rm(list = ls())  # remove existing objects from workspace.

# Load all the functions used in this script from a folder where they are each stored and documented seperately. 
load.files <- list.files(path = "Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}

#####################################################################


## Load results data from each of the 4 models
load("results cluster output/Results_ analysis_for_model_25_simulated_for_ 5000_timesteps_180_replicates_used.R")
sea 		<- returns  ## S + E + A 

load("results cluster output/Results_ analysis_for_model_29_simulated_for_ 5000_timesteps_168_replicates_used.R")
sea.T 	<- returns  ## S + E + A + T

load("results cluster output/Results_ analysis_for_model_28_simulated_for_ 5000_timesteps_168_replicates_used.R")
sea.D 	<- returns  ## S + E + A + D

load("results cluster output/Results_ analysis_for_model_31_simulated_for_ 5000_timesteps_168_replicates_used.R")
sea.DT 	<- returns ## S + E + A + D + T
names(returns)

#### These results are plotted across 4 different pages, each with their own pdf call

##################################################################
# PAGE  
# getwd()
pdf( file = "Figures/Half_page_graphical_summary_for_proposal.pdf", width = 7, height = 4) # start page one, this command ends when the command dev.off() is called.

# This plot needs to house 24 plot boxes (4 x 6) and a double margin at the top for category labeling. Adding a margin box between each of those boxes and around the periphery defines a 9 x 14. 

# Matrix arrangement:
# 1, 1, 1, 1, 1, 1, 1, 1, 1,     						# top margin
# 2, 3, 4, 5, 6, 7, 8, 9, 10 							# top  label row
# 11, 12, 13, 14, 15, 16, 17, 18, 19, 		# top plot row
# 20, 20, 20, 20, 20, 20, 20, 20, 20,  		# first internal margin
# 21, 22, 23, 24, 25, 26, 27, 28, 29, 		# second row
# 30, 30, 30, 30, 30, 30, 30, 30, 30,  		# second internal margin
# 31, 32, 33, 34, 35, 36, 37, 38, 39,  		# third row
# 40, 40, 40, 40, 40, 40, 40, 40, 40,  		# third internal margin
# 41, 42, 43, 44, 45, 46, 47, 48, 49,  		# fourth row
# 50, 50, 50, 50, 50, 50, 50, 50, 50 			# fourth internal margin
# 51, 52, 53, 54, 55, 56, 57, 58, 59			# fifth row
# 60, 60, 60, 60, 60, 60, 60, 60, 60, 		# fifth internal margin
# 61, 62, 63, 64, 65, 66, 67, 68, 69,	        # bottom row
# 70, 70, 70, 70, 70, 70, 70, 70, 70, 		# bottom margin



page_two_layout_matrix <- matrix(c( rep(1,5), 2:6, rep(7,5), 8:12, rep(13,5), 14:18, rep(19,5)), 7, 5, byrow=TRUE)

# Specify the layout. Alternate the width calls so that margins are different between plot boxes and margin boxes. 
page_two_layout <-layout(page_two_layout_matrix, width=c( 0.1, 1, 0.1, 1, 0.1), height=c( 0.1, 2, 0.1, 1, 0.1, 1.5, 1)    )
#layout.show(page_two_layout) 

#dev.off()
par(mar=c(0,0,0,0))



replicate( 2 , blankplot(c(0,0), c(0,0)))

maps_with_points(sea)
blankplot(c(0,0), c(0,0))

maps_with_points(sea.T)
replicate( 3 , blankplot(c(0,0), c(0,0)))

example_tree(sea, "limegreen")
blankplot(c(0,0), c(0,0))

example_tree(sea.T, "orange")
replicate( 3 , blankplot(c(0,0), c(0,0)))



FarmGammaPlotBoxTwo(sea, sea.D, sea.T, sea.DT)
blankplot(c(0,0), c(0,0))

LineageThroughTimeTvTwo(sea, sea.D, sea.T, sea.DT)
replicate( 2 , blankplot(c(0,0), c(0,0)))


dev.off()