# Script for calling and assembling results figures into a large dashboard for comparison
#
# This script calls a large series of plots to summarize model outputs.
#
# 28 July 2016 
# Ty Tuff, Bruno Vilela & Carlos A. Botero
# Washington University in Saint Louis
#==================================================================

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


#### These results are plotted across 4 different pages, each with their own pdf call

##################################################################
# PAGE 1 
# Landscape layout with the distribution of input parameters on the top row, maps of the final arrangement of points for each model in the middle, and an example tree using the mean parameter values for each model.
getwd()
pdf( file = "Figures/PLOT_Page_1_overview.pdf", width = 11, height = 8.5) # start page one, this command ends when the command dev.off() is called.

# The layout function establishes the grid background for plots to be plotted to. 
# This layout should contain a boarder around the periphery for formatting adjustment later. 
# This layout should contain blank rows and columns between primary plot boxes for later formatting. 

# This plot needs to house 12 plot boxes (4 x 3). Adding a margin box between each of those boxes and around the periphery defines a 9 x 9. The first three plot boxes (and the two margin boxes seperating them) in the first row will be joined together into a single plot box.
# Matrix arrangement:
# 1, 1, 1, 1, 1, 1, 1, 1, 1,     						# top margin
# 2, 3, 3, 3, 3, 3, 4, 5, 6, 							# top row
# 7, 7, 7, 7, 7, 7, 7, 7, 7, 							# first internal margin
# 8, 9, 10, 11 ,12, 13, 14, 15, 16,   			# second row
# 17, 17, 17, 17, 17, 17, 17, 17, 17, 		# second internal margin
# 18, 19, 20, 21, 22, 23, 24,	 25, 26, 		# third row
# 27, 27, 27, 27, 27, 27, 27, 27, 27, 		# third internal margin
# 28, 29, 30, 31, 32, 33, 34, 35, 36, 		# bottom row
# 37, 37, 37, 37, 37, 37, 37, 37, 37, 		# bottom margin
page_one_layout_matrix <- matrix(c( rep( 1, 9 ), 2, rep( 3, 5 ), 4:6, rep( 7,  9 ), 8:16, rep( 17, 9), 18:26, rep( 27, 9 ), 28:36, rep( 37, 9 ) ), 9, 9, byrow=TRUE)

# Specify the layout. Alternate the width calls so that margins are different between plot boxes and margin boxes. 
page_one_layout <-layout(page_one_layout_matrix, width=c( 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1), height=c( 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1))    
#layout.show(page_one_layout) 
par(mar=c(0,0,0,0)) #set the default margin size within plot boxes to 0. This transfers the control of margin size to the width specified in the layout call.
























































dev.off()

##################################################################
# PAGE 2 
# Tree metrics

pdf( file = "Figures/PLOT_Page_2_tree_metrics.pdf", width = 8.5, height = 11)

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



page_two_layout_matrix <- matrix(c( rep( 1, 9 ), 2:19, rep( 20, 9 ), 21:29, rep( 30,  9 ), 31:39, rep( 40, 9), 41:49, rep( 50, 9 ), 51:59, rep( 60, 9 ), 61:69, rep( 70, 9 ) ), 14, 9, byrow=TRUE)

# Specify the layout. Alternate the width calls so that margins are different between plot boxes and margin boxes. 
page_two_layout <-layout(page_two_layout_matrix, width=c( 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1), height=c( 0.5, 0.5, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1,1, 0.1))    
#layout.show(page_two_layout) 
par(mar=c(0,0,0,0))






dev.off()


#################################################################
# PAGE 3 
# Tree rates

pdf( file = "Figures/PLOT_Page_3_evolutionary_rates.pdf", width = 8.5, height = 11)

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
# 51, 52, 53, 54, 55, 56, 57, 58, 59			# bottom row
# 60, 60, 60, 60, 60, 60, 60, 60, 60, 		# bottom margin

page_three_layout_matrix <- matrix(c( rep( 1, 9 ), 2:19, rep( 20, 9 ), 21:29, rep( 30,  9 ), 31:39, rep( 40, 9), 41:49, rep( 50, 9 ), 51:59, rep( 60, 9 ) ), 12, 9, byrow=TRUE)

# Specify the layout. Alternate the width calls so that margins are different between plot boxes and margin boxes. 
page_three_layout <-layout(page_three_layout_matrix, width=c( 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1), height=c( 0.5, 0.5, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1,1, 0.1))    
#layout.show(page_three_layout) 
par(mar=c(0,0,0,0))




dev.off()



##################################################################
# PAGE 4 
# Spatial metrics

pdf( file = "Figures/PLOT_Page_4_spatial_metrics.pdf", width = 8.5, height = 11)

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
# 51, 52, 53, 54, 55, 56, 57, 58, 59			# bottom row
# 60, 60, 60, 60, 60, 60, 60, 60, 60, 		# bottom margin

page_four_layout_matrix <- matrix(c( rep( 1, 9 ), 2:19, rep( 20, 9 ), 21:29, rep( 30,  9 ), 31:39, rep( 40, 9), 41:49, rep( 50, 9 ), 51:59, rep( 60, 9 ) ), 12, 9, byrow=TRUE)

# Specify the layout. Alternate the width calls so that margins are different between plot boxes and margin boxes. 
page_four_layout <-layout(page_four_layout_matrix, width=c( 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1), height=c( 0.5, 0.5, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1,1, 0.1))    
#layout.show(page_four_layout) 
par(mar=c(0,0,0,0))



dev.off()




