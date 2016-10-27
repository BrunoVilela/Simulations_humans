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




one <- subset(results_table, Model_type=="01" )
two <- subset(results_table, Model_type=="02" )
three <- subset(results_table, Model_type=="03" )
four <- subset(results_table, Model_type=="04" )

names(one)
one$NA.10
j<-3
sub_one <- as.matrix(subset(one, NA.10 == "00000029901"))
sub_two <- as.matrix(subset(two, NA.10 == "00000029901"))
sub_three <- as.matrix(subset(three, NA.10 == "00000029901"))
sub_four <- as.matrix(subset(four, NA.10 == "00000029901"))

sub_one <- as.matrix(subset(one, speciation_of_Env_NonD == 0.1981))
sub_two <- as.matrix(subset(two, speciation_of_Env_NonD == 0.2171))
sub_three <- as.matrix(subset(three, speciation_of_Env_NonD == 0.1823))
sub_four <- as.matrix(subset(four, speciation_of_Env_NonD == 0.2088))

chosen_ones <- as.data.frame(rbind(sub_one, sub_two, sub_three, sub_four))

this_time_step <- subset(chosen_ones, NA.10 == levels(chosen_ones$NA.10)[1])

names(chosen_ones)
levels(chosen_ones$NA.10)


#####################################################################
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/PLOT_blankplot.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/PLOT_display_important_numbers.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/PLOT_example_tree.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/PLOT_maps_with_points.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/PLOT_input_parameter_distributions.R', chdir = TRUE)



setwd("~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/Module_1_sequence_for_movie")
j <- 1

for(j in 151:length(levels(chosen_ones$NA.10))){

this_time_step <- subset(chosen_ones, NA.10 == levels(chosen_ones$NA.10)[j])

sub_one <- as.matrix(this_time_step[1,])
sub_two <- as.matrix(this_time_step[2,])
sub_three <- as.matrix(this_time_step[3,])
sub_four <- as.matrix(this_time_step[4,])

## Load results data from each of the 4 models
sea_name <- paste0(as.character(sub_one[1]), "_",as.character(sub_one[2]), "_",as.character(sub_one[3]), "_",as.character(sub_one[ 4]), "_",as.character(sub_one[5]), "_",as.character(sub_one[ 6]), "_",as.character(sub_one[ 7]), "_",as.character(sub_one[8]), "_",as.character(sub_one[ 9]), "_",as.character(sub_one[ 10]), "_",as.character(sub_one[11]), "_",as.character(sub_one[ 12]), "_",as.character(sub_one[ 13]), "_",as.character(sub_one[ 14]), "_",as.character(sub_one[ 15]), "_",as.character(sub_one[ 16]), "_",as.character(sub_one[ 17]), "_",as.character(sub_one[ 18]), "_",as.character(sub_one[ 19]), "_",as.character(sub_one[ 20]), "_",as.character(sub_one[ 21]), "_",as.character(sub_one[ 22]), "_",as.character(sub_one[ 23]), "_",as.character(sub_one[ 24]), "_",as.character(sub_one[ 25]), "_",as.character(sub_one[ 26]), "_",as.character(sub_one[ 27]), "_",as.character(sub_one[ 28]), "_",as.character(sub_one[ 29]), "_",as.character(sub_one[ 30]), "_",as.character(sub_one[ 31]), "_",as.character(sub_one[ 32]), "_",as.character(sub_one[ 33]), "_",as.character(sub_one[ 34]), ".R",as.character(sub_one[ 35]))
load( sea_name)
sea 		<- myOut  ## S + E + A 

sea.D_name <- paste0(as.character(sub_one[1]), "_",as.character(sub_one[2]), "_",as.character(sub_two[3]), "_",as.character(sub_two[ 4]), "_",as.character(sub_two[ 5]), "_",as.character(sub_two[6]), "_",as.character(sub_two[ 7]), "_",as.character(sub_two[ 8]), "_",as.character(sub_two[ 9]), "_",as.character(sub_two[ 10]), "_",as.character(sub_two[ 11]), "_",as.character(sub_two[ 12]), "_",as.character(sub_two[ 13]), "_",as.character(sub_two[ 14]), "_",as.character(sub_two[ 15]), "_",as.character(sub_two[ 16]), "_",as.character(sub_two[ 17]), "_",as.character(sub_two[ 18]), "_",as.character(sub_two[ 19]), "_",as.character(sub_two[ 20]), "_",as.character(sub_two[ 21]), "_",as.character(sub_two[ 22]), "_",as.character(sub_two[ 23]), "_",as.character(sub_two[ 24]), "_",as.character(sub_two[ 25]), "_",as.character(sub_two[ 26]), "_",as.character(sub_two[ 27]), "_",as.character(sub_two[ 28]), "_",as.character(sub_two[ 29]), "_",as.character(sub_two[ 30]), "_",as.character(sub_two[ 31]), "_",as.character(sub_two[ 32]), "_",as.character(sub_two[ 33]), "_",as.character(sub_two[ 34]), ".R",as.character(sub_two[ 35]))
load(sea.D_name)
sea.D 	<- myOut  ## S + E + A + D


sea.T_name <- paste0(as.character(sub_three[1]), "_",as.character(sub_three[2]), "_",as.character(sub_three[3]), "_",as.character(sub_three[ 4]), "_",as.character(sub_three[ 5]), "_",as.character(sub_three[6]), "_",as.character(sub_three[ 7]), "_",as.character(sub_three[ 8]), "_",as.character(sub_three[ 9]), "_",as.character(sub_three[ 10]), "_",as.character(sub_three[ 11]), "_",as.character(sub_three[ 12]), "_",as.character(sub_three[ 13]), "_",as.character(sub_three[ 14]), "_",as.character(sub_three[ 15]), "_",as.character(sub_three[16]), "_",as.character(sub_three[ 17]), "_",as.character(sub_three[ 18]), "_",as.character(sub_three[ 19]), "_",as.character(sub_three[ 20]), "_",as.character(sub_three[ 21]), "_",as.character(sub_three[ 22]), "_",as.character(sub_three[23]), "_",as.character(sub_three[ 24]), "_",as.character(sub_three[ 25]), "_",as.character(sub_three[ 26]), "_",as.character(sub_three[ 27]), "_",as.character(sub_three[ 28]), "_",as.character(sub_three[ 29]), "_",as.character(sub_three[ 30]), "_",as.character(sub_three[ 31]), "_",as.character(sub_three[ 32]), "_",as.character(sub_three[ 33]), "_",as.character(sub_three[ 34]), ".R",as.character(sub_three[ 35]))
load(sea.T_name)
sea.T 	<- myOut  ## S + E + A + T


sea.DT_name <- paste0(as.character(sub_four[1]), "_",as.character(sub_four[2]), "_",as.character(sub_four[3]), "_",as.character(sub_four[ 4]), "_",as.character(sub_four[ 5]), "_",as.character(sub_four[ 6]), "_",as.character(sub_four[ 7]), "_",as.character(sub_four[ 8]), "_",as.character(sub_four[ 9]), "_",as.character(sub_four[ 10]), "_",as.character(sub_four[ 11]), "_",as.character(sub_four[ 12]), "_",as.character(sub_four[ 13]), "_",as.character(sub_four[ 14]), "_",as.character(sub_four[ 15]), "_",as.character(sub_four[ 16]), "_",as.character(sub_four[ 17]), "_",as.character(sub_four[ 18]), "_",as.character(sub_four[ 19]), "_",as.character(sub_four[ 20]), "_",as.character(sub_four[ 21]), "_",as.character(sub_four[ 22]), "_",as.character(sub_four[ 23]), "_",as.character(sub_four[ 24]), "_",as.character(sub_four[ 25]), "_",as.character(sub_four[ 26]), "_",as.character(sub_four[ 27]), "_",as.character(sub_four[ 28]), "_",as.character(sub_four[ 29]), "_",as.character(sub_four[ 30]), "_",as.character(sub_four[ 31]), "_",as.character(sub_four[ 32]), "_",as.character(sub_four[ 33]), "_",as.character(sub_four[ 34]), ".R",as.character(sub_four[ 35]))
load(sea.DT_name )
sea.DT 	<- myOut ## S + E + A + D + T

names(sea)
#### These results are plotted across 4 different pages, each with their own pdf call

##################################################################
# PAGE 1 
# Landscape layout with the distribution of input parameters on the top row, maps of the final arrangement of points for each model in the middle, and an example tree using the mean parameter values for each model.


getwd()
#pdf( file = paste0("~/Box Sync/colliding ranges/Simulations_humans/Figures/Time sequence for movie/", j,".pdf"), width = 8.5, height = 11) # start page one, this command ends when the command dev.off() is called.
jpeg( file = paste0("~/Box Sync/colliding ranges/Simulations_humans/Figures/Time sequence for movie/", j,".jpg"), width = 12, height = 14, units="in", res=1080 , quality=100)
# The layout function establishes the grid background for plots to be plotted to. 
# This layout should contain a boarder around the periphery for formatting adjustment later. 
# This layout should contain blank rows and columns between primary plot boxes for later formatting. 

# This plot needs to house 12 plot boxes (4 x 3). Adding a margin box between each of those boxes and around the periphery defines a 9 x 9. The first three plot boxes (and the two margin boxes seperating them) in the first row will be joined together into a single plot box.
# Matrix arrangement:
# 1, 1, 1, 1, 1, 1, 1, 1, 1,     						# top margin
# 2, 3, 3, 3, 3, 3, 4, 5, 6, 							# top row
# 7, 7, 7, 7, 7, 7, 7, 7, 7, 							# first internal margin
# 8, 9, 9, 9, 9, 9, 10, 11 ,12,   					# second row
# 13, 13, 13, 13, 13, 13, 13, 13, 13, 		# second internal margin
# 14, 15, 15, 15, 15, 15, 16, 17, 18,  		# third row
# 19, 19, 19, 19, 19, 19, 19, 19, 19, 		# third internal margin
# 20, 21, 21, 21, 21, 21, 22, 23, 24,  		# fourth row
# 25, 25, 25, 25 25, 25, 25, 25, 25,  		# fourth margin
# 26, 27, 27, 27, 27, 27, 28, 29, 30, 
#  31, 31, 31, 31, 31, 31, 31, 31, 31, 				# bottom row	# bottom margin



#page_one_layout_matrix <- matrix(c( rep( 1, 9 ), 2, rep( 3, 5 ), 4:6, rep( 7,  9 ), 8, rep(9, 5), 10:12, rep( 13, 9), 14, rep( 15, 5 ), 16:18, rep( 19, 9 ), 20, rep( 21, 5 ), 22:24, rep( 25, 9 ), 26, rep(27, 5), 28:30, rep( 31, 9 ) ), 11, 9, byrow=TRUE)

# Specify the layout. Alternate the width calls so that margins are different between plot boxes and margin boxes. 
#page_one_layout <-layout(page_one_layout_matrix, width=c( 0.1, 0.8, 0.1, 0.8, 0.01, 0.8, 0.1, 1.2, 0.1), height=c(0.5, 0.8, 0.2, 1, 0.1, 1, 0.1, 1, 0.1,1,0.1))    
#page_one_layout <-layout(matrix(seq(1:10),5,2, byrow=TRUE), width=c( 2,1), height=c(1,1,1,1))


#layout.show(page_one_layout) 
par(mar=c(0,0,0,0)) #set the default margin size within plot boxes to 0. This transfers the control of margin size to the width specified in the layout call.
par(mfrow=c(5,2))


### Row 1
#replicate( 2 , blankplot(c(0,0), c(0,0))) #fill the first to boxes with blank plots to skip over them.

# plot input parameter distributions
#input_parameters_plot(sea, sea.T, sea.D, sea.DT)
blankplot(c(0,0), c(0,0))


#blankplot(c(0,0), c(0,0))

# display date, replicated quantity, and timesteps in top right plot box
display_important_numbers_2(j*100)


#replicate( 3 , blankplot(c(0,0), c(0,0)))


### Row 2



maps_with_points_2(sea$myWorld)

#blankplot(c(0,0), c(0,0))


example_tree_2(sea$mytree, "black")

#replicate( 3 , blankplot(c(0,0), c(0,0)))
mtext("S + E + A", 2, -40)

maps_with_points_2(sea.D$myWorld)

#blankplot(c(0,0), c(0,0))



example_tree_2(sea.D $mytree, "black")
#replicate( 3 , blankplot(c(0,0), c(0,0)))
mtext("S + E + A + D", 2, -40)

maps_with_points_2(sea.T$myWorld)

#blankplot(c(0,10), c(0,0))



example_tree_2(sea.T$mytree, "black")
#replicate( 3 , blankplot(c(0,0), c(0,0)))
mtext("S + E + A + T", 2, -40)

maps_with_points_2(sea.DT$myWorld)

#blankplot(c(0,10), c(0,0))


example_tree_2(sea.DT $mytree, "black")
mtext("S + E + A + D + T", 2, -40)



dev.off()

}




