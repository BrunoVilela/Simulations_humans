

example_tree <- function(which_model, color){
	require(phytools)
	require(diversitree)
	
	tree_of_choice <- which_model[[3]]
	# plot(tree_of_choice[[15]], type="fan")
	plot(tree_of_choice[[15]], type="fan", no.margin = TRUE, show.tip.label = FALSE, edge.col= color)
	#, edge.color = c("red", "blue", "grey")[as.data.frame(sea[[5]][1])$Trait])
	
}
color="black"

#tree_of_choice <- sea$mytree
#example_tree(sea)
example_tree_2 <- function(tree_of_choice, color, timestep){
	require(phytools)
	require(diversitree)
	str(sea$mytree)
	this_tree <- tree_of_choice
	tip_count <- length(this_tree$tip.label)
	
	small_mar <- 15
	large_mar <- 0.01
	
	count_vector <- rev(seq(large_mar, small_mar, length.out=1256))
	this_adjust <- 0
	try(this_adjust <- floor(count_vector[tip_count]), silent=TRUE)
	
	scaler <- 2
	up_down <- rev((seq(900,1255, length.out=385)/1255) * scaler)[tip_count]
	#tree_of_choice <- which_model[[3]]
	# plot(tree_of_choice[[15]], type="fan")
	
	plotTree(this_tree, type="fan", no.margin = FALSE, show.tip.label = FALSE, edge.col= color, add=TRUE, fsize=0.01, mar= c(0 ,0, this_adjust ,45.5))
	#, edge.color = c("red", "blue", "grey")[as.data.frame(sea[[5]][1])$Trait])
	
}


