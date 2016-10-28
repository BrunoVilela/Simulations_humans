

example_tree <- function(which_model, color){
	require(phytools)
	require(diversitree)
	
	tree_of_choice <- which_model[[3]]
	# plot(tree_of_choice[[15]], type="fan")
	plot(tree_of_choice[[15]], type="fan", no.margin = TRUE, show.tip.label = FALSE, edge.col= color)
	#, edge.color = c("red", "blue", "grey")[as.data.frame(sea[[5]][1])$Trait])
	
}

#example_tree(sea)
example_tree_2 <- function(tree_of_choice, color, timestep){
	require(phytools)
	require(diversitree)
	str(sea$mytree)
	this_tree <- sea$mytree
	tip_count <- length(this_tree$tip.label)
	
	small_mar <- 15
	large_mar <- 0
	
	count_vector <- seq(1,1255, by=1)/1255 *15
	
	
	this_mar <- seq(start_mar, end_mar, length.out=300)
	
	par(mar=c(this_mar[timestep], this_mar[timestep], this_mar[timestep], this_mar[timestep]))
	#tree_of_choice <- which_model[[3]]
	# plot(tree_of_choice[[15]], type="fan")
	
	plotTree(tree_of_choice, type="fan", no.margin = FALSE, show.tip.label = FALSE, edge.col= color, add=TRUE, fsize=0.01, mar= c(that_mar[timestep],1,this_mar[timestep],48.5))
	#, edge.color = c("red", "blue", "grey")[as.data.frame(sea[[5]][1])$Trait])
	
}


