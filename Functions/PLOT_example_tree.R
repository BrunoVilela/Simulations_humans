

example_tree <- function(which_model, color){
	require(phytools)
	require(diversitree)
	
	tree_of_choice <- which_model[[3]]
	# plot(tree_of_choice[[15]], type="fan")
	plot(tree_of_choice[[15]], type="fan", no.margin = TRUE, show.tip.label = FALSE, edge.col= color)
	#, edge.color = c("red", "blue", "grey")[as.data.frame(sea[[5]][1])$Trait])
	
}

#example_tree(sea)

example_tree_2 <- function(tree_of_choice, color){
	require(phytools)
	require(diversitree)
	
	#tree_of_choice <- which_model[[3]]
	# plot(tree_of_choice[[15]], type="fan")
	plot(tree_of_choice, type="fan", no.margin = TRUE, show.tip.label = FALSE, edge.col= color)
	#, edge.color = c("red", "blue", "grey")[as.data.frame(sea[[5]][1])$Trait])
	
}
