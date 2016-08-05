

example_tree <- function(which_model){
	require(phytools)
	require(diversitree)
	
	tree_of_choice <- which_model[[3]]
	# plot(tree_of_choice[[15]], type="fan")
	plot(tree_of_choice[[15]], type="fan", no.margin = TRUE, show.tip.label = FALSE, edge.color = c("red", "blue")[trait_type], outline=FALSE)
	
}

#example_tree(sea)