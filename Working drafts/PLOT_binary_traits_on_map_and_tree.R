Binary_traits_on_map_and_tree <- function(trait_choice, tree_choice){
load('~/Box Sync/colliding ranges/Simulations_humans/Concatenated data tables/dplace_traits_as_binary.Rdata')
binary_cultures <- as.data.frame(rdata)
binary_cultures

require(spdep)
require(letsR)
require(raster)

par(mar=c(0,12,0,0))
r <- raster("~/Box Sync/colliding ranges/Simulations_humans/Functions/richnobuf.asc")
#plot(r)
plot(r, col = gray.colors(100, start = 0.7, end = 0.2), interpolate=TRUE, box = FALSE, axes = FALSE, legend = FALSE)

 #myWorld <-  as.data.frame(which_model[[5]][1])
  coords <- as.matrix(cbind(as.numeric(binary_cultures[,8]), as.numeric(binary_cultures[,7])))
 # nbs <- knn2nb(knearneigh(coords, k = 7, longlat = TRUE),
  #            sym = TRUE)
  
 
  col1 <- adjustcolor("red", alpha = .7)
  col2 <-  adjustcolor("gold", alpha = .7)
  col3 <- adjustcolor("white", alpha = 0)
  binary_cultures[which(is.na(binary_cultures[,10]) == TRUE) ,10]<- 3
  points(coords, col = c(col2, col1, col3)[binary_cultures[,10]], pch = 19, cex=1.2)
 	
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




