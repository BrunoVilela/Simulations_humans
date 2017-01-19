Binary_traits_on_map_and_tree <- function(trait_choice, tree_choice){

library(ape)
require(phytools)
	require(diversitree)
	#source("https://bioconductor.org/biocLite.R")
#biocLite("ggtree")
	library(ggtree)


require(spdep)
require(letsR)
require(raster)


tree_choice <- as.phylo(read.nexus("~/Box Sync/colliding ranges/Simulations_humans/Available trees/language_tree_from_MPI.tree"))

load('~/Box Sync/colliding ranges/Simulations_humans/Concatenated data tables/dplace_traits_as_binary.Rdata')
binary_cultures <- as.data.frame(rdata)
binary_cultures
iso_new_693_3 <- as.vector(tree_choice$tip.label)
names(iso_new_693_3) <- "iso_new_693_3"
class(iso_new_693_3)
tip_order <- match(iso_new_693_3, binary_cultures[,3], nomatch=NA) 
binary_cultures_tip_ordered <- binary_cultures[tip_order,]
row.names(binary_cultures_tip_ordered) <- 1:length(row.names(binary_cultures_tip_ordered))
head(binary_cultures_tip_ordered)
names(binary_cultures_tip_ordered)
names(binary_cultures_tip_ordered[9:36])

variable <- 1
tree_names <- binary_cultures_tip_ordered[, variable + 8]


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
 	
 	 	
	

		
	str(sea$mytree)
	this_tree <- tree_choice
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
	
	library(geiger)
	tree_list <- list(
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,
	tree_choice,

	tree_choice
	
	)
	class(tree_list) <- "multiPhylo"
	#plot.multiPhylo(tree_list)
	
#	plotTree( tree_choice, type="fan", no.margin = FALSE, show.tip.label = TRUE, edge.color = c("red", "blue", "green")[as.factor(binary_cultures_tip_ordered[,32])], add=FALSE)
	pdf("~/Desktop/test_tree.pdf", height=40, width=80)
length(8:36)
	trait_column <- 34
	p <- ggtree(tree_list, layout="circular")+ facet_wrap(~.id, ncol=7)

col_vector <- c("red", "blue", "green")[as.factor(unlist(c(binary_cultures_tip_ordered[, 9:36])))]
length(col_vector)

p + geom_tippoint(color= col_vector, shape=19, size=2) + geom_tiplab(aes(angle=angle), color= col_vector, size=.5) 

#p + scale_fill_manual(values=c("#E41A1C","#377EB8","#FC8D59")) + theme_tree()

dev.off()

}


pdf("~/Desktop/test_tree.pdf", height=40, width=80)

color_matrix <- as.data.frame(col_vector[1:1297])

p <- ggtree(tree_list[[1]], layout='circular', color="firebrick", 
           size=0.15, branch.length='none', right=T) + xlim(-30, NA) 
p2 <- gheatmap(p, color_matrix , width=1,  colnames=F) +
   scale_fill_manual(values=c("blue")) + theme_tree()
open_tree(p2, 20) %>% rotate_tree(10)

dev.off()



p <- ggplot(tree_list[[1]], aes(mpg, wt)) 




Binary_traits_on_map_and_tree(trait_choice, tree_choice)

