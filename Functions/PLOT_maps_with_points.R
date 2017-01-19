maps_with_points <- function(which_model){
require(spdep)
require(letsR)
require(raster)

r <- raster("~/Box Sync/colliding ranges/Simulations_humans/Functions/richnobuf.asc")
plot(r)

 myWorld <-  as.data.frame(which_model[[5]][1])
  coords <- as.matrix(myWorld[, 2:3])
  nbs <- knn2nb(knearneigh(coords, k = 7, longlat = TRUE),
              sym = TRUE)
  
  #map(mar=c(0,0,0,0), interior=FALSE)
  #mtext(text = myT)
  #plot(nbs, coords, add = TRUE, col = "gray80", lty = 3)
  col1 <- adjustcolor("firebrick", alpha = 1)
  col2 <-  adjustcolor("cornflowerblue", alpha = 1)
  points(coords, col = c(col2, col1)[myWorld[, 7]], pch = "O", cex=.28)
  
  col1 <- adjustcolor("firebrick", alpha = 1)
  col2 <-  adjustcolor("cornflowerblue", alpha = 1)
  col3 <- adjustcolor("limegreen", alpha = 1)
  myWorld[which(is.na(myWorld[,6]) == TRUE) ,6]<- 3
  points(coords, col = c(col2, col1, col3)[myWorld[, 6]], pch = 18, cex=.40)
 
}

#maps_with_points(sea)


maps_with_points_2 <- function(myWorld){
require(spdep)
require(letsR)
require(raster)

load('~/Box Sync/colliding ranges/Simulations_humans/Functions/richness.RData')
par(mar=c(0,12,0,0))
#r <- raster("~/Box Sync/colliding ranges/Simulations_humans/Functions/richnobuf.asc")
#plot(r)
plot(rich, col = gray.colors(100, start = 0.7, end = 0.2), interpolate=TRUE, box = FALSE, axes = FALSE, legend = FALSE)

 #myWorld <-  as.data.frame(which_model[[5]][1])
  coords <- as.matrix(myWorld[, 2:3])
  nbs <- knn2nb(knearneigh(coords, k = 7, longlat = TRUE),
              sym = TRUE)
  
  #map(mar=c(0,0,0,0), interior=FALSE)
  #mtext(text = myT)
  #plot(nbs, coords, add = TRUE, col = "gray80", lty = 3)
  col1 <- adjustcolor("white", alpha = 0)
  col2 <-  adjustcolor("white", alpha = 0)
  points(coords, col = c(col2, col1)[myWorld[, 7]], pch = 19, cex=1)
  
  col1 <- adjustcolor("red", alpha = .7)
  col2 <-  adjustcolor("gold", alpha = .7)
  col3 <- adjustcolor("white", alpha = 0)
  myWorld[which(is.na(myWorld[,6]) == TRUE) ,6]<- 3
  points(coords, col = c(col2, col1, col3)[myWorld[, 6]], pch = 19, cex=1.2)
 	
 	 
}




maps_with_points_3 <- function(myWorld){
require(spdep)
require(letsR)
require(raster)

load('~/Box Sync/colliding ranges/Simulations_humans/Functions/richness.RData')
par(mar=c(0,5,6,0))
#r <- raster("~/Box Sync/colliding ranges/Simulations_humans/Functions/richnobuf.asc")
#plot(r)
plot(rich, col = gray.colors(100, start = 0.7, end = 0.2), interpolate=TRUE, box = FALSE, axes = FALSE, legend = FALSE)

 #myWorld <-  as.data.frame(which_model[[5]][1])
  coords <- as.matrix(myWorld[, 2:3])
  nbs <- knn2nb(knearneigh(coords, k = 7, longlat = TRUE),
              sym = TRUE)
  
  #map(mar=c(0,0,0,0), interior=FALSE)
  #mtext(text = myT)
  #plot(nbs, coords, add = TRUE, col = "gray80", lty = 3)
  col1 <- adjustcolor("white", alpha = 0)
  col2 <-  adjustcolor("white", alpha = 0)
  #points(coords, col = c(col2, col1)[myWorld[, 7]], pch = 19, cex=1)
  
  col1 <- adjustcolor("red", alpha = 1)
  col2 <-  adjustcolor("gold", alpha = 1)
  col3 <- adjustcolor("white", alpha = 0)
  myWorld[which(is.na(myWorld[,6]) == TRUE) ,6]<- 3
  #points(coords, col = c(col2, col1, col3)[myWorld[, 6]], pch = 19, cex=1)
 points(30,-20, col=col1 , pch = 19, cex=1)
 points(-120,50, col=col2 , pch = 19, cex=1)
}




