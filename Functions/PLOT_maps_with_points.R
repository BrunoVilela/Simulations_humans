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

