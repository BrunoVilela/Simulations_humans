
maps_with_points <- function(which_model){
require(spdep)
require(letsR)


 myWorld <-  as.data.frame(which_model[[5]][1])
  coords <- as.matrix(myWorld[, 2:3])
  nbs <- knn2nb(knearneigh(coords, k = 7, longlat = TRUE),
              sym = TRUE)
  
  map(mar=c(0,0,0,0))
  #mtext(text = myT)
  #plot(nbs, coords, add = TRUE, col = "gray80", lty = 3)
  col1 <- adjustcolor("firebrick", alpha = 1)
  col2 <-  adjustcolor("cornflowerblue", alpha = 1)
  points(coords, col = c(col2, col1)[myWorld[, 7]], pch = 1, cex=.08)
  
  col1 <- adjustcolor("firebrick", alpha = 1)
  col2 <-  adjustcolor("cornflowerblue", alpha = 1)
  points(coords, col = c(col2, col1)[myWorld[, 6]], pch = 18, cex=.25)
 
}

#maps_with_points(sea)

