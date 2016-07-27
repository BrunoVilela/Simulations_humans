draw_point_top_hex <- function(center_x, center_y, size, i){
	angle_deg <- (60 * i) + 30
	angle_rad <- pi/180 * angle_deg
	point <- c(center_x + size * cos(angle_rad), center_y + size *sin(angle_rad))
	
	return(point)
	
} 

plot_single_pointed_hex <- function(center_x, center_y, size){
point_1 <- draw_point_top_hex(center_x, center_y, size, 1)
point_2 <- draw_point_top_hex(center_x, center_y, size, 2)
point_3 <- draw_point_top_hex(center_x, center_y, size, 3)
point_4 <- draw_point_top_hex(center_x, center_y, size, 4)
point_5 <- draw_point_top_hex(center_x, center_y, size, 5)
point_6 <- draw_point_top_hex(center_x, center_y, size, 6)


lines(c(point_1[[1]], point_2[[1]]), c(point_1[[2]], point_2[[2]]))
lines(c(point_2[[1]], point_3[[1]]), c(point_2[[2]], point_3[[2]]))
lines(c(point_3[[1]], point_4[[1]]), c(point_3[[2]], point_4[[2]]))
lines(c(point_4[[1]], point_5[[1]]), c(point_4[[2]], point_5[[2]]))
lines(c(point_5[[1]], point_6[[1]]), c(point_5[[2]], point_6[[2]]))
lines(c(point_6[[1]], point_1[[1]]), c(point_6[[2]], point_1[[2]]))

}

full_center_x <- 0
full_center_y <- 0
standard_size <- 1

X<-seq(-10,20,by=1) # create coordinates vectors X and Y
Y<-seq(35,65,by=1)

xy <- expand.grid(X=X,Y=Y)

plot(0,0,xlim=c(0,100), ylim=c(0,100), type="n")
plot_single_pointed_hex(full_center_x, full_center_y, standard_size)


pointed_hex_height <- standard_size * 2
pointed_hex_width <- pointed_hex_height * sqrt(3)/2


for(j in 1:10){
plot_single_pointed_hex(full_center_x + (pointed_hex_width * j), full_center_y + 0, standard_size)
}

for(w in 1:10){
plot_single_pointed_hex(full_center_x + (pointed_hex_width/2) + (pointed_hex_width * w) , full_center_y + ((pointed_hex_height * (3/4))), standard_size)
}

#########


library(plotrix)

full_center_x <- 0
full_center_y <- 0

standard_size <- 1
pointed_hex_height <- standard_size * 2
pointed_hex_width <- pointed_hex_height * sqrt(3)/2


x_hex_coords <- NA
y_hex_coords <- NA


even_row_x <- full_center_x + (pointed_hex_width * 0:100)
odd_row_x <- full_center_x + (pointed_hex_width/2) + (pointed_hex_width * 0:100)
w <- seq(1,100,by= 2* ((pointed_hex_height * (3/4))))

for( z in 1:50){
	x_hex_coords <- rbind(x_hex_coords, full_center_x + (pointed_hex_width * 1:100))
x_hex_coords <- rbind(x_hex_coords, full_center_x + (pointed_hex_width/2) + (pointed_hex_width * 1:100))
}

x_hex_coords <- x_hex_coords[-1,]
a <- rbind(rep(x_hex_coords,50))
dim(a)

y_hex_coords <- matrix( rep(seq(1,150,by= ((pointed_hex_height * (3/4)))),100), 100,100, byrow=FALSE)
head(y_hex_coords)
dim(x_hex_coords)


transformed_x <- x_hex_coords - 100
transformed_y <- y_hex_coords - 100










setwd("~/Desktop")
pdf(file="gexgrid.pdf", width=8, height=8)

par(cex=.2)
plot(0,0,xlim=c(-500,500), ylim=c(-500,500), type="n")
#points(x_hex_coords, y_hex_coords, col="lightgrey")



a <- draw.circle(0,0,30,nv=100,border=NULL,col=NA,lty=1,lwd=1)


library(sp)
in_points <- point.in.polygon(transformed_x, transformed_y,a$x,a$y)




new_x <- transformed_x * in_points
new_y <- transformed_y * in_points
new_x[which(new_x == 0 )] <- NA




#points(new_x, new_y, col="green")

#points(new_x[30,30], new_y[30,30], col="red", pch=19)
## using 'odd-r' horizontal layout
for(recurc in seq(1,10, length.out=10)){


points(new_x * recurc, new_y * recurc, col=rainbow(10)[recurc])
}

dev.off()

## lets transform the final matrix rather than change the computing function. 

 
# Find what happens when you rotate (2, 0, 0) by 45 degrees about the y axis:
 


#####################################################
x_cube <- 50
z_cube <- 50
y_cube <- -x_cube - z_cube


# convert cube to odd-r offset
col <-  x + (z - (z&1)) / 2
row <- z

col <- 50
row <- 50
# convert odd-r offset to cube
x <- col - (row - (row&1)) / 2
z <- row
y = -x-z



#####################################################
library(hexbin)


aa <- hgridcent(100, c(1,10), c(1,10), 1, edge.add = 0)
 list2hexList(aa)
 hcell2xyInt(aa)
 
 
 
plot(0,0,xlim=c(-1,14), ylim=c(-1,14), type="n") 
aa <- hexGraphPaper(xbins = 11, xbnds = c(2,10), ybnds = c(2,10), add=FALSE, edge.add = 0) 
 points(aa$x, aa$y, col="blue", cex=.1)
 
 

 jk <-  which(bb$i == 5 & bb$j == 5)
 points(aa$x[bb$i], aa$y[bb$j], col="red", cex=.5, pch=19)
 
 
for(u in 1:10){
 jk <-  which(bb$i == 5 & bb$j == 5)
 points(aa$x[jk], aa$y[jk], col="green", cex=.5, pch=19)
 }
 
  
bb <- hcell2xyInt(xbins = 10, xbnds = c(1,10), ybnds = c(1,10), shape=aa$dy/aa$dx)
plot(0,0,xlim=c(-1,14), ylim=c(-1,14), type="n") 
points(bb$i, bb$j, col="red", cex=.5, pch=19)

point_index <- which(bb$i == 50 & bb$j ==50)
aa$x[point_index]
aa$y[point_index]
class(bb)
points(aa$x[point_index], aa$y[point_index], col="red")


 plot(aa$x, aa$y, col="red", cex=.3)

#for(h in 1:500){
pointer <- 90
#points(aa$x[pointer], aa$y[pointer], col="blue")
#hexagon(aa$x[pointer] + (aa$dx/2), aa$y[pointer] + (aa$dy), aa$dx,  col="blue")
#}

plot_single_pointed_hex(aa$x[pointer] , aa$y[pointer] , aa$dx)
plot_single_flat_hex(aa$x[pointer] , aa$y[pointer] , aa$dx)


#####################################################











draw_flat_top_hex <- function(center_x, center_y, size, i){
	angle_deg <- 60 * i + 30
	angle_rad <- pi/180 * angle_deg
	point <- rev(c(center_x + size * cos(angle_rad), center_y + size *sin(angle_rad)))
	
	return(point)
	
} 

plot_single_flat_hex <- function(center_x, center_y, size){
point_1 <- draw_flat_top_hex(center_x, center_y, size, 1)
point_2 <- draw_flat_top_hex(center_x, center_y, size, 2)
point_3 <- draw_flat_top_hex(center_x, center_y, size, 3)
point_4 <- draw_flat_top_hex(center_x, center_y, size, 4)
point_5 <- draw_flat_top_hex(center_x, center_y, size, 5)
point_6 <- draw_flat_top_hex(center_x, center_y, size, 6)

#plot(0,0,xlim=c(-30,30), ylim=c(-30,30), type="n")
lines(c(point_1[[1]], point_2[[1]]), c(point_1[[2]], point_2[[2]]))
lines(c(point_2[[1]], point_3[[1]]), c(point_2[[2]], point_3[[2]]))
lines(c(point_3[[1]], point_4[[1]]), c(point_3[[2]], point_4[[2]]))
lines(c(point_4[[1]], point_5[[1]]), c(point_4[[2]], point_5[[2]]))
lines(c(point_5[[1]], point_6[[1]]), c(point_5[[2]], point_6[[2]]))
lines(c(point_6[[1]], point_1[[1]]), c(point_6[[2]], point_1[[2]]))

}

full_center_x <- 0
full_center_y <- 0
standard_size <- 10


plot_single_flat_hex(full_center_x, full_center_y, size)

