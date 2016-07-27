## Scaling hexgrid

## Ty Tuff
## Washington University in St. Louis
## 14 June 2016

## This code creates a hex grid for simulations. After making the template grid, it scales the grid to the correct resolution and then crops the grid to fit inside a polygon defining the spatial extent. 

## to-do: The cropping polygon is currently define as a generic shape. I need to import map vectors to do the clipping

#####################################################################
##### Load packages ####

library(plotrix)
library(sp)

#####################################################################
##### Initialize parameters ####

x_hex_coords <- NA
y_hex_coords <- NA

## set the starting criteria for creating the base grid. This should be the origin or the math is unstable. Standard size should always be 1 here. 
full_center_x <- 0
full_center_y <- 0
standard_size <- 1
grid_x_length <- 1:1000
grid_y_length <- 1000

## These values shift the grid left and down so that it's centered on the origin. This means that the transform should be half the value of the total grid size. 
x_transform <- -100
y_transform <- -100

## set the relative proportions of individual hexagons
pointed_hex_height <- standard_size * 2
pointed_hex_width <- pointed_hex_height * sqrt(3)/2

## Set the conditions for template rows and the number of repeats of template rows
even_row_x <- full_center_x + (pointed_hex_width * grid_x_length)
odd_row_x <- full_center_x + (pointed_hex_width/2) + (pointed_hex_width * grid_x_length)
w <- seq(1, grid_y_length, by= 2* ((pointed_hex_height * (3/4))))

# set grid resolution and point size
scaling_factor <- 5
par(cex=.1)

## set radius of cropping circle
circle_radius <- 300

#####################################################################
##### Create grid ####

grid_height <- 
grid_width <- 

## using 'odd-r' horizontal layout
for( z in 1:50){
	x_hex_coords <- rbind(x_hex_coords, full_center_x + (pointed_hex_width * grid_x_length))
	x_hex_coords <- rbind(x_hex_coords, full_center_x + (pointed_hex_width/2) + (pointed_hex_width * grid_x_length))
	}

x_hex_coords <- x_hex_coords[-1,]
y_hex_coords <- matrix( rep(seq(1,150,by= ((pointed_hex_height * (3/4)))),100), 100,100, byrow=FALSE)

plot(x_hex_coords, y_hex_coords)
#####################################################################
##### Center grid on origin ####

transformed_x <- x_hex_coords + x_transform
transformed_y <- y_hex_coords + y_transform

plot(transformed_x, transformed_y)
#####################################################################
##### Scale grid to resolution ####

scaled_x <- transformed_x * scaling_factor
scaled_y <- transformed_y * scaling_factor

plot(scaled_x,scaled_y)
#####################################################################
##### Create or import polygon for cropping ####
plot(0,0,xlim=c(-500,500), ylim=c(-500,500), type="n")
a <- draw.circle(0,0, circle_radius,nv=100,border=NULL,col=NA,lty=1,lwd=1)

#####################################################################
##### Crop grid using polygon ####
in_points <- point.in.polygon(scaled_x, scaled_y,a$x,a$y)
new_x <- scaled_x * in_points
new_y <- scaled_y * in_points
new_x[which(new_x == 0 )] <- NA

#####################################################################
##### Print cropped grid to PDF ####

setwd("~/Desktop")
pdf(file="gexgrid.pdf", width=8, height=8)

plot(0,0,xlim=c(-500,500), ylim=c(-500,500), type="n")
points(new_x, new_y, col="green")

dev.off()

