load('~/Box Sync/colliding ranges/Simulations_humans/Concatenated data tables/dplace_traits_as_binary.Rdata')
 
 
 load('~/Desktop/dplace.Rdata')
binary_cultures <- as.data.frame(rdata)
binary_cultures

names(binary_cultures)
head(binary_cultures)


par(mfrow=c(4,8), mar=c(2,0,4,0))

for(i in 9:36){
	hist(na.omit(as.numeric(binary_cultures[,i])), xlim=c(-1,2), ylim=c(0,1200), main=names(binary_cultures)[i])
	
}

#####################################################################
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/PLOT_blankplot.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/PLOT_display_important_numbers.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/PLOT_example_tree.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/PLOT_maps_with_points.R', chdir = TRUE)
source('~/Box Sync/colliding ranges/Simulations_humans/Functions/PLOT_input_parameter_distributions.R', chdir = TRUE)


getwd()
#pdf( file = paste0("~/Box Sync/colliding ranges/Simulations_humans/Figures/Time sequence for movie/", j,".pdf"), width = 8.5, height = 11) # start page one, this command ends when the command dev.off() is called.
jpeg( file = paste0("~/Box Sync/colliding ranges/Simulations_humans/Figures/Time sequence for movie 2/", j,".jpg"), width =8.5, height = 6, units="in", res=1000 , quality=100)


try(maps_with_points_2(sea$myWorld), silent=TRUE)

resolution1 <-  round(exp(seq(log(1), log(30000), length.out = 750)))
    resolution <- resolution1[!duplicated(resolution1)]
    

mtext(paste0("Timestep  ", resolution[j]), 3, adj=-0.25, line=-2, font=2)



#blankplot(c(0,0), c(0,0))
polygon(x= c(-180,-180,180,180), y=c(-100,-60,-60,-100), col="white", border="white")

mtext("Basic model", 2, -50, cex=1.5, adj= 0.5)

try(example_tree_2(sea $mytree, "black", j), silent=TRUE)


#replicate( 3 , blankplot(c(0,0), c(0,0)))




try(maps_with_points_2(sea.D$myWorld), silent=TRUE)

#blankplot(c(0,0), c(0,0))
polygon(x= c(-180,-180,180,180), y=c(-100,-60,-60,-100), col="white", border="white")


mtext("+  Diffusion", 2, -50, cex=1.5, adj=0.5)

try(example_tree_2(sea.D $mytree, "black", j), silent=TRUE)

#replicate( 3 , blankplot(c(0,0), c(0,0)))

try(maps_with_points_2(sea.T$myWorld), silent=TRUE)

#blankplot(c(0,10), c(0,0))


polygon(x= c(-180,-180,180,180), y=c(-100,-60,-60,-100), col="white", border="white")
mtext("+ Takeover", 2, -50, cex=1.5, adj=0.5)

try(example_tree_2(sea.T $mytree, "black", j), silent=TRUE)

#replicate( 3 , blankplot(c(0,0), c(0,0)))

try(maps_with_points_2(sea.DT$myWorld), silent=TRUE)

#blankplot(c(0,10), c(0,0))

polygon(x= c(-180,-180,180,180), y=c(-100,-60,-60,-100), col="white", border="white")
mtext("+ Diffusion \n+ Takeover", 2, -50, cex=1.5, adj=0.5)
mtext(paste0("Created by Ty Tuff, Bruno Vilela, and Carlos Botero at WUSTL on ", format(Sys.time(), format ="%d-%b-%Y")), 1, adj=1, line=-1.5)
try(example_tree_2(sea.DT$mytree, "black", j), silent=TRUE)




dev.off()

}


prob_choose <- c(0.5, 0.5, 0.5, 0.4, 0.15, 0.4, 0.15, 0.1, 0.1, 0.1, 0.2, 0.2)
