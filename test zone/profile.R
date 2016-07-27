


#####################################################################
rm(list = ls())
library(profvis)
library(gtools)
library(ape)
library(adephylo)
library(diversitree)
library(TotalCopheneticIndex)
library(phytools)
library(apTreeshape)
library(plyr)
library(fitdistrplus)
library(geiger)
library(caper)
library(msm)
library(spdep)
library(parallel)
library(phylobase)


load.files <- list.files(path = "Functions", pattern = ".R",
                         full.names = TRUE)
for (i in 1:length(load.files)) {
  source(load.files[i])
}

coords <- as.matrix(read.csv("Functions/coords.csv", row.names = 1))
conds <- as.matrix(read.csv("Functions/suitability.csv", row.names = 1))
conds <- ifelse(conds <= 21, 1, 2)
conds[is.na(conds)] <- sample(c(1, 2), sum(is.na(conds)), replace = TRUE) 
sub <- sample(1:nrow(coords), 200) # subsample (remove when running for all)

myWorld <- BuildWorld(coords[sub, ], conds[sub, ])
nbs <- knn2nb(knearneigh(coords[sub, ], k = 7, longlat = TRUE),
              sym = TRUE) # 7 symmetric neighbors
dim(myWorld)
number_of_time_steps <- 300 ## these are for testing the function, not for the main code
replicate_cycle <- 3
combo_number <- 31

chosen_combo <- combo_of_choice(combo_number, FALSE)

if (any(chosen_combo[[2]] == "Speciate")) {
  prob_choose <- as.numeric(formatC(rtnorm(1, mean = .5, sd =.05, lower = 0, upper = 1), width = 3,flag = 0, digits=2))  #prob speciation
  P.speciation <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom")
} else {
  P.speciation <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
}

if (any(chosen_combo[[2]] == "Extinct")) {
  prob_choose <- as.numeric(formatC(rtnorm(1, mean = .05, sd =.05, lower = 0, upper = 1), width = 3,flag = 0, digits=2)) #prob of extinction
  P.extinction  <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom")
  P.extinction["For", "Dom"] <- 0.4
} else {
  P.extinction  <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
}


if (any(chosen_combo[[2]] == "Random_new_origin")) {
  prob_choose_a <- as.numeric(formatC(rtnorm(1, mean = .05, sd =.01, upper=1, lower=0), width = 3,flag = 0)) # prob of Arisal
  P.Arisal <- matrix(prob_choose_a, ncol = 2, nrow = nrow(myWorld)) # probability per cell
  P.Arisal[myWorld[, 7] == 1, 2] <- 0 # probability of agriculture is zero in non-suitable places
} else {
  P.Arisal <- matrix(0, ncol = 2, nrow = nrow(myWorld))
}
colnames(P.Arisal) <- c("Prob_of_Foraging", "Porb_of_Domestication")


if (any(chosen_combo[[2]] == "Diffusion")) {
  prob_choose <- as.numeric(formatC(rtnorm(1, mean = .2, sd =.2, upper=1, lower=0.05), width = 3,flag = 0, digits=2)) #prob of diffusion
  P.diffusion <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom") 
} else {
  P.diffusion <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
}

if (any(chosen_combo[[2]] == "Takeover")) {
  prob_choose <- as.numeric(formatC(rtnorm(1, mean = .2, sd =.2, upper=1, lower=0.05), width = 3,flag = 0, digits=2)) #prob of takeover
  P.TakeOver <- parameters(prob_choose, prob_choose, prob_choose, prob_choose, "For", "Dom", "For", "Dom")
} else {
  P.TakeOver <- parameters(0, 0, 0, 0, "For", "Dom", "For", "Dom")
}



profvis({
  myOut <- RunSimUltimate(myWorld, P.extinction, P.speciation, 
                          P.diffusion, P.Arisal, P.TakeOver, nbs,
                          N.steps = number_of_time_steps, silent = F, 
                          multiplier = 2)
})



map()
plot(nbs, coords[sub, ], add = TRUE, col = "gray80", lty = 3)
points(coords[sub, ], col = c("blue", "red")[conds[sub, ]])
points(coords[sub, ], col = c("blue", "red")[myOut$myWorld[, 6]], pch = 20)
myOut$myWorld[, 8] <- paste0("t", myOut$myWorld[, 8])
match
plot(myOut$mytree, edge.color = c("blue", "red")[myOut$myWorld[, 6]],
     show.tip.label = FALSE)
