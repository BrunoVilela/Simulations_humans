# Here we check how the results can predict the model


# List the result files
lista <- list.files("results cluster output/", full.names = TRUE)


# Functions
spatial_div <- function(x) {
  a <- x[[1]]
  spatial.for <- a$estimate[1] / a$estimate[2]
  b <- x[[2]]
  spatial.dom <- b$estimate[1] / b$estimate[2]
  return(setNames(c(spatial.for, spatial.dom), NULL))
} 


# Empty objects
n.groups <- length(lista)
replication <- numeric(n.groups)

data.analysis.comp <- NULL
dif.take <- NULL

# Resume the table to be analyzed
i = 1 # PLACE THE LOOP HERE
for (i in 1:n.groups) { 
  load(lista[[i]]) # Name returns
  replication[i] <- length(returns[[8]])
  dif.take <- rbind(dif.take, t(sapply(returns[[2]][returns[[6]]], function(x) strsplit(x, "_")[[1]][c(19, 23)])))
  
  # Analysis data table
  names.vars <- names(returns)
  pos.list <- c(10:12, 14:16, 18:21, 24, 28)
  names.vars.in <- names.vars[pos.list]
  n.names <- length(names.vars.in)
  names.added <- c("Model", "spatial.for", "spatial.dom",
                   "birth", "death", "death.bird.div",
                   "death.bird.sub", rownames(returns[[27]]))
  n.added <- length(names.added)
  total.cols <- n.added + n.names
  data.analysis <- matrix(ncol = total.cols, nrow = replication[i])
  
  colnames(data.analysis) <- c(names.added,
                               names.vars.in)
  # Model
  data.analysis[, 1] <- rep(as.numeric(strsplit(lista[[i]], "_")[[1]][5]), replication[i])
  
  # Spatial data = expected/observed
  spatial.data <- returns[[8]]
  data.analysis[, 2:3] <- t(sapply(spatial.data, spatial_div))
  
  # Macroevolutionary
  data.analysis[, 4:7] <- t(returns[[26]])
  data.analysis[, 8:13] <- t(returns[[27]])
  
  # General variables
  data.analysis[, (n.added + 1):total.cols] <- do.call(cbind, returns[pos.list])
  data.analysis.comp <- rbind(data.analysis.comp, data.analysis)
}

# Remove everybody
#rm(list = ls()[ls() != "data.analysis.comp"])



# PCA
PCAdata <- data.analysis.comp[, -1]

ev <- eigen(cor(PCAdata)) # get eigenEstimates
ap <- parallel(subject = nrow(PCAdata), var = ncol(PCAdata),
               rep = 100, cent = .05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) # this suggests we stick with ONE 

myPCA <- princomp(PCAdata, cor = TRUE)

# plot
library(plot3D)
plus <- 10
loadings <- cor(PCAdata, myPCA$scores[, 1:3])
layout(matrix(c(1, 1, 2, 1, 1, 3, 1, 1, 4, 1, 1, 4), nrow = 4, ncol = 3, byrow = TRUE))
par(mar = c(2, 1, 1, 1))
scatter3D(x = log(myPCA$scores[, 1] + plus), 
          y = log(myPCA$scores[, 2] + plus),
          z = log(myPCA$scores[, 3] + plus), ylab = "PCA2",
          xlab = "PCA1", zlab = "PCA3", cex = 2 * ((as.numeric(dif.take[, 1]) + 1)/(as.numeric(dif.take[, 2]) + 1)), 
          phi = 20, theta = -40, colvar = data.analysis.comp[, 1],
          pch = 20, alpha = .4, colkey = T)
par(mar = c(1, 2, 1, 1))
x <- barplot(loadings[, 1], xaxt = "n", main = "PC1")
x <- barplot(loadings[, 2], xaxt = "n", main = "PC2")
par(mar = c(12, 2, 1, 1))
x <- barplot(loadings[, 3], xaxt = "n", main = "PC3")
text(cex = 1, x = x + .4, y = -1, 
     rownames(loadings), xpd = TRUE, srt = 45, pos = 2)



# Random forests
library(randomForest)
n <- nrow(data.analysis.comp2)
sub <- 1:n %in% sample(1:n, n) 
data.analysis.comp2 <- data.frame("Model" = as.factor(data.analysis.comp[, 1]), myPCA$scores)
fit <- randomForest(Model ~ .,
                    data=data.analysis.comp2[sub, ], 
                    importance=TRUE, 
                    ntree=2000)
plot(fit)
varImpPlot(fit)
sum(data.analysis.comp2[!sub, 1] == predict(fit, data.analysis.comp2[!sub, -1])) /
  sum(!sub)

