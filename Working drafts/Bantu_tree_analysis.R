# Packages
library(FARM)
library(phytools)
library(randomForest)

# Complete dataset for other traits
load("~/Box Sync/Bruno/WUSTL projects/Simulations_humans/Concatenated data tables/FULL_TREE_Society_data_with_binary_conversions.Rdata")# 8 should be 1

# Real Bantu tree
load("~/Box Sync/Bruno/WUSTL projects/Simulations_humans/Available trees/Tree_Bantu.RData")

# Match
match.bant <- match(bantu_tree$tip.label, 
                    Society_data_with_binary_conversions$Society.id)
mydata <- Society_data_with_binary_conversions[match.bant, ]
myWorld <- as.data.frame(BuildWorld(mydata[, c(9, 8)], conditions = 1))
myWorld[, 6] <- mydata[, 37]
# myWorld[, 6] <- c(rep(1, 14), rep(0, 54), rep(1, 13))
myWorld[, 8] <- mydata[, 1]

remove <- which(is.na(myWorld[, 6]))
mytree <- drop.tip(bantu_tree, remove)
# remove2 <- which(myWorld[, 8] %in% c("Ah3", "Af18"))
# mytree <- drop.tip(bantu_tree, remove2)
# myWorld <- myWorld[-remove2, ]
myWorld <- myWorld[-remove, ]
# Plot
cols <- c("blue", "red")
plotTree(mytree, ftype = "i", fsize = 0.5)
tiplabels(pie = myWorld[, 6], piecol = cols, cex = 0.3)

# Analysis
mytree$tip.label <- myWorld[, 8] <- paste0("t", 1:Ntip(mytree))
myWorld[, 6] <- myWorld[, 6] + 1
class(mytree) <- "phylo"
myOut <- list("myWorld" = myWorld, "mytree" = mytree)
resu.bantu <- Module_2(myOut)

# Predictions
load("~/Box Sync/Bruno/WUSTL projects/Simulations_humans/Concatenated data tables/BANTU_Four_model_compare_background_TO_and_not.Rdata")
Concatenated_data <- Concatenated_data[Concatenated_data[, 2] == "no_background_takeover", ]
Concatenated_data[, 6] <- as.numeric(Concatenated_data[, 6])
Concatenated_data[, 6] <- factor(Concatenated_data[, 6])

PCAdata <- Concatenated_data[, -(1:35)]
PCAdata <- PCAdata[, -12]
PCAdata <- apply(PCAdata, 2, as.numeric)
remove <- apply(is.na(PCAdata), 1, any)
PCAdata <- PCAdata[!remove, ]


data.analysis.comp2 <- data.frame("Model" = as.factor(Concatenated_data[!remove, 6]), PCAdata)
data.analysis.comp2$sprate <- data.analysis.comp2$trait_1_speciation/data.analysis.comp2$trait_2_speciation
data.analysis.comp2$extrate <- data.analysis.comp2$trait_1_extinction/data.analysis.comp2$trait_2_extinction

a <- as.data.frame(resu.bantu$results_summary_of_single_value_outputs)
a$sprate <- a$trait_1_speciation / a$trait_2_speciation
a$extrate <- a$trait_1_extinction / a$trait_2_extinction

data.analysis.comp3 <- data.analysis.comp2[, -c(2, 13:14, 16:20, 27)]
# data.analysis.comp3 <- data.analysis.comp2[, c(1, 15, 23:26, 27:28)]
data.analysis.comp3 <- data.analysis.comp3[data.analysis.comp3$Model %in% c(1:4), ]
data.analysis.comp3$Model <- factor(data.analysis.comp3$Model)
sub.test <- unlist(lapply(as.list(c(1:4)), function(x, y) {
  sample(which(y$Model == x), min(table(data.analysis.comp3$Model)))},
  y = data.analysis.comp3))
data.analysis.comp3 <- data.analysis.comp3[sub.test, ]
(fit <- randomForest(Model ~ ., data=data.analysis.comp3, importance=TRUE, ntree=10000))
(predictions <- predict(fit, 
                        a,
                        type="prob"))
varImpPlot(fit)
