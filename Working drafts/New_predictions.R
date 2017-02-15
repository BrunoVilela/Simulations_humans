load("~/Box Sync/Bruno/WUSTL projects/Simulations_humans/Concatenated data tables/Four_model_compare_FULL.Rdata")
Concatenated_data <- Concatenated_data[Concatenated_data[, 2] == "stats.no.bTO", ]
Concatenated_data <- Concatenated_data[Concatenated_data[, 6] != "05", ]
# Concatenated_data[, 6] <- as.numeric(Concatenated_data[, 6])
# # Concatenated_data[original[, 2] == "background_takeover", 6] <-  Concatenated_data[original[, 2] == "background_takeover", 6] + 4
Concatenated_data[, 6] <- factor(Concatenated_data[, 6])


PCAdata <- Concatenated_data[, -(1:35)]
PCAdata <- PCAdata[, -12]
PCAdata <- apply(PCAdata, 2, as.numeric)
remove <- apply(is.na(PCAdata), 1, any)
PCAdata <- PCAdata[!remove, ]

# Predictions
library(randomForest)

data.analysis.comp2 <- data.frame("Model" = as.factor(Concatenated_data[!remove, 6]),
                                  PCAdata)
data.analysis.comp2$sprate <- data.analysis.comp2$trait_1_speciation/data.analysis.comp2$trait_2_speciation
data.analysis.comp2$extrate <- data.analysis.comp2$trait_1_extinction/data.analysis.comp2$trait_2_extinction


load("Real_phy/real.analysis.RData")
a <- as.data.frame(real.analysis$results_summary_of_single_value_outputs)
a$sprate <- a$trait_1_speciation / a$trait_2_speciation
a$extrate <- a$trait_1_extinction / a$trait_2_extinction

data.analysis.comp3 <- data.analysis.comp2[, -c(2, 13:14, 16:20, 27)]
data.analysis.comp3 <- data.analysis.comp3[data.analysis.comp3$Model %in% 1:4, ]
data.analysis.comp3$Model <- factor(data.analysis.comp3$Model)
sub <- unlist(lapply(as.list(c(1:4)), function(x, y) {
  sample(which(y$Model == x), min(table(data.analysis.comp3$Model)))},
  y = data.analysis.comp3))
data.analysis.comp3 <- data.analysis.comp3[sub, ]
fun <- function(x, y) {sample(which(y$Model == x), 50)}

sub.test <- unlist(lapply(as.list(c(1:4)), fun,
                          y = data.analysis.comp3))
test2 <- data.analysis.comp3[sub.test, 2:ncol(data.analysis.comp3)]
test1 <- data.analysis.comp3[sub.test, 1]
train <- data.analysis.comp3[-sub.test, ]
(fit <- randomForest(Model ~ ., data=train, xtest = test2, ytest = test1, 
                    importance=TRUE, ntree=2000, keep.forest = TRUE))
plot(fit)
predictions <- predict(fit, 
                       a,
                       type="prob")
