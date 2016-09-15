
# Define the parameters min mean ma


spec <- c(0.8278971, 0.5752526, 1.0000000)
ext <- c(0.8470351, 1.0000000, 0.7572174)
prob_choose_all <- list(c(0.05, 0.3, 0.6), c(0.05, 0.3, 0.6), c(0.05, 0.3, 0.6), #speciation F, Non-D, D
                        c(0.05, 0.2, 0.4), c(0.05, 0.2, 0.4), c(0.05, 0.2, 0.4), #extinction F, Non-D, D
                        c(0, 0.2, 0.4), c(0, 0.2, 0.4), #diffusion F-D, D-F
                        c(0, 0.2, 0.4), c(0, 0.2, 0.4), c(0, 0.2, 0.4),
                        c(0, 0.2, 0.4)) #takeover F-F, F-D, D-F, D-D
prob_choose_all[1:6] <- mapply(function(x, y){x * y}, x = prob_choose_all[1:6],
                               y = c(spec, ext), SIMPLIFY = FALSE)
names(prob_choose_all) <- c("speciationF", "speciationNonD", "speciationD",
                            "extinctionF", "extinctionNonD", "exintctionD",
                            "diffusionFD", "diffusionDF", "takeoverFF",
                            "takeoverFD", "takeoverDF", "takeoverDD")
parameters.table <- expand.grid(prob_choose_all[1:length(prob_choose_all)])
parameters.table <- parameters.table[sample(1:nrow(parameters.table)), ]
library(devtools)
devtools::use_data(parameters.table, overwrite = TRUE)
