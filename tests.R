# Test new functions versus old functions
source("SimulationFunctions.R")
source("original_functions.R")

# New buildworld function
system.time(test1 <- BuildWorld(10, 0.5))
system.time(test2 <- BuildWorld2(10, 0.5))
all(test1[, 1:3] == test2[, 1:3])


