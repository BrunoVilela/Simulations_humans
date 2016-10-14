one <- subset(results_table, Model_type=="01")
two <- subset(results_table, Model_type=="02")
three <- subset(results_table, Model_type=="03")
four <- subset(results_table, Model_type=="04")
five <- subset(results_table, Model_type=="05")

crop <- min(length(one[,1]),
length(two[,1]),
length(three[,1]),
length(four[,1]),
length(five[,1]))

one <- one[1:crop,]
two <- two[1:crop,]
three <- three[1:crop,]
four <- four[1:crop,]
five <- five[1:crop,]

Concatenated_data <- rbind(one, two, three, four, five)

save(Concatenated_data, file="~/Box Sync/colliding ranges/Simulations_humans/Four_model_compare_402_trees_of_each.Rdata")


Concatenated_data$gamma