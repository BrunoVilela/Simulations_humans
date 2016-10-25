


one <- subset(results_table, Model_type=="01" )
two <- subset(results_table, Model_type=="02" )
three <- subset(results_table, Model_type=="03" )
four <- subset(results_table, Model_type=="04" )
crop <- min(length(one[,1]),
length(two[,1]),
length(three[,1]),
length(four[,1]))
one <- one[1:crop,]
two <- two[1:crop,]
three <- three[1:crop,]
four <- four[1:crop,]

Concatenated_data <- rbind(one, two, three, four)
dim(Concatenated_data)
save(Concatenated_data, file="~/Box Sync/colliding ranges/Simulations_humans/Concatenated data tables/Four_model_compare_Bantu.Rdata")
crop

#################################

levels(results_table[,2]) <- c("background_takeover", "no_background_takeover")
names(results_table)

one.bto <- subset(results_table, Model_type=="01" & background_takeover_type == "background_takeover")
two.bto <- subset(results_table, Model_type=="02" & background_takeover_type == "background_takeover")
three.bto <- subset(results_table, Model_type=="03" & background_takeover_type == "background_takeover")
four.bto <- subset(results_table, Model_type=="04" & background_takeover_type == "background_takeover")
one.nbto <- subset(results_table, Model_type=="01" & background_takeover_type == "no_background_takeover")
two.nbto <- subset(results_table, Model_type=="02" & background_takeover_type == "no_background_takeover")
three.nbto <- subset(results_table, Model_type=="03" & background_takeover_type == "no_background_takeover")
four.nbto <- subset(results_table, Model_type=="04" & background_takeover_type == "no_background_takeover")

crop.bto <- min(length(one.bto[,1]),
length(two.bto[,1]),
length(three.bto[,1]),
length(four.bto[,1]))

crop.nbto <- min(length(one.nbto[,1]),
length(two.nbto[,1]),
length(three.nbto[,1]),
length(four.nbto[,1]))

one.bto <- one.bto[1:crop.bto,]
two.bto <- two.bto[1:crop.bto,]
three.bto <- three.bto[1:crop.bto,]
four.bto <- four.bto[1:crop.bto,]

one.nbto <- one.nbto[1:crop.nbto,]
two.nbto <- two.nbto[1:crop.nbto,]
three.nbto <- three.nbto[1:crop.nbto,]
four.nbto <- four.nbto[1:crop.nbto,]


Concatenated_data <- rbind(one.bto, two.bto, three.bto, four.bto, one.nbto, two.nbto, three.nbto, four.nbto)

save(Concatenated_data, file="~/Box Sync/colliding ranges/Simulations_humans/Four_model_compare_background_TO_and_not.Rdata")


Concatenated_data$gamma
head(Concatenated_data)

