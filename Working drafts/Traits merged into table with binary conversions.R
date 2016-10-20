social.complexity <- read.csv("~/Box Sync/colliding ranges/Simulations_humans/Concatenated data tables/social complexity.csv")
head(social.complexity)
slavery <- read.csv("~/Box Sync/colliding ranges/Simulations_humans/Concatenated data tables/slavery.csv")
head(slavery)

Jurisdictional.heirarchy <- read.csv("~/Box Sync/colliding ranges/Simulations_humans/Concatenated data tables/Jurisdictional_heirarchy_beyond_local_community.csv")
head(Jurisdictional.heirarchy)

length(social.complexity[,3])
length(slavery[,3])
length(Jurisdictional.heirarchy[,3])
names(slavery)
merged <- merge(social.complexity, slavery, by = "Society.id")
merged_2 <- merge(Jurisdictional.heirarchy, merged, by = "Society.id")

dim(slavery)
dim(merged)
dim(merged_2)

names(merged_2)



social.complexity.types <- as.numeric(as.character(merged_2[,15]))
sedentary_homes <- social.complexity.types
sedentary_homes[which(sedentary_homes == 1 | sedentary_homes == 2 | sedentary_homes == 3 | sedentary_homes == 4 )] <- 0
sedentary_homes[which(sedentary_homes == 5 | sedentary_homes == 6 | sedentary_homes == 7 )] <- 1

slavery.types <- as.numeric(as.character(merged_2[,21]))
formalized_slavery <- slavery.types
formalized_slavery[which(formalized_slavery == 1 | formalized_slavery == 2 )] <- 0
formalized_slavery[which(formalized_slavery == 3 | formalized_slavery == 4 )] <- 1

Jurisdictional.heirarchy.types <- as.numeric(as.character(merged_2[,41]))
more_complex_than_chiefdoms <- Jurisdictional.heirarchy.types
more_complex_than_chiefdoms[which(more_complex_than_chiefdoms == 1 | more_complex_than_chiefdoms == 2 | more_complex_than_chiefdoms == 3 )] <- 0
more_complex_than_chiefdoms[which(more_complex_than_chiefdoms == 4 | more_complex_than_chiefdoms == 5 | more_complex_than_chiefdoms == 6 | more_complex_than_chiefdoms == 7 )] <- 1

#hist(as.numeric(as.character(na.omit(Jurisdictional.heirarchy.types))), breaks=5)



merged_3 <- cbind(merged_2[,1:25], merged_2[,40:45], social.complexity.types, sedentary_homes, slavery.types, formalized_slavery, Jurisdictional.heirarchy.types, more_complex_than_chiefdoms)
head(merged_3)
Society_data_with_binary_conversions <- merged_3
dim(Society_data_with_binary_conversions)
save(Society_data_with_binary_conversions, file="~/Box Sync/colliding ranges/Simulations_humans/FULL_TREE_Society_data_with_binary_conversions.Rdata")

load('~/Box Sync/colliding ranges/Simulations_humans/Available trees/Tree_Bantu.RData')
bantu_tree$tip.label
BANTU_Society_data_with_binary_conversions <- merged_2[which(bantu_tree$tip.label%in% merged_2[,1] ), ]
save(BANTU_Society_data_with_binary_conversions, file="~/Box Sync/colliding ranges/Simulations_humans/Concatenated data tables/BANTU_Society_data_with_binary_conversions.Rdata")
dim(BANTU_Society_data_with_binary_conversions)


load('~/Box Sync/colliding ranges/Simulations_humans/Available trees/Tree_austronisian.RData')
aus_tree$tip.label
AUSTRONISIAN_Society_data_with_binary_conversions <- merged_2[which(aus_tree$tip.label%in% merged_2[,1] ), ]
save(AUSTRONISIAN_Society_data_with_binary_conversions, file="~/Box Sync/colliding ranges/Simulations_humans/Concatenated data tables/AUSTRONISIAN_Society_data_with_binary_conversions.Rdata")
dim(AUSTRONISIAN_Society_data_with_binary_conversions)

load('~/Box Sync/colliding ranges/Simulations_humans/Available trees/Tree_Uto.RData')
uto_tree $tip.label
UTO_Society_data_with_binary_conversions <- merged_2[which(uto_tree$tip.label%in% merged_2[,1] ), ]
save(UTO_Society_data_with_binary_conversions, file="~/Box Sync/colliding ranges/Simulations_humans/Concatenated data tables/UTO_Society_data_with_binary_conversions.Rdata")
dim(UTO_Society_data_with_binary_conversions)


