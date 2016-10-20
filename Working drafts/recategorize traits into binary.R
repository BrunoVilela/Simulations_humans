social.complexity <- read.csv("~/Desktop/social complexity.csv")
head(social.complexity)


load('~/Desktop/Tree_Bantu.RData')

objects()

str(bantu_tree)

bantu_tree$tip.label


names(social.complexity)


social.complexity.bantu <- social.complexity[which(social.complexity[,3] %in% bantu_tree$tip.label),]

head(social.complexity.bantu)
dim(social.complexity.bantu)
hist(as.numeric(as.character(na.omit(social.complexity.bantu[,15]))), breaks=8)

types <- as.numeric(as.character(social.complexity.bantu[,15]))
types[which(types == 1 | types == 2 | types == 3 | types == 4 )] <- 0
types[which(types == 5 | types == 6 | types == 7 )] <- 1
sedentary_homes <- types

social.complexity.bantu_binary <- cbind(social.complexity.bantu, sedentary_homes)
head(social.complexity.bantu_binary)

###########################################

slavery <- read.csv("~/Desktop/slavery.csv")
head(slavery)


load('~/Desktop/Tree_Bantu.RData')

objects()

str(bantu_tree)

bantu_tree$tip.label


names(slavery)


slavery.bantu <- slavery[which(slavery[,3] %in% bantu_tree$tip.label),]

head(slavery.bantu)
dim(slavery.bantu)
hist(as.numeric(as.character(na.omit(slavery.bantu[,15]))), breaks=5)

types <- as.numeric(as.character(slavery.bantu[,15]))
types[which(types == 1 | types == 2 )] <- 0
types[which(types == 3 | types == 4 )] <- 1
formalized_slavery <- types

Bantu.data.table <- cbind(social.complexity.bantu_binary, formalized_slavery)
head(Bantu.data.table)


###########################################

Jurisdictional.heirarchy <- read.csv("~/Desktop/Jurisdictional_heirarchy_beyond_local_community.csv")
head(Jurisdictional.heirarchy)


load('~/Desktop/Tree_Bantu.RData')

objects()

str(bantu_tree)

bantu_tree$tip.label


names(Jurisdictional.heirarchy)


Jurisdictional.heirarchy.bantu <- slavery[which(Jurisdictional.heirarchy[,3] %in% bantu_tree$tip.label),]

head(Jurisdictional.heirarchy.bantu)
dim(Jurisdictional.heirarchy.bantu)
hist(as.numeric(as.character(na.omit(Jurisdictional.heirarchy.bantu[,15]))), breaks=5)

types <- as.numeric(as.character(Jurisdictional.heirarchy.bantu[,15]))
types[which(types == 1 | types == 2 )] <- 0
types[which(types == 3 | types == 4 )] <- 1
complexity <- types

Bantu.data.table <- cbind(Jurisdictional.heirarchy.bantu, complexity)
head(Bantu.data.table)



