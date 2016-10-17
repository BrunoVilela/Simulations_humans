library(ape)
# Match codes
tree <- read.nexus("Real_phy/x.tree")
iso <- read.csv("Real_phy/Isomatch.csv")
match.iso <- match(tree$tip.label, iso[, 3])

newlab <- iso[match.iso, 2]
newtree <- drop.tip(tree, which(is.na(newlab)))
newtree$tip.label <- as.character(newlab[!is.na(newlab)])
plot(newtree)

# Get farming values
mydata <- read.csv("Real_phy/Domestication_var.csv")
sub <- mydata[, 1] %in% newtree$tip.label
mydata <- mydata[sub, ]
match.farm <- match(newtree$tip.label, mydata[, 1])
mydata2 <- mydata[match.farm, ]
mydata2 <- na.omit(mydata2)
newtree2 <- drop.tip(newtree, which(is.na(match.farm)))
head(newtree2$tip.label)
farming_binary <- ifelse(mydata2$farming >= 0, 1, 0)
names(farming_binary) <- mydata2$soc_id

# Plot
library(phytools)
library(diversitree)
trait.plot(newtree2, as.data.frame(farming_binary), 
           list(cols = c("blue", "red")), lab = rep("", Ntip(newtree2)))

# Match space
myWorld2 <- as.data.frame(BuildWorld(mydata2[, c(2, 3)], conditions = 1))
myWorld2[, 6] <- mydata2[, 12]
myWorld2[, 8] <- mydata2[, 1]

# Analysis
library(FARM)
MyOut2 <- list("myWorld" = myWorld2, "mytree" = newtree2)
real.analysis <- Module_2(MyOut2)
