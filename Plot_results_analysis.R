# Plot Results analysis
load("25_50_analysis.R")
a <- data.result
load("28_50_analysis.R")
b <- data.result
load("29_50_analysis.R")
c <- data.result
load("31_50_analysis.R")
d <- data.result

c <- rbind(a, b, c, d)

combinations <- c$combo
combinations[combinations == 25] <- "S+E+A"
combinations[combinations == 28] <- "S+E+A+D"
combinations[combinations == 29] <- "S+E+A+T"
combinations[combinations == 31] <- "S+E+A+D+T"
combinations <- factor(combinations, labels = c("S+E+A", "S+E+A+D", "S+E+A+T", "S+E+A+D+T"))
# Plot
col <- c("cornflowerblue", "firebrick", "limegreen", "yellow2")

quartz(width = 11, height = 4)
par(mar = c(4, 4, 1, 1),
    mfrow = c(2, 8), 
    cex.axis = .7)

plot(c$Difference ~ combinations, 
     ylab = "Predominance (F - D)",
     xlab = "", col = col)
lim <- c(min(c[, c("DF", "DD", "FF")], na.rm = TRUE), max(c[, c("DF", "DD", "FF")], na.rm = TRUE))
plot(c$DF ~ combinations, 
     ylab = "Spatial signal in DF",
     xlab = "", ylim = lim, col = col)
plot(c$DD ~ combinations, 
     ylab = "Spatial signal in DD",
     xlab = "", ylim = lim, col = col)
plot(c$FF ~ combinations, 
     ylab = "Spatial signal in FF",
     xlab = "", ylim = lim, col = col)
plot(c$N.tips ~ combinations, 
     ylab = "Number of tips",
     xlab = "", col = col)
plot(c$N.nodes ~ combinations, 
     ylab = "Number of nodes",
     xlab = "", col = col)
plot(c$shape ~ combinations, 
     ylab = "BL distribuiton shape",
     xlab = "", log = "y", col = col)
plot(c$scale ~ combinations, 
     ylab = "BL distribuiton scale",
     xlab = "", log = "y", col = col)
plot(c$Colless ~ combinations, 
     ylab = "Tree balance (Colless)",
     xlab = "", col = col)
plot(c$TCI ~ combinations, 
     ylab = "Tree balance (TCI)",
     xlab = "", col = col)
plot(c$gamma ~ combinations, 
     ylab = "Tree gamma parameter",
     xlab = "", col = col)
plot(c$Phy_Signal ~ combinations, 
     ylab = "Phylogenetic signal (D)",
     xlab = "", col = col)
plot(c$MS ~ combinations, 
     ylab = "Net Diversification rates (MS)",
     xlab = "", col = col)
plot(c$KM ~ combinations, 
     ylab = "Net Diversification rates (KM)",
     xlab = "", col = col)
plot(c$Medusa.BP ~ combinations, 
     ylab = "Diversification shifts",
     xlab = "", col = col)
plot(c$Trasition.rates ~ combinations, 
     ylab = "Trait evolution transtion rates",
     xlab = "", log = "y", col = col)


