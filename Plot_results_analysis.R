
setwd("~/Box Sync/colliding ranges/Simulations_humans/results cluster output")

# Plot Results analysis
load("Results_for_25_simulated_for_ 100_time_steps_analysis.R")
a <- data.result
load("Results_for_28_simulated_for_ 100_time_steps_analysis.R")
b <- data.result
load("Results_for_29_simulated_for_ 100_time_steps_analysis.R")
c <- data.result
load("Results_for_31_simulated_for_ 100_time_steps_analysis.R")
d <- data.result
=======

#==================================================================
# Function to check if a cell is inside the world
plot.output <- function(c, col = c("cornflowerblue", "firebrick", "limegreen", "yellow2"),
                        ...) {
  combinations <- c$combo
  combinations[combinations == 25] <- "S+E+A"
  combinations[combinations == 28] <- "S+E+A+D"
  combinations[combinations == 29] <- "S+E+A+T"
  combinations[combinations == 31] <- "S+E+A+D+T"
  combinations <- factor(combinations, labels = c("S+E+A", "S+E+A+D", "S+E+A+T", "S+E+A+D+T"))
  # Plot
  plot(c$Difference ~ combinations, 
       ylab = "Predominance (F - D)",
       xlab = "", col = col, ...)
  lim <- c(min(c[, c("DF", "DD", "FF")], na.rm = TRUE),
           max(c[, c("DF", "DD", "FF")], na.rm = TRUE))
  plot(c$DF ~ combinations, 
       ylab = "Spatial signal in DF",
       xlab = "", ylim = lim, col = col, ...)
  plot(c$DD ~ combinations, 
       ylab = "Spatial signal in DD",
       xlab = "", ylim = lim, col = col, ...)
  plot(c$FF ~ combinations, 
       ylab = "Spatial signal in FF",
       xlab = "", ylim = lim, col = col, ...)
  # plot(c$N.tips ~ combinations, 
  #      ylab = "Number of tips",
  #      xlab = "", col = col, ...)
  plot(c$N.nodes ~ combinations, 
       ylab = "Number of nodes",
       xlab = "", col = col, ...)
  plot(c$shape ~ combinations, 
       ylab = "BL distribuiton shape",
       xlab = "", log = "y", col = col, ...)
  plot(c$scale ~ combinations, 
       ylab = "BL distribuiton scale",
       xlab = "", log = "y", col = col, ...)
  plot(c$gamma ~ combinations, 
       ylab = "BL distribuiton Gamma",
       xlab = "", col = col, ...)
  plot(c$Colless ~ combinations, 
       ylab = "Tree balance (Colless)",
       xlab = "", col = col, ...)
  plot(c$TCI ~ combinations, 
       ylab = "Tree balance (TCI)",
       xlab = "", col = col, ...)
  plot(c$Phy_Signal ~ combinations, 
       ylab = "Phylogenetic signal (D)",
       xlab = "", col = col, ...)
  # plot(c$MS ~ combinations, 
  #      ylab = "Net Diversification rates (MS)",
  #      xlab = "", col = col, ...)
  plot(c$KM ~ combinations, 
       ylab = "Net Diversification rates",
       xlab = "", col = col, ...)
  # plot(c$Medusa.BP ~ combinations, 
  #      ylab = "Diversification shifts",
  #      xlab = "", col = col, ...)
  plot(c$Trasition.rates ~ combinations, 
       ylab = "Transtion rates",
       xlab = "", log = "y", col = col, ...)
}

# Plot Results analysis
plot.data.25 <- NULL
plot.data.50 <- NULL
plot.data.75 <- NULL
plot.data.100 <- NULL
combo.type <- c(25, 28, 29, 31)

for (i in combo.type) {
  load(paste0("results cluster output/Results_for_", i,"_simulated_for_ ", 
              25, "_time_steps_analysis.R"))
  plot.data.25 <- rbind(plot.data.25, data.result)
  load(paste0("results cluster output/Results_for_", i,"_simulated_for_ ", 
              50, "_time_steps_analysis.R"))
  plot.data.50 <- rbind(plot.data.50, data.result)
  load(paste0("results cluster output/Results_for_", i,"_simulated_for_ ", 
              75, "_time_steps_analysis.R"))
  plot.data.75 <- rbind(plot.data.75, data.result)
  load(paste0("results cluster output/Results_for_", i,"_simulated_for_ ", 
              100, "_time_steps_analysis.R"))
  plot.data.100 <- rbind(plot.data.100, data.result)
}

titles <- c("Predominance (F - D)", "Spatial signal in DF",
            "Spatial signal in DD", "Spatial signal in FF",
            "Number of nodes", "BL distribuiton shape",
            "BL distribuiton scale", "BL distribuiton Gamma",
            "Tree balance (Colless)", "Tree balance (TCI)",
            "Phylogenetic signal (D)", "Net Diversification rates",
            "Transtion rates")

pdf(file="tree analysis figure.pdf", width = 50, height = 15)
par(mar = c(1, 1, 1, 1))
times <- c("", 25, 50, 75, 100)
nvars <- 13
ntimes <- length(times) - 1
n.plots <- (nvars * ntimes)
mat <- matrix(c(1:n.plots), ncol = nvars, nrow = ntimes, byrow = TRUE)
mat <- rbind((n.plots + 1):((n.plots) + nvars), mat)
mat <- cbind(((n.plots + 1) + nvars):((((n.plots + 1) + nvars)) + ntimes), mat)
cols.size <- rep(1, (nvars + 1))
cols.size[1] <- 0.4
rows.size <- rep(1, (ntimes + 1))
rows.size[1] <- 0.4
nf <- layout(mat, cols.size, rows.size, TRUE)
#layout.show(nf)
plot.output(plot.data.25, border = c("darkblue", "darkred", "darkgreen", "yellow3"),
            xaxt = "n", cex.lab = 2, bg = "white")
plot.output(plot.data.50, border = c("darkblue", "darkred", "darkgreen", "yellow3"),
            xaxt = "n", cex.lab = 2, bg = "gray90")
plot.output(plot.data.75, border = c("darkblue", "darkred", "darkgreen", "yellow3"),
            xaxt = "n", cex.lab = 2, bg = "gray90")
plot.output(plot.data.100, border = c("darkblue", "darkred", "darkgreen", "yellow3"),
            xaxt = "n", cex.lab = 2, bg = "gray90")

for (i in titles) {
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n',
       type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, i, cex = 2.8)
}
for (i in times) {
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n',
       type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, paste(i), cex = 4)
}
dev.off()




#==================================================================
data.analysis <- rbind(plot.data.25, plot.data.50, plot.data.75, plot.data.100)

library(nnet)
combinations <- data.analysis$combo
combinations[combinations == 25] <- "S+E+A"
combinations[combinations == 28] <- "S+E+A+D"
combinations[combinations == 29] <- "S+E+A+T"
combinations[combinations == 31] <- "S+E+A+D+T"
combinations <- factor(combinations, labels = c("S+E+A", "S+E+A+D", "S+E+A+T", "S+E+A+D+T"))
data.analysis$combinations <- combinations

test <- multinom(combinations ~ (gamma + Colless + Phy_Signal + shape +
                                   Trasition.rates + KM),
                 data = data.analysis)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p <- p.adjust(p, method = "holm")
p <- matrix(p, nrow = 3)
colnames(p) <- colnames(z)
rownames(p) <- rownames(z)

summary(test)


