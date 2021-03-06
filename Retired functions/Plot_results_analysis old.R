
setwd("~/Box Sync/colliding ranges/Simulations_humans/results cluster output")

# Plot Results analysis
load("Results_for_25_simulated_for_ 300_time_steps_analysis.R")
a <- data.result
load("Results_for_28_simulated_for_ 300_time_steps_analysis.R")
b <- data.result
load("Results_for_29_simulated_for_ 300_time_steps_analysis.R")
c <- data.result
load("Results_for_31_simulated_for_ 300_time_steps_analysis.R")
d <- data.result


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
times <- c(300)
ntimes <- length(times) - 1


for (i in 1:ntimes) {
  assign(paste0("plot.data.", times[j + 1]), NULL)
}

combo.type <- c(25, 28, 29, 31)
setwd("~/Box Sync/colliding ranges/Simulations_humans/results cluster output")
for (k in combo.type) {
  list.files()
    load(paste0("Results_for_", k,"_simulated_for_ ", 
                "300", "_time_steps_analysis.R"))
    assign(paste0("plot.data.", times[j + 1]), 
           rbind(get(paste0("plot.data.", times[j + 1])), data.result))
  
}

titles <- c("Predominance (F - D)", "Spatial signal in DF",
            "Spatial signal in DD", "Spatial signal in FF",
            "Number of nodes", "BL distribuiton shape",
            "BL distribuiton scale", "BL distribuiton Gamma",
            "Tree balance (Colless)", "Tree balance (TCI)",
            "Phylogenetic signal (D)", "Net Diversification rates",
            "Transtion rates")
pdf(file="tree analysis figure.pdf", width = 3.75 * ntimes, height = 15)
par(mar = c(1, 1, 1, 1))
nvars <- 13
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
for (i in 1:ntimes) {
  plot.output(get(paste0("plot.data.", times[i + 1])), border = c("darkblue", "darkred", "darkgreen", "yellow3"),
              xaxt = "n", cex.lab = 2, bg = "white")
}
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


