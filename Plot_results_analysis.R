
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
  plot(c$Medusa.BP ~ combinations, 
       ylab = "Diversification shifts",
       xlab = "", col = col, ...)
  plot(c$Trasition.rates ~ combinations, 
       ylab = "Transtion rates",
       xlab = "", log = "y", col = col, ...)
}

titles < c("Predominance (F - D)", "Spatial signal in DF",
           "Spatial signal in DD", "Spatial signal in FF",
           "Number of nodes", "BL distribuiton shape",
           "BL distribuiton scale", "BL distribuiton Gamma",
           "Tree balance (Colless)", "Tree balance (TCI)",
           "Phylogenetic signal (D)", "Net Diversification rates",
           "Diversification shifts", "Transtion rates")


# Plot Results analysis
plot.data.25 <- NULL
plot.data.50 <- NULL
plot.data.75 <- NULL
plot.data.100 <- NULL
combo.type <- c(25, 28, 29, 31)

for (i in combo.type) {
  load(paste0("results cluster output/Results_for_", i,"_simulated_for_ ", 
              50, "_time_steps_analysis.R"))
  plot.data.25 <- rbind(plot.data.25, data.result)#CHANGE
  load(paste0("results cluster output/Results_for_", i,"_simulated_for_ ", 
              50, "_time_steps_analysis.R"))
  plot.data.50 <- rbind(plot.data.50, data.result)
  load(paste0("results cluster output/Results_for_", i,"_simulated_for_ ", 
              50, "_time_steps_analysis.R"))
  plot.data.75 <- rbind(plot.data.75, data.result)
  load(paste0("results cluster output/Results_for_", i,"_simulated_for_ ", 
              50, "_time_steps_analysis.R"))#CHANGE
  plot.data.100 <- rbind(plot.data.100, data.result)
}

pdf(file="tree analysis figure.pdf", width = 50, height = 15)
par(mar = c(1, 1, 1, 1))
mat <- matrix(c(1:56), ncol = 14, nrow = 4, byrow = TRUE)
mat <- rbind(57:70, mat)
mat <- cbind(c(71:75), mat)
cols.size <- rep(1, 15)
cols.size[1] <- 0.4
rows.size <- rep(1, 5)
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

for (i in 1:titles) {
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n',
       type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, titles[i], cex = 2)
}
times <- c(25, 50, 75, 100)
for (i in 1:4) {
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n',
       type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, paste(times[i]), cex = 2)
}

dev.off()
