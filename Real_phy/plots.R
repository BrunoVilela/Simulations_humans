labs <- c("Basic", "+Diffusion", "+Takeover", "+Diffusion +Takeover")

# bar plot
tiff("Prob_aus.tif", width = 25, height = 25, res = 90, units = "cm",
     compression = "lzw")
par(mar = c(8, 8, 1, 1))
pred <- setNames(as.numeric(predictions), labs)
cols <- rev(c("darkgreen", "red", "blue", "darkorange1"))
barplot(pred, col = cols, ylab = "Proability", cex.lab = 3, cex.names = 2)
dev.off()

# Plot confusion matrix
pdf("Conffusion_matrix_all.pdf", width = 25, height = 25)
par(mar = c(10, 11, 1, 1))
colors1 <- colorRampPalette(colors = c("#f0f0f0", "#bdbdbd","#636363"))
prop <- apply(fit$confusion[, -5], 2, function(x){x / sum(x)}) * 100

image(prop, col = colors1(20), axes=FALSE)
axis(1, at=c(0, .33, .66, 1), labels=labs, tick = FALSE, line = FALSE, cex.axis = 3.5, pos = -.19)
axis(2, at=c(0, .33, .66, 1), labels=labs, tick = FALSE, line = FALSE, cex.axis = 3.5)
mtext("ACTUAL", side = 1, padj = 3, cex = 4)
mtext("PREDICTED", side = 2, padj = -3, cex = 4)

for(i in 1:4) {
  for(j in 1:4) {
    text(x = c(0, .33, .66, 1)[i], y = c(0, .33, .66, 1)[j], paste0(round(prop[i, j], 2), "%"),
         cex = 5)
  }
}
dev.off()
# Variables importance

imp <- importance(fit)
imp <- apply(imp, 2, function(x) (x - min(x))/(max(x) - min(x)))
imp <- imp[sort(imp[, 5], index.return = TRUE, decreasing = TRUE)$ix, ]


names <- rownames(imp)
names[names == "spatial.tests.fora"] <- "Space F"
names[names == "spatial.tests.dom"] <- "Space D"
names[names == "sprate"] <- "Sp(ratio)"
names[names == "transition_from_trait_1_to_2"] <- "TR(1-2)"
names[names == "transition_from_trait_2_to_1"] <- "TR(2-1)"
names[names == "Phylogenetic_signal"] <- "PhySig(D)"
names[names == "Evolutionary_distinctiveness_sum"] <- "EDsum"
names[names == "Pylo_diversity_is_sum_of_BL"] <- "PDsum"
names[names == "transition_rate_ratio_1to2_over_2to1"] <- "TR(ratio)"
names[names == "gamma"] <- "Gamma"
names[names == "mean_Phylogenetic_isolation"] <- "MPI"
names[names == "extrate"] <- "Ext(ratio)"
names[names == "average_phylogenetic_diversity_is_mean_of_BL"] <- "PDmean"
names[names == "extinction_per_speciation"] <- "DR"
names[names == "variance_Phylogenetic_isolation"] <- "VPI"
names[names == "F_quadratic_entropy_is_sum_of_PD"] <- "F"
names[names == "Mean_pairwise_distance"] <- "MPD"
names[names == "variance_Pylo_diversity_is_variance_of_BL"] <- "PDvar"
names[names == "variance_pairwise_distance"] <- "VPD"


pdf("var_import_all.pdf", width = 25, height = 25)
par(mar = c(10, 18, 1, 1))
plot(x = rev(imp[, 5]), y = 1:nrow(imp), type = "l", yaxt = "n", 
     ylab = "", xlab = "Variable Importance",
     xlim = c(0, 1), lwd = 2, cex.lab = 4)
for (i in 1:nrow(imp)) {
  abline(h = i, lty = 3, col = "gray80")
}
abline(v = seq(0, 1, 1/19), lty = 3, col = "gray80")

lines(x = rev(imp[, 4]), y = 1:nrow(imp), col = "darkgreen", lwd = 2)
lines(x = rev(imp[, 3]), y = 1:nrow(imp), col = "red", lwd = 2)
lines(x = rev(imp[, 2]), y = 1:nrow(imp), col = "blue", lwd = 2)
lines(x = rev(imp[, 1]), y = 1:nrow(imp), col = "darkorange1", lwd = 2)
lines(x = rev(imp[, 5]), y = 1:nrow(imp), lwd = 3)

points(x = rev(imp[, 4]), y = 1:nrow(imp), col = "darkgreen", cex = 2)
points(x = rev(imp[, 3]), y = 1:nrow(imp), col = "red", cex = 2)
points(x = rev(imp[, 2]), y = 1:nrow(imp), col = "blue", cex = 2)
points(x = rev(imp[, 1]), y = 1:nrow(imp), col = "darkorange1", cex = 2)
points(x = rev(imp[, 5]), y = 1:nrow(imp), pch = 20, cex = 3)


text(y = 1:nrow(imp), x = par("usr")[1] - .17, labels = rev(names),
     srt = 0, pos = 4, xpd = T, cex = 4)
dev.off()

# Box plots
boxplot(spatial.tests.fora ~ Model, data = data.analysis.comp3)
abline(h = a$spatial.tests.fora, col = "red", lty = 2)

boxplot(spatial.tests.dom ~ Model, data = data.analysis.comp3)
abline(h = a$spatial.tests.fora, col = "red", lty = 2)

boxplot(log(sprate) ~ Model, data = data.analysis.comp3, ylim = c(-10, 10))
abline(h = log(a$sprate), col = "red", lty = 2)

boxplot(log(extrate) ~ Model, data = data.analysis.comp3, ylim = c(-10, 10))
abline(h = log(a$extrate), col = "red", lty = 2)

boxplot(log(transition_rate_ratio_1to2_over_2to1) ~ Model, data = data.analysis.comp3)
abline(h = log(a$sprate), col = "red", lty = 2)

boxplot(Phylogenetic_signal ~ Model, data = data.analysis.comp3, ylim = c(0, 1))
abline(h = a$Phylogenetic_signal, col = "red", lty = 2)


