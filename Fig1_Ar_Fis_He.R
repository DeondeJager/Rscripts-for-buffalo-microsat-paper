##########################################################################################################################
#### Fig 1 ####
# Plot Ar, Fis and He together

# Load package to plot with confidence intervals:
#install.packages("plotrix")
library(plotrix)

# Run the code below (up to and including dev.off() all at once by highlighting all and pressing ctrl + enter):

## Save as PNG:
png("Ar_Fis_He_plots_combined_labelled.png", 
    units = "in", 
    width = 7.0, 
    height = 5.85, 
    pointsize = 10,
    res = 600) #Call the PNG driver to create PNG file to which the plot will be written.
par(mar = c(5, 4, 2, 2) + 0.1)
# Create matrix for layout
m <- rbind(c(1,2), c(3,3)) # Two rows; first row plots fig 1 and 2, third row plots fig 3.
#create layout; #Use layout.show(3) to see layout of plots before plotting
layout(m)

# Ar plot
Ar <- read.csv("Ar.csv", header = T)
plotCI(Ar$Ar, ui = Ar$upperCI_95., li = Ar$lowerCI_95., xaxt = "n", ylim = c(0,8), lab = c(16,5,7), ylab = "", xlab = "", pch = 19, sfrac = 0.005)
axis(side = 1, at = 1:16, las = 2, labels = c("AENP", "GNP", "MNP", "WPP", "P001", "P002", "P003", "P004", "P005", "P006", "P007", "P008", "P009", "P010", "P011", "P012"))
title(xlab = "Locality", line = 3.5)
title(ylab = expression("Allelic richness" ~ (italic("A")[R])), line = 2.5) #The tilde (~) is handled as a space in a true regular expression in R.
abline(h = c(5.5, 5.67), lty = c(5, 3))
mtext(text = expression(bold("A")), side = 2, line = 2.5, at = 9, las = 2)

# Fis plot
Fis <- read.csv("Fis.csv", header = T)
plotCI(Fis$Fis, ui = Fis$X95.CI_High, li = Fis$X95.CI_Low, xaxt = "n", ylim = c(-0.25,0.25), lab = c(16,5,7), ylab = "", xlab = "", pch = 19, sfrac = 0.005) #lab: number of ticks on x, y axes (the third number is unimplemented in R).
axis(side = 1, at = 1:16, las = 2, labels = c("AENP", "GNP", "MNP", "WPP", "P001", "P002", "P003", "P004", "P005", "P006", "P007", "P008", "P009", "P010", "P011", "P012"))
title(xlab = "Locality", line = 3.5)
title(ylab = expression("Inbreeding coefficient" ~ (italic("F")[IS])), line = 2.5)
abline(h = 0, lty = 3)
mtext(text = expression(bold("B")), side = 2, line = 2.5, at = 0.3, las = 2)
if(Fis$X95.CI_High[1]<0) {text(x=1, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[1]>0) {text(x=1, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[2]<0) {text(x=2, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[2]>0) {text(x=2, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[3]<0) {text(x=3, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[3]>0) {text(x=3, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[4]<0) {text(x=4, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[4]>0) {text(x=4, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[5]<0) {text(x=5, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[5]>0) {text(x=5, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[6]<0) {text(x=6, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[6]>0) {text(x=6, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[7]<0) {text(x=7, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[7]>0) {text(x=7, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[8]<0) {text(x=8, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[8]>0) {text(x=8, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[9]<0) {text(x=9, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[9]>0) {text(x=9, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[10]<0) {text(x=10, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[10]>0) {text(x=10, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[11]<0) {text(x=11, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[11]>0) {text(x=11, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[12]<0) {text(x=12, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[12]>0) {text(x=12, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[13]<0) {text(x=13, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[13]>0) {text(x=13, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[14]<0) {text(x=14, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[14]>0) {text(x=14, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[15]<0) {text(x=15, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[15]>0) {text(x=15, y=0.25, "*", cex=1.5)}
if(Fis$X95.CI_High[16]<0) {text(x=16, y=-0.25, "*", cex=1.5)}
if(Fis$X95.CI_Low[16]>0) {text(x=16, y=0.25, "*", cex=1.5)}

# He plot
Heterozygosity <- read.csv("Heterozygosity_spaces.csv", header = T)
par(xpd = T, mar = c(5, 4, 2, 5) + 0.1)  #ps: pointsize (font size) in plot.
plotCI(Heterozygosity_spaces$Heterozygosity, uiw = Heterozygosity$SD, xaxt = "n", ylim = c(0,1), lab = c(32,5,7), ylab = "", xlab = "", pch = c(19,15,20), sfrac = 0.005) #lab: number of ticks on x, y axes (the third number is unimplemented in R). The pch = 20 was put in to account for the "empty datapoint", which was putting the 19,15 symbols out of order.
axis(side = 1, at = c(1.5, 4.5, 7.5, 10.5, 13.5, 16.5, 19.5, 22.5, 25.5, 28.5, 31.5, 34.5, 37.5, 40.5, 43.5, 46.5), las = 2, labels = c("AENP", "GNP", "MNP", "WPP", "P001", "P002", "P003", "P004", "P005", "P006", "P007", "P008", "P009", "P010", "P011", "P012"))
title(xlab = "Locality", line = 3.5)
title(ylab = "Heterozygosity", line = 2.5)
legend(49.5,1, c(expression(italic("H")[O]), expression(italic("H")[E])), pch = c(19,15))
mtext(text = expression(bold("C")), side = 2, line = 2.5, at = 1.15, las = 2)

dev.off()
