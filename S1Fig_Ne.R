##########################################################################################################################
#### S1 Fig ####

# Load package to plot with confidence intervals:
#install.packages("plotrix")
library(plotrix)

# Read CSV file into R:
Ne <- read.csv(file = "Ne.csv", header = T)

# Plot and write to PDF:
pdf("Ne_extended.pdf", 
    width = 6.5, 
    height = 5) #Call the pdf driver to create PDF file to which the plot will be written.
par(xpd = F,
    mar = c(5, 4, 2, 5) + 0.1,
    ps = 10)
plotCI(Ne$Ne, 
       ui = Ne$Upper, 
       li = Ne$Lower, 
       xaxt = "n", 
       ylim = c(0,150), 
       lab = c(32,10,7), #lab: number of ticks on x, y axes (the third number is unimplemented in R).
       ylab = "", xlab = "", 
       pch = 19,
       cex = 0.75,
       sfrac = 0.005)  
axis(side = 1, 
     at = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19), 
     las = 2, 
     labels = c("AENP", "GNP", "MNP", "WPP", "P001", "P002", "P003", "P004", "P005", "P006", "P007", "P008", "P009", "P010", "P011", "P012", "GNP-MNP", "PVT"))
axis(side = 4)
mtext(c(expression("Effective Population Size\ "(italic("N")[e]))), side = 4, line = 2.3)
title(xlab = "Locality", line = 3)
title(ylab = c(expression("Effective Population Size\ "(italic("N")[e]))), line = 2.25)
textbox(c(1.55, 2.45), 145, 
        c("518"), 
        justify = "c", 
        adj = 0.05,
        cex = 0.66,
        box = T, 
        fill = "white", 
        border = "white")
textbox(c(17.55, 18.45), 145,
        c("291"),
        justify = "c",
        adj = 0.05,
        cex = 0.66,
        box = T,
        fill = "white",
        border = "white")
segments(x0 = 0, y0 = Ne[2,3], x1 = 17, lty = 2)
abline(v = 17, lty = 3)
# points(Ne$Sample.Size,
#        pch = 4)
dev.off()

#Plot and write to PNG:
png("Ne_extended.png", 
    units = "in", 
    width = 6.5, 
    height = 5, 
    res = 600)
par(xpd = F,
    mar = c(5, 4, 2, 5) + 0.1,
    ps = 10)
plotCI(Ne$Ne, 
       ui = Ne$Upper, 
       li = Ne$Lower, 
       xaxt = "n", 
       ylim = c(0,150), 
       lab = c(32,10,7), #lab: number of ticks on x, y axes (the third number is unimplemented in R).
       ylab = "", xlab = "", 
       pch = 19,
       cex = 0.75,
       sfrac = 0.005)  
axis(side = 1, 
     at = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19), 
     las = 2, 
     labels = c("AENP", "GNP", "MNP", "WPP", "P001", "P002", "P003", "P004", "P005", "P006", "P007", "P008", "P009", "P010", "P011", "P012", "GNP-MNP", "PVT"))
axis(side = 4)
mtext(c(expression("Effective Population Size\ "(italic("N")[e]))), side = 4, line = 2.3)
title(xlab = "Locality", line = 3)
title(ylab = c(expression("Effective Population Size\ "(italic("N")[e]))), line = 2.25)
textbox(c(1.55, 2.45), 145, 
        c("518"), 
        justify = "c", 
        adj = 0.05,
        cex = 0.66,
        box = T, 
        fill = "white", 
        border = "white")
textbox(c(17.55, 18.45), 145,
        c("291"),
        justify = "c",
        adj = 0.05,
        cex = 0.66,
        box = T,
        fill = "white",
        border = "white")
segments(x0 = 0, y0 = Ne[2,3], x1 = 17, lty = 2)
abline(v = 17, lty = 3)
# points(Ne$Sample.Size,
#        pch = 4)
dev.off()