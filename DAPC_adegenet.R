############################################################################################################################
#### DAPC analysis - adegenet package ####
# Also see: https://github.com/thibautjombart/adegenet/wiki/Tutorials 


############################################################################################################################
#### Import data ####

# Load package
#install.packages("adegenet")
library(adegenet)

# Read Genepop data into R (saved as genind object used for DAPC)
fulldataset <- read.genepop("DAPC_Fulldataset_GenePop.gen", ncode = 3L)

# Set new population names to replace generic population names from the genepop file
x <- c("AENP", "GNP", "MNP", "WPP", "P001", "P002", "P003", "P004", "P005", "P006", "P007", "P008", "P009", "P010", "P011", "P012")
popNames(fulldataset) <- x
# Check that it worked:
popNames(fulldataset)


############################################################################################################################
#### Perform DAPC analysis for optimum K, as determined in the manuscript via Structure methods (K = 3) ####
# Find clusters
grp_k3 <- find.clusters(fulldataset, 
                                 max.n.clust = 60, 
                                 stat = "BIC", 
                                 n.pca = 60,
                                 n.clust = 3,
                                 choose.n.clust = TRUE,
                                 n.iter = 1e7,
                                 n.start = 20)
# Check order of clusters, to know the order of colours to use in the plot
# Note that the designation of clusters as "Cluster 1" or "Cluster 2" etc. is random and so you have to carefully check
# which cluster is first, second and third to make sure the correct colours are assigned to each cluster in the plot below.
table(pop(fulldataset), grp_k3$grp)

# Then perform dapc analysis:
dapc_k3_final <-dapc(fulldataset, 
                     grp_k3$grp,
                     n.pca = 60,
                     n.da = nPop(fulldataset) - 1)


############################################################################################################################
#### Plot the results ####

# Default plot
scatter(dapc_k3_final)

# Use ?scatter.dapc for info on how to change the look of the plot
# Use https://www.hexcolortool.com/ to view colours of hexadecimal codes.
        # AENP cluster should be blue, GNP-MNP cluster should be orange and the third cluster should be greem.
        # Also change the order of the items in the legend (txt.leg) if the colours need to be switched around.

# Adjust various plot parameters to get the desired look
scatter(dapc_k3_final,
        col = c("#0f9ee6", "#fd6f10", "#1b6e1b"), 
        legend = T,
        posi.leg = "bottomright",
        txt.leg = c("AENP Cluster", "GNP-MNP Cluster", '"Other" Cluster'),
        scree.da = T,
        posi.da = "topright",
        scree.pca = T,
        posi.pca = "topleft",
        inset.solid = 0.25,
        cell = 0,
        clab = 0,
        cstar = 0)

# Save the plot as a PDF. See ?pdf for more save options, such as png, tif etc.
pdf("dapc_plot_k3_FINAL_rgb_structure_nolines.pdf", width = 10, height = 6.5, pointsize = 12)
scatter(dapc_k3_final,
        col = c("#0f9ee6", "#fd6f10", "#1b6e1b"), 
        legend = T,
        posi.leg = "bottomright",
        txt.leg = c("AENP Cluster", "GNP-MNP Cluster", '"Other" Cluster'),
        scree.da = T,
        posi.da = "topright",
        scree.pca = T,
        posi.pca = "topleft",
        inset.solid = 0.25,
        cell = 0,
        clab = 0,
        cstar = 0)
dev.off()

