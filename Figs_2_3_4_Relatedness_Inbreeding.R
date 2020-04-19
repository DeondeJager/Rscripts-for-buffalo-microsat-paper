############################################################################################################################
#### Construct relatedness and inbreeding figures from the manuscript, and calculate related statistics ####


############################################################################################################################
#### Import data and load required packages ####

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("moments")  #This package can calculate skewness and kurtosis of distributions.

library(ggplot2)
library(dplyr)
library(reshape2)
library(moments)

# Import data:
relatedness <- read.csv("Relatedness_TrioML.csv", header = T)
inbreeding <- read.csv("Inbreeding_TrioML.csv", header = T)

# Reshape data for ggplot2 (using the reshape2 package)
# In order to plot each population in a separate plot, using facet_wrap() from ggplot2, 
# we need to transform the dataframe, for which we need the reshape2 package
relatedness_melt <- melt(relatedness,  id.vars = 'Dyad', variable.name = 'Population')
inbreeding_melt <- melt(inbreeding, id.vars = "Individual", variable.name = "Population")
# Look at the data to see how it has been transformed

############################################################################################################################
#### Fig 2 ####
#### Construct relatedness distribution plots ####


# First calculate the mean for each group (i.e. Population), using functions from the dplyr package
grp_mean_rel <- relatedness_melt %>%
  group_by(Population) %>%
  summarise(value = mean(value, na.rm = T))
# View results
grp_mean_rel

# To add the mean relatedness at the maxiumum y-value 
# Extract the max density points from underlying ggplot data (to position text at ymax)
# Note: There must be an easier way to do this.
m <- ggplot(relatedness_melt) +
  geom_histogram(aes(value)) +
  facet_wrap(~Population, scales = "free_y") +
  labs(x = "Relatedness", y = "Density") +
  geom_vline(data = grp_mean_rel, aes(xintercept = value), size = 0.75, linetype = "dashed") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "lightgrey", size = 0.5),
        panel.grid.minor = element_blank()) #+
p <- ggplot_build(m) # Extract underlying data
head(p$data) # View underlying data and then save underlying data as csv:
write.csv(p$data, "ggplot_data.csv")  # From this file, 
                                      # extract ymax for each panel manually in Excel, save as csv and load back into R:
ymax_density_relatedness <- read.csv("ymax_density_relatedness.csv", header = T)

# Then we can finally contruct the plot:
ggplot(relatedness_melt) +
  geom_histogram(aes(value), bins = 30) +
  facet_wrap(~Population, scales = "free_y") +
  labs(x = "Relatedness", y = "Density") +
  geom_vline(data = grp_mean_rel, aes(xintercept = value), size = 0.6, linetype = "dashed", alpha = 0.5) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "lightgrey", size = 0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(face = "bold")) +
  geom_text(data = grp_mean_rel, aes(x = value + 0.12, 
                                     y = (ymax_density_relatedness$ymax)*0.90, 
                                     label = signif(value, 2)), 
            size = 4)

# We then store the plot as an object in R, to make saving it easier using the ggsave() function
relatedness_plot <- ggplot(relatedness_melt) +
  geom_histogram(aes(value), bins = 30) +
  facet_wrap(~Population, scales = "free_y") +
  labs(x = "Relatedness", y = "Density") +
  geom_vline(data = grp_mean_rel, aes(xintercept = value), size = 0.6, linetype = "dashed", alpha = 0.5) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "lightgrey", size = 0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(face = "bold")) +
  geom_text(data = grp_mean_rel, aes(x = value + 0.12, 
                                     y = (ymax_density_relatedness$ymax)*0.90, 
                                     label = signif(value, 2)), 
            size = 4)

# Save plot as png using ggsave
ggsave("relatedness_plot.png", 
       plot = relatedness_plot, 
       scale = 1,
       width = 297,
       height = 210,
       units = "mm",
       dpi = 600)

#### Calculate skewness, kurtosis and variance of relatedness distributions, for inclusion in SI table ####
# Load required package
library(moments)

# Skewness
skewness_rel <- relatedness_melt %>%
  group_by(Population) %>%
  summarise(skewness = skewness(value, na.rm = T))
skewness_rel

# Kurtosis
kurtosis_rel <- relatedness_melt %>%
  group_by(Population) %>%
  summarise(kurtosis = kurtosis(value, na.rm = T))
kurtosis_rel

# Variance in relatedness
variance_rel <- relatedness_melt %>%
  group_by(Population) %>%
  summarise(variance = var(value, na.rm = T))


############################################################################################################################
#### Fig 3 ####
#### Relationship of kurtosis & proportion values >= 0.25, for relatedness ####

# Import data
kurtosis_vs_025 <- read.csv("kurtosis_proportiongreater025.csv", header = T)

#install.packages("ggpubr")
library(ggpubr) # Contains functions to add correlation stats to ggplot (stat_cor)
#install.packages("ggrepel")
library(ggrepel) # To offset point labels in scatterplot (geom_text_repel)

ggplot(kurtosis_vs_025) +
  geom_point(aes(Kurtosis, Proportion0.25), size = 3, colour = "lightcoral") +
  geom_text_repel(aes(Kurtosis, Proportion0.25, label = Locality), point.padding = 0.1) +
  #geom_smooth(aes(Kurtosis, Proportion0.25)) + 
  stat_cor(aes(Kurtosis, Proportion0.25), 
           method = "pearson",
           label.sep = "\n",
           label.x = 13.5,
           label.y = 0.20) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        axis.text = element_text(size = 10)) +
  labs(x = "Kurtosis (of r distribution)", y = "Proportion of r >= 0.25")

# I changed the "r" in the axes to italics by editing the plot in Inkscape (after exporting it as a pdf)

# Can also use the "last_plot" argument in ggsave to save the last plot you contructed 
# (instead of storing it as an object first)
ggsave("kurtosis_vs_proportion025.png",
       plot = last_plot(),
       scale = 1,
       width = 297,
       height = 210,
       units = "mm",
       dpi = 600)

############################################################################################################################
#### Fig 4 ####
#### Plot inbreeding estimates ####

# Calculate the mean for each population
grp_mean_inb <- inbreeding_melt %>%
  group_by(Population) %>%
  summarise(value = mean(value, na.rm = T))
# View
grp_mean_inb

# Boxplot of inbreeding, also showing mean (the turqoise square)
ggplot(inbreeding_melt) +
  geom_boxplot(aes(Population, value)) + 
  geom_point(data = grp_mean_inb, aes(Population, value), colour = "turquoise3", size = 2, shape = "square") +
  ylim(0,1) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(),
        axis.text = element_text(size = 10)) +
  labs(x = "Locality", y = expression("Individual inbreeding coefficient\ " (italic("F"))))

# Store plot as an object to make saving it easier
inbreeding_plot <- ggplot(inbreeding_melt) +
  geom_boxplot(aes(Population, value)) + 
  geom_point(data = grp_mean_inb, aes(Population, value), colour = "turquoise3", size = 2, shape = "square") +
  ylim(0,1) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(),
        axis.text = element_text(size = 10)) +
  labs(x = "Locality", y = expression("Individual inbreeding coefficient\ " (italic("F"))))

ggsave("inbreeding_plot.png", 
       plot = inbreeding_plot, 
       scale = 1,
       width = 297,
       height = 210,
       units = "mm",
       dpi = 600)

#### Calcuate variance in inbreeding estimates, for inclusion in a supplementary table ####
# Variance in inbreeding
variance_inb <- inbreeding_melt %>%
  group_by(Population) %>%
  summarise(variance = var(value, na.rm = T))


############################################################################################################################
#### Combine relatedness and inbreeding stats into excel file ####

#install.packages("xlsx)
library(xlsx)

# Combine everything into a dataframe in r:
relatedness_inbreeding_stats <- data.frame(grp_mean_rel, variance_rel$variance, skewness_rel$skewness, kurtosis_rel$kurtosis, grp_mean_inb$value, variance_inb$variance)

# Change column names in dataframe, since it can't be done within the write.xlsx2 function
colnames(relatedness_inbreeding_stats) <- c("Locality", "Mean r", "Variance r", "Skewness r", "Kurtosis r", "Mean F", "Variance F")

write.xlsx2(relatedness_inbreeding_stats,
            "Relatedness_Inbreeding_stats.xlsx",
            col.names = T,
            row.names = F)


############################################################################################################################
#### Bonus section ####
# Here are some other ways to visualise the relatedness distributions that I didn't end up using in the manuscript

# Density plots (like histograms, but with a line instead)
ggplot(relatedness_melt) +
  geom_density(aes(value), stat = "density") +
  facet_wrap(~Population, scales = "free_y") +
  labs(x = "Relatedness", y = "Density")

# All on the same axes
ggplot(relatedness_melt) +
  geom_density(aes(value, colour = Population), stat = "density") +
  labs(x = "Relatedness", y = "Density")

# Make boxplots
ggplot(relatedness_melt) +
  geom_boxplot(aes(Population, value))
# This doesn't look very useful. Most of the values are close to zero. Maybe jitter plots will be better?

# Make jitter plot
ggplot(relatedness_melt) +
  geom_jitter(aes(Population, value))
# Nope, too many points leads to overplotting

# What about an violin plot
ggplot(relatedness_melt) +
  geom_violin(aes(Population, value))
# Not terrible, as you can see some differences in the shape of the violins, but I prefer the histograms.
