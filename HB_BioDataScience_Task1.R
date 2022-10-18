# Mitochondria-endoplasmic reticulum contacts in reactive astrocytes promote vascular remodelling. Goebel et al
# Comparison of FACS-enriched astrocytes from uninjured and injured wild-type mice at different time points

# Import .csv file in R
astrocyte_data <- read.csv(file.choose())
astrocyte_data

# Data Visualization in R

# Bar plot
barplot(c(mean(astrocyte_data$MFN1_1), mean(astrocyte_data$MFN1_2), mean(astrocyte_data$MFN1_4), mean(astrocyte_data$WT_1), mean(astrocyte_data$WT_3), mean(astrocyte_data$WT_4), mean(astrocyte_data$MFN1), mean(astrocyte_data$WT))
        
# Histogram
hist(astrocyte_data$MFN1_1, col = "Sea green", main = "Distribution of MFN1_1 Gene", xlab = "Index")
        
# Lineplots
plot(astrocyte_data$MFN1_1, type = "h", xlim = c(0,1300), ylim = c(0, 40))
        
# Boxplots
boxplot(astrocyte_data$MFN1_1, astrocyte_data$MFN1_2, astrocyte_data$MFN1_4, astrocyte_data$WT_1, astrocyte_data$WT_3, astrocyte_data$WT_4, astrocyte_data$MFN1, astrocyte_data$WT)
boxplot(astrocyte_data$MFN1_1, astrocyte_data$MFN1_2, astrocyte_data$MFN1_4, astrocyte_data$WT_1, astrocyte_data$WT_3, astrocyte_data$WT_4, astrocyte_data$MFN1, astrocyte_data$WT, col = 'red', outline = F, main = 'BOXPLOT')
boxplot(astrocyte_data$MFN1_1, astrocyte_data$MFN1_2, astrocyte_data$MFN1_4, astrocyte_data$WT_1, astrocyte_data$WT_3, astrocyte_data$WT_4, astrocyte_data$MFN1, astrocyte_data$WT, col = 3:10, notch = T, outline = F, main = 'BOXPLOT', xaxt = 'n', xlab = 'Wild-type mice at different time points', ylab = 'FACS-enriched astrocytes', ylim = c(10,50), xlim = c(0,12))
# Add axis
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels = c('MFN1_1', 'MFN1_2', 'MFN1_4', 'WT_1', 'WT_3', 'WT_4', 'MFN1', 'WT'))
# Add legend
legend('topright', legend = c('MFN1_1', 'MFN1_2', 'MFN1_4', 'WT_1', 'WT_3', 'WT_4', 'MFN1', 'WT'), col = 3:10, pch = 19)
        
# Scatterplots
plot(x = astrocyte_data$MFN1_1[1:50], y = astrocyte_data$WT_1[1:50], col = 'black', pch = 21, bg = 'blue', main = 'MFN1_1 vs WT_1', xlab = 'MFN1_1', ylab = 'WT_1', xlim = c(20,30), ylim = c(20,30))
plot(astrocyte_data$MFN1_1[1:50], astrocyte_data$WT_1[1:50], col = 1:50, pch = 19, main = 'MFN1_1 vs WT_1', xlab = 'MFN1_1', ylab = 'WT_1', xlim = c(20,30), ylim = c(20,30))
legend('topright', legend = c(astrocyte_data$T..Gene.names[1:50]), pch = 19, col = 1:50)
        
# Heatmaps
heatmap(as.matrix(astrocyte_data[2:9]), Colv = NA, scale = 'col', margins = c(10,10), main = 'Heat map plot of astrocyte')
        
#Pie Charts
table(astrocyte_data$T..Gene.names[1:10])
item <- unique(astrocyte_data$T..Gene.names[1:10])
itemCount <- as.vector(table(astrocyte_data$T..Gene.names[1:10]))
pie (x = itemCount, labels = item, radius = 0.5, col = 11:20, main = 'Pie chart for astrocyte species')
pie (x = itemCount, labels = item, radius = 1.0, edge = T, border = 'red', col = 11:20, main = 'Pie chart for astrocyte species')

# Let's arrange these plots as we usually see in publications
par(mfrow = c(2,2))

        
# LET US NOW GO FULLY INTO GGPLOTS
# We are going to work with a new dataset
# About data set: Growth and Toxigenicity of A. flavus on Resistant and Susceptible Peanut Genotypes. This study seeks to determine the reaction of peanut genotypes to  Aflatoxigenic and non-aflatoxigenic A. flavus inoculation and also determine the mechanisms of their resistance. It was established that non-aflatoxigenic A. flavus grows faster than aflatoxigenic A. flavus. There was no significant difference in the incidence and severity of the A. flavus resistant genotypes (L027B and ICGV-03401) however, there were significant differences between resistant genotypes and the susceptible check Manipinta.  This study also confirmed that non-aflatoxigenic A. flavus inoculation did not lead to aflatoxin production. Non-aflatoxigenic A. flavus identified could serve as  a good biocontrol against aflatoxin contamination under field conditions. Additionally, peanut genotypes with resistance to post-harvest aflatoxin accumulation will resist the growth of A. flavus and subsequent aflatoxin accumulation.

# Install ggplot2
install.packages("ggplot2")
library(ggplot2)

# Import .csv file in R
aflatoxin_data <- read.csv(file.choose())
aflatoxin_data

# Start with defining the base data for ggplot
pl <- ggplot(data = aflatoxin_data)

# To the base, add what you want to plot and color
pl + geom_bar(aes(x=GENOTYPES))

# Plot frequency of data from a single column
pl + geom_bar(aes(y=Isolate))

# Flip the coordinates
pl + geom_bar(aes(y=Isolate))+coord_flip()

# See the frequency of each variable within the species.
# Install.package(reshape2)
# Melt the data to have something understandable by ggplot better
install.packages("reshape2")
install.packages("Rcpp")
library(reshape2)
melted_aflatoxin_data <- melt(aflatoxin_data)
# Stacked/continuous bar plot.
ggplot(data = aflatoxin_data) + geom_bar(aes(x=GENOTYPES, y=Isolate, fill = GENOTYPES), stat = "identity")
# Multiple bar plot
ggplot(data = aflatoxin_data) + geom_bar(aes(x=GENOTYPES, y=Isolate, fill = GENOTYPES), stat = "identity", position = 'dodge')

#Pie chart
ggplot(data = aflatoxin_data) + geom_bar(aes(x=factor(1), fill = GENOTYPES), width = 1) + coord_polar(theta = 'y')

# Remember to add theme you like
ggplot(data = aflatoxin_data) + geom_bar(aes(x=factor(1), fill = GENOTYPES), width = 1) + coord_polar(theta = 'y') + theme_classic()

# Histogram
# pl <- ggplot(data = aflatoxin_data)
pl + geom_histogram(aes(x=S_Day2, fill = GENOTYPES)) + facet_grid(. ~GENOTYPES)
#Add a color parameter
pl + geom_histogram(aes(x=S_Day2, fill = GENOTYPES), color = "black") + facet_grid(. ~GENOTYPES)

# Scatter plots
pl + geom_point(aes(x = S_Day2, y = S_Day8, color = 'red'))
# Color by Genotype; quite automated
pl + geom_point(aes(x = S_Day2, y = S_Day8, color = GENOTYPES)) + theme_bw() + ggtitle(label = 'Scatterplot', subtitle = 'Days')

# Boxplot plus a theme
pl + geom_boxplot(notch = F, aes(x= GENOTYPES, y = S_Day8, fill = GENOTYPES)) + theme_classic()

# Heat map
# Using your melting methods, create heat map with ggtiles
meltCorData <- melt(cor(aflatoxin_data[3:8]))
# Set your new ggplot
hm <- ggplot(data = meltCorData)
hm + geom_tile(aes(x = Var1, y = Var2, fill = value))
# Coloring in heatmaps
hm + geom_tile(aes(x = Var1, y = Var2, fill = value)) + scale_fill_gradient2(low = 'red', high = 'blue')
# You can also use hex code
hm + geom_tile(aes(x = Var1, y = Var2, fill = value)) + scale_fill_gradient2(low = '#1687ee', high = '#d27C1c')
# Change x and y labels
hm + geom_tile(aes(x = Var1, y = Var2, fill = value)) + scale_fill_gradient2() + xlab('First Variable') + ylab('Second Variable')

# Let's arrange these plots as we usually see in publications
par(mfrow = c(2,2))
