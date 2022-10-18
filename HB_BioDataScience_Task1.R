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
