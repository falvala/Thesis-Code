## -----------------------------------------------------------------------------
## Clear RStudio
rm(list = ls())  # Clear the global environment
graphics.off()  # Clear all plots
cat("\014")  # Clear the console
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## Setting R to fetch the data from my saved location
getwd()
setwd("C:/Users/tsamu/OneDrive/Documents/SU/Year 2/Thesis/Data/Facereader")

# Importing the data:
# Dyad 4
# Data frames 'df_4A' for participant A and 'df_4B' for participant B.

df_4a <- read.table("4A.txt", sep = '\t', header = TRUE,  skip = 12,
                 na.strings = c("."))

df_4B <- read.table("4B.txt", sep = '\t', header = TRUE,  skip = 12, 
                 na.strings = c("."))

# Merging two data frames (for 4A & 4B) by 'Video.Time'
# i.e. frames per second (fps)

df4_dyad <- merge.data.frame(x = data_frame1, y = data_frame2, 
by = "Video.Time", all = TRUE)

# Creating a correlation matrix to assess simple correlations for 
# all variables.

cor_mat4 <- cor(data_frame3[,unlist(lapply(data_frame3, is.numeric))], 
               use = "complete.obs")

# Plotting two the variables 'Arousal' for both participants in the dyad
# Load package 'ggpubr'

library(ggpubr)

# A basic scatter plot to perform an 'eye test' on the data, looking for 
# distribution and outliers

test_plot4 <- ggscatter(df4_dyad, x = "Arousal.x", y = "Arousal.y", 
                       cor.coef = TRUE, cor.method = "spearman",
                       xlab = "Arousal - A", ylab = "Arousal - B")

# Participant A on the x-axis and Participant B on the y
# Print the plot

test_plot4

## Subset data based on specific correlations of interest
# In order to do this we need to extract the correlation matrix as a data frame

df4_cor_new <- as.data.frame(cor_mat4)

# Sanity check - good to check the structure and appearance of the data

str(df4_cor_new)
head(df4_cor_new) 

# Load package 'dplyr' pipe operator and 'filter' functions

library(dplyr)

# Extract only correlations from 'df4_cor_new' in which the absolute value of 
# the correlation between Arousal.x is at least 0.40 or below -0.40:

Cor_Arous <- df4_cor_new %>% filter(Arousal.x > 0.4 | Arousal.x < -0.4)




