# FACE READER- READER
###############################################################################

# 12/04/2023, Fátima Álvarez. Update of Thomas' script from Stockholm University   

# Reading automatization for Face Reader files. The script takes Face Reader 
# text files for each participant and returns a) two comma-delimited files, 
# containing: (1)the merged data of the dyad and (2) a correlation matrix

# In the last section, the script read data from xls files and merged data in a 
# final dataset.

## -----------------------------------------------------------------------------
## Clear RStudio
rm(list = ls())  # Clear the global environment
graphics.off()  # Clear all plots
cat("\014")  # Clear the console

## -----------------------------------------------------------------------------
# (0) Configuration settings, IMPORTANT :

# Please, hard code configuration or load a configuration file:

npairs <- 6 ; # set how many pair you have!
nvariables <- 17; # select how many variables you need from the data (17: till "Roll")
final_variables <- c("Dyad","Valence", "Arousal", "Pitch", "Yaw", "Roll")
# note: Add in "final variables" the variables that you'll need and it will be 
# included in the final dataset
save_files <- 0 # set 1 to save dataframes 
save_plots <- 0 # set 1 to save plots

# Gets libraries and packages ready (?):

library(ggplot2)
library(readxl)
library(ggpubr)
library(tidyverse)
# install.packages("openxlsx")
library("openxlsx")

##################################################################### RUN NOW

## -----------------------------------------------------------------------------
## (1) Set path where data is:

path <- getwd() # FIRST: open it from RStudio in the files tab 
setwd (path)

final_dataset<-  matrix(nrow = npairs, ncol = length(final_variables));


## (2) Import data: initiate loop to import data.

# Loop to import data from FaceReader -------------------------------------
for (pair in 1:npairs){

    # import data from participant A
    d1 <- read.table(paste("Pair",as.character (pair), "A" ,".txt", sep=""), 
                     sep = '\t', header = TRUE,  skip = 12, na.strings = c("."))
    d1 <- d1 [, 1:nvariables]; 
    
    # import data from participant B
    d2 <- read.table(paste("Pair",as.character (pair), "B" ,".txt", sep=""),
                     sep = '\t', header = TRUE,  skip = 12, na.strings = c("."))
    d2 <- d2 [, 1:nvariables]; 
    
    # merge two data frames by time points
    d3 <- merge(d1,d2,by="Video.Time");

    # (3) generate a correlation matrix
    cor_mat <- cor(d3[,unlist(lapply(d3, is.numeric))], use = "complete.obs");
    
    # save dataframe and correlation matrix for this pair :
    if (save_files== 1 ) {
      write.csv(d3, file = paste("dataframe_dyad_" , as.character (pair),
                                 ".csv", sep=""), row.names = FALSE)
      write.csv(cor_mat, file = paste("matrixco_dyad_" , as.character (pair),
                                      ".csv", sep=""), row.names = FALSE)
    } else {
      # must be empty
    }
    
    # save correlation in the final dataset
    for (v in 2:length(final_variables)){
    final_dataset[pair, 1] <- pair;
    corA<- paste(final_variables[v],".x", sep="")
    corB<- paste(final_variables[v],".y", sep="")
    final_dataset[pair, v] <- cor_mat[corA, corB]; # correlation value
        }
    
    # (4) plot Arousal (as an example) and save it:
    # test_plot <- plot(d3$Arousal.x, d3$Arousal.y,
    #      main = "scatter plot",
    #      xlab = "Arousal.A",
    #      ylab = "Arousal.B")
    
    test_plot <- ggscatter(d3, x = "Arousal.x", y = "Arousal.y", 
                           cor.coef = TRUE, cor.method = "pearson",
                           xlab = "Arousal - A", ylab = "Arousal - B")
    test_plot
    
    if (save_plots== 1 ) {
      ggsave(file = paste("graph_dyad_", as.character (pair), ".png", sep=""), plot = test_plot, width = 6, height = 4, dpi = 300)
    } else {
      # must be empty
    }
    
} # loops ends

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

## (5) Generates final_dataset:

# final_dataset -----------------------------------------------------------
colnames(final_dataset)<-final_variables
print(final_dataset)

# read excell data and merge it by pairs:
survey <- read_excel ("Survey_Stats.xlsx", sheet =3, skip = 1)
final_dataset <- merge(final_dataset,survey,by="Dyad")

# read excell data and merge it individually (A and B role):
survey_role <- read_excel ("Survey_Stats.xlsx", sheet =1, skip = 1)
survey_role$role <- substr(survey_role$ID, nchar(survey_role$ID), nchar(survey_role$ID))
vv <-names(survey_role)

# gives name to the columns in the dataframe
subtable <- subset(survey_role, select = c(vv[4],vv[26],vv[9],vv[14], vv[19],vv[24]))
vv<- names (survey)
names(subtable)[3:length(subtable)] <- names(survey)[2:5]

# reshape data for dyad and role
subtable<- subtable[order(subtable$Dyad, subtable$role),]
subtable<- subtable %>%
  tidyr::pivot_wider(names_from = role, values_from = c(Outcome, Self, Process, Relationship))

# merge new data in a the final dataset
final_dataset <- merge(final_dataset,subtable,by="Dyad")


# Saves final dataset -----------------------------------------------------

# saves it in an EXCEL file if you asked for it at the beggining
if (save_files== 1 ) {
  # write.csv(final_dataset, file = "final_dataset.csv", row.names = FALSE)
  write.xlsx(final_dataset, paste(path, "/final_dataset.xlsx", sep=""), rowNames = FALSE)
  
} else {
  # must be empty
}

##------------------------------------------------------------------------ END
