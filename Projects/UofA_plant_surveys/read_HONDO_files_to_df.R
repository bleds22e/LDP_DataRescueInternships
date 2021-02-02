# Reading vascular data
# EKB; January 2021

### LIBRARIES ###

#install.packages("groundhog")
library(groundhog)
groundhog.library(tidyverse, "2021-01-01")
groundhog.library(janitor, "2021-01-01")
source("UofA_plant_surveys/functions.R")

### FILES ###

## PREP DATA ##

# make df with .txt file paths
file_path <- "UofA_plant_surveys/Vascular Plant Surveys/Stand 8"
myfiles <- as.list(list.files(file_path, full.names = TRUE, pattern = "*.txt"))

# make df with metadata pulled from file names
metadata <- filename_to_metadata(file_path)

## PROCESS FILES ##

# empty list for processed dataframes
cleaned_list <- list()

for (f in 1:length(myfiles)) {
  
  # read in file, clean, and turn into long df
  f2 <- myfiles[[f]]
  f3 <- read_in_txt_file(f2)
  f4 <- txt_file_to_df(f3)
  
  # add metadata from file name
  f4$Month <- metadata$month[f]
  f4$Year <- metadata$year[f]
  f4$Stand <- metadata$stand[f]
  
  # add to a list
  cleaned_list[f] <- f4
  
}
