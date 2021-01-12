# Reading vascular data
# EKB; January 2021

### LIBRARIES ###

#install.packages("groundhog")
library(groundhog)
groundhog.library(tidyverse, "2021-01-01")
groundhog.library(janitor, "2021-01-01")

### NOTES ###

# Right now, this script does the following:
# 1. Reads in a text file. We lose the metadata located in the file name.
# 2. Pulls out the first line of the file and saves it as metadata.
# 3. Removes extra white-space and makes each list the same length 
# 4. Binds the lists together to be rows in one dataframe, names the first row
#     into column names, drops empty rows and any rows that repeat the column names.
# 5. Makes a list that contains both the cleaned dataframe and the metadata

# Ideally:
# 1. We either convert to text files in R or figure out how to do this with 
#    whatever files they currently are.
# 2. Fix some of the hard-coding.
# 3. Retain the metadata from the file names.
# 4. Maybe we want to restructure the files once we've read them all in?
#     a. read in a file and save the metadata from in (site, stand, year, month)
#     b. pull out metadata row
#     c. make the cross-tab data as above
#     d. save all above in 3 big lists then rotate through and make 1 giant df?
#         - columns for site, stand, year, month, quad, species, value, temp?, 
#           and a notes column for the metadata row 
#         - or keep some of the cross-tab part? or one df for each stand? 

### READ IN FILE ###
## this attempt using a txt file

# open a connection to the file we want to read in
con <- file('../../../Desktop/HONDO181.txt') 
open(con)

# make a list to put the results into
results_list <- list()

# start with the first line in the file and cycle through all of them
current_line <- 1
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  results_list[current_line] <- line
  current_line <- current_line + 1
} 

# close the connection to the file
close(con)

### TURN INTO DATAFRAME ###

# remove remaining white spaces
results_list <- lapply(results_list, str_trim, side = "both") %>% 
  lapply(., str_squish)

# save first list as metadata to be attached later
# are all the first rows metadata?
metadata <- results_list[[1]]
  
# get the rows after the metadata and unlist them so each value gets read 
# separately; otherwise, each row is one big long character string
split_list <- lapply(results_list[3:length(results_list)], 
                     str_split, 
                     pattern = " ")
 
#### NOTE ####
# when we code this for running through multiple files, we'll want a loop to run
# through all the files to find the longest row and set max_length to that value
 
# in order to bind the list together as rows, they need to be the same length.
for (i in 1:length(split_list)){
    
  # get length of row
  # 31 is assuming only 1 sub-stand in ever surveyed twice
  # if 31 is not the maximum length, we'll need to edit this code
  max_length <- 31
  row_length <- length(split_list[[i]][[1]])
  
  # for lists that start with "QUAD," find length of "row" (actually a list)
  if (row_length > 1){
    
    # this code adds NAs to the row to match max_length
    if (row_length < max_length) {
      add_NAs <- vector(mode = "character", length = (max_length - row_length)) %>% 
        na_if("")
      split_list[[i]][[1]] <- c(split_list[[i]][[1]], add_NAs)
    }

      # for lists that are empty, put the appropriate number of NAs in, based on the
  # length of the corrected "QUAD"  list  
  } else if (row_length <= 1) {
    split_list[[i]][[1]][1:max_length] <- NA
    
  } else {
    print("ERROR: value greater than 31 possibly detected")
    # ideally, would want to print which file once this is written for multiple
  }
    
}
  
# stitch lists together to act as rows in a dataframe
data <- data.frame(matrix(unlist(split_list), 
                          nrow = length(split_list), 
                          byrow = T)) %>% 
  # remove the empty rows
  janitor::remove_empty("rows") %>% 
  # make the first row ("QUAD") into the column names
  janitor::row_to_names(., row_number = 1) %>% 
  # remove any remaining "QUAD" rows
  filter(QUAD != "QUAD")

# make a list to keep metadata together
data_and_metadata <- list(data, metadata)

