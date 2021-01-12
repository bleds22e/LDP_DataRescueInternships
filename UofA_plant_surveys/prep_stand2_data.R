# Prep Stand 2 Data 
# January 2021

# LIBRARIES #
library(tidyverse)

# READ IN THE FILE #

# open a connection to the file we want to read in
con <- file('../../../Desktop/old versions of the missing files/s281.v.st25.txt') 
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

# CLEAN FILE #

# remove remaining white spaces
results_list <- lapply(results_list, str_trim, side = "both") %>% 
  lapply(., str_squish)

# get the rows after the metadata and unlist them so each value gets read 
# separately; otherwise, each row is one big long character string
split_list <- lapply(results_list[7:length(results_list)], 
                     str_split, 
                     pattern = " ")

#######

### Ellen's musings ###
# There are a few things that will need to be dealt to make the file usable. 
# I ~think~ the following order makes sense as a strategy:

# 1) use a combination of a for loop or apply function with an if/else statement 
# to run through each row of data 
#   - is string length greater than 2? (can use `str_length` function)
#   - if so, separate that string at the . (`separate` function)
#   - might need to run for each row multiple times until no strings are > 2
#
# some code bits that might be helpful here:
# writing the for loop like this -- for (i in 1:length(split_list)){}
# then you can run through each row using this code -- split_list[[i]][[1]]
# you can test to see if your code is working line by line by assigning a random
# value to i -- i = 4

# 2) a particularly tricky bit here is that the rows of data that we want to put
# into a data frame are split between two rows (or technically lists). AKA lists
# 1 and 2 should be all 1 row of data in the dataframe.
# We might be able to finagle a weird for loop to take any odd numbered row/list,
# add on the list right below it (row_number + 1) using `c`
# This only works if there aren't any empty rows, which fortunately seems to be the case.

# 3) The rows/lists can then be merged together into 1 data frame. We'll have to
# pull the column names from one of the other docs, I think? And then transpose
# the rows and columns before we can get it into HONDO format, but we'll cross
# that bridge when we come to it.

