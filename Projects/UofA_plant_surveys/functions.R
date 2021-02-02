# FUNCTIONS #

read_in_txt_file <- function(file_path){
  
  ### READ IN FILE ###
  ## this attempt using a txt file
  
  # open a connection to the file we want to read in
  con <- file(file_path) 
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
  
  return(results_list)
  
}


txt_file_to_df <- function(results_list){
  
  ### TURN INTO DATAFRAME ###
  
  # remove remaining white spaces and make everything uppercase
  results_list <- lapply(results_list, str_trim, side = "both") %>% 
    lapply(., str_squish) %>% 
    lapply(., str_to_upper)
  
  # get the rows after the metadata and unlist them so each value gets read 
  # separately; otherwise, each row is one big long character string
  split_list <- lapply(results_list[1:length(results_list)], 
                       str_split, 
                       pattern = " ")
  
  # find first "quad" row and cut out the rest
  quad_rows <- vector()
  for (i in 1:length(split_list)){
    if (split_list[[i]][[1]][1] == 'QUAD') {
      quad_rows <- c(quad_rows, i)
    }
  }
  
  split_list <- split_list[min(quad_rows):length(split_list)]
  
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
  cover_df <- data.frame(matrix(unlist(split_list), 
                                nrow = length(split_list), 
                                byrow = T)) %>% 
    # remove the empty rows
    janitor::remove_empty("rows") %>% 
    # make the first row ("QUAD") into the column names
    janitor::row_to_names(., row_number = 1) %>% 
    # remove any remaining "QUAD" rows --- filter throws an error, for some reason
    .[.$QUAD != 'QUAD',]
  
  # make tidy 
  colnames(cover_df) <- make.unique(colnames(cover_df))
  cover_df_long <- rename(cover_df, "Species" = "QUAD") %>% 
    pivot_longer(2:ncol(cover_df), names_to = "Quad") %>% 
    rename("Cover" = "value")
  
  return(cover_df_long)
  
}


filename_to_metadata <- function(file_path){
  
  # fxn to read in HONDO file names and pull out metadata (stand, month, year)
  myfiles <- as.data.frame(list.files(file_path, 
                                      full.names = FALSE, 
                                      pattern = "*.txt"))
  colnames(myfiles) <- c("file_name")
  
  myfiles <- myfiles %>% 
    separate(file_name, c("hondo", "month", NA)) %>% 
    mutate("month" = str_to_upper(month),
           "year" = as.numeric(str_sub(hondo, start = -2)),
           "stand" = str_sub(hondo, start = -3, end = -3)) %>% 
    mutate("year" = as.character(if_else(year < 50, year + 2000, year +1900))) %>% 
    mutate("month" = ifelse(month == 'JAN', 1, 
                            ifelse(month == 'FEB', 2,
                                   ifelse(month == 'MAR', 3,
                                          ifelse(month == 'APR', 4, 
                                                 ifelse(month == 'MAY', 5,
                                                       ifelse(month == 'JUN', 6,
                                                               ifelse(month == 'JUL', 7,
                                                                     ifelse(month == 'AUG', 8,
                                                                             ifelse(month == 'SEP', 9, 
                                                                                   ifelse(month == 'OCT', 10, 
                                                                                          ifelse(month == 'NOV', 11,
                                                                                                 ifelse(month == 'DEC', 12, NA))))))))))))) %>% 
    select(-hondo)
  
  return(myfiles)
  
}
  
