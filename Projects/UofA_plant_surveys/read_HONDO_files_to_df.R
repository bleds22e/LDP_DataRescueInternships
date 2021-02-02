# Reading vascular data
# EKB; January 2021

### LIBRARIES ###

#install.packages("groundhog")
library(groundhog)
groundhog.library(tidyverse, "2021-01-01")
groundhog.library(janitor, "2021-01-01")
source("UofA_plant_surveys/functions.R")

### FILES ###

stands <- c("Stand 1", "Stand 2 (missing some)", "Stand 3", 
            "Stand 4 (maybe missing some)", "Stand 5", "Stand 6", "Stand 7", "Stand 8")
list_names <- c("Stand_1", "Stand_2", "Stand_3", "Stand_4", 
                "Stand_5", "Stand_6", "Stand_7", "Stand_8")
df_list <- sapply(list_names, function(x) NULL)

for (s in 1:length(stands)) {
  
  ## PREP DATA ##
  
  # make df with .txt file paths
  file_path <- paste0("UofA_plant_surveys/Vascular Plant Surveys/", stands[s])
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
    cleaned_list[[f]] <- f4
    
  }
  
  # make one big dataframe
  long_data <- do.call(rbind, cleaned_list)
  
  # pull out temp data and put in it's own df
  cover_data <- long_data %>% 
    filter(Species != 'TEMP')
  
  temp_data <- long_data %>% 
    filter(Species == 'TEMP',
           as.numeric(Cover) > 0) %>%
    rename("Temp_F" = "Cover") %>% 
    select(-Species)
  
  # make species data wide
  cover_data <- cover_data %>% 
    select(Month, Year, Quad, Stand, Species, Cover) %>% 
    pivot_wider(id_cols = c("Month", "Year", "Quad", "Species"),
                names_from = Species, 
                values_from = Cover, 
                values_fill = NA)
  
  # make list for export
  df_list[[s]] <- list(cover_data, temp_data)
  
}


  
### looking for duplicates 

stand8_dups <- cover_data[which(duplicated(cover_data)),]

cover_data2 <- anti_join(cover_data, stand7_dups) %>% 
  distinct(Month, Year, Quad, Species)
cover_data3 <- anti_join(cover_data, cover_data2)
