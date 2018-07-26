
# Load packages
library(tidyverse); library(stringr); library(magrittr); library(lubridate)

# Define function to read and combine ####
combine_bhteds <- function(directory) {
  
  ## 'directory' is a char vector of len 1 indicating location of CSV files
  files <- list.files(directory,full.names = TRUE) # make list of full file names
  n <- length(files)
  # Create empty data frame
  df <- tibble() 
  # Loop through files, binding them together
  for (i in 1:n) {
    x <- readxl::read_excel(files[i], skip = 9)
    print(files[i])
    df <- rbind(df, x)
  } 
  df <- df %>% distinct() %>% select(-starts_with("X_"))
  # Clean colnames (rm spaces, other symbols, add underscore sep)
  names(df) <- tolower(gsub(" |-|:", "_", names(df)))
  return(df)
}

# Bind separate CMH dataframes together
open_date <- 
  combine_bhteds(directory = paste0(path,"/active_consumers")) %>%
  select(medicaid_id,agency_admission_date)

