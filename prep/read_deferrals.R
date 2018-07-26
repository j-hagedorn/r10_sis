# read_deferrals.R #

library(tidyverse);library(readxl)

sis_defer_1_1 <- 
  read_excel(paste0(path,"Deferral Tracking Spreadsheet 6-1-14 to 9-30-17.xlsx"),sheet = 1) %>%
  rename(MEDICAID_ID = `Medicaid ID`) %>%
  select(MEDICAID_ID)

sis_defer_1_2 <- 
  read_excel(paste0(path,"Deferral Tracking Spreadsheet 6-1-14 to 9-30-17.xlsx"),sheet = 2) %>%
  rename(MEDICAID_ID = `Medicaid ID`) %>%
  select(MEDICAID_ID)

sis_defer_1_3 <- 
  read_excel(paste0(path,"Deferral Tracking Spreadsheet 6-1-14 to 9-30-17.xlsx"),sheet = 3) %>%
  rename(MEDICAID_ID = `Medicaid ID`) %>%
  select(MEDICAID_ID)

sis_defer_1_4 <- 
  read_excel(paste0(path,"Deferral Tracking Spreadsheet 6-1-14 to 9-30-17.xlsx"),sheet = 4) %>%
  rename(MEDICAID_ID = `Medicaid ID`) %>%
  select(MEDICAID_ID)

sis_defer_2 <- 
  read_excel(
    paste0(path,"Deferral Tracking Spreadsheet 10-1-17 to Current.xlsx")
  ) %>%
  rename(MEDICAID_ID = `Medicaid ID #`) %>%
  select(MEDICAID_ID)

sis_defer <- 
  sis_defer_1_1 %>% 
  bind_rows(sis_defer_1_2,sis_defer_1_3,sis_defer_1_4,sis_defer_2) %>%
  # Clean Medicaid ID field
  mutate(
    # Trim lead / trail whitespace
    MEDICAID_ID = str_trim(MEDICAID_ID),
    # Remove alpha and special chars
    MEDICAID_ID = str_replace_all(MEDICAID_ID, "[[:alpha:]]", ""),
    MEDICAID_ID = str_replace_all(MEDICAID_ID, "[[:punct:]]", ""),
    # Convert blanks to NA
    MEDICAID_ID = ifelse(MEDICAID_ID == "", yes = NA, no = MEDICAID_ID), 
    # If string > 10 chars, include only last 10 chars
    MEDICAID_ID = ifelse(nchar(as.character(MEDICAID_ID)) > 10,
                         yes = substr(MEDICAID_ID, 
                                      start = nchar(as.character(MEDICAID_ID)) - 9, 
                                      stop = nchar(as.character(MEDICAID_ID))),
                         no = MEDICAID_ID),
    # If string < 10 chars, pad with leading zeroes
    MEDICAID_ID = ifelse(nchar(as.character(MEDICAID_ID)) < 10,
                         yes = sprintf("%010d", as.integer(MEDICAID_ID)),
                         no = MEDICAID_ID),
    # Make 'NA' & 0000000000 to NA
    MEDICAID_ID = ifelse(MEDICAID_ID %in% c("        NA","NA","0000000000"), 
                         yes = NA,
                         no = MEDICAID_ID),
    # Convert to factor
    MEDICAID_ID = as.factor(MEDICAID_ID)
  ) %>%
  mutate(
    deferral = T
  )

rm(sis_defer_1_1); rm(sis_defer_1_2);rm(sis_defer_1_3);rm(sis_defer_1_4);rm(sis_defer_2)
