
library(tidyverse); library(lubridate); library(stringr)

#### Read in QI data ####

path <- "C:/Users/joshh/OneDrive - TBD Solutions LLC/files/Region10/SIS/"

qi <- read_csv(paste0(path,"QI 2017-05-31.csv"))

# Remove cols where all values are NA
qi <- Filter(function(x)!all(is.na(x)), qi)

# Clean colnames (rm spaces, other symbols, add underscore sep)
names(qi) <- gsub(":|[[:punct:]]", "", names(qi))
names(qi) <- gsub(" |-", "_", names(qi))

# Clean data to prepare for analysis
qi %<>%
  # Clean Medicaid ID field
  rename(MEDICAID_ID = MEDICAID_RECIP_ID) %>%
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
  # Change numeric ID vars to characters
  mutate_at(
    .cols = vars(DISABILITYDD:MAJOR_MENTAL_ILLNESS,-DATE_OF_BIRTH),
    .funs = funs(as.character)
  ) %>%
  # Change all character columns to factors
  mutate_if(is.character,as.factor) %>%
  # Clean date fields to prepare for analysis
  mutate(
    DATE_OF_BIRTH = ymd(DATE_OF_BIRTH)
  ) 

#### Read in services data ####

source("C:/Users/joshh/Documents/GitHub/r10_autism/prep/read_autism_svs.R")

#### Read in SIS data ####

## Define local source of SIS data
sis_src <- "C:/Users/joshh/OneDrive - TBD Solutions LLC/files/Region10/SIS/SIS Online Full Report 6-8-17.xlsx"

library(readxl)
sis_full <- read_excel(sis_src,na = c(""," ","NA","Choose one","Enterprise Wide"))
sis_full <- sis_full[-1, ] # Remove 1st row (col descriptions)
names(sis_full)[1] <- "sis_id" # Rename 1st col
sis_full$sis_id <- as.factor(sis_full$sis_id)

## Transform using standard script
# Returns 0 records of processed dfs (just ran lines 1-78)
# source("https://raw.githubusercontent.com/j-hagedorn/exploreSIS/master/prep/readSIS.R")

sis <- 
  sis_full %>% 
  # Filter Status == Completed
  filter(statusText %in% c("COMPLETED")) %>% 
  # Remove text fields
  select(-ends_with("notes")) %>% 
  # Format datetime fields
  mutate(# Remove hms from sis_completed_dt and convert it
    sis_completed_dt = gsub(" .*$","",as.character(sis_completed_dt)),
    # Convert numeric from Excel
    sis_completed_dt = as.Date(as.numeric(sis_completed_dt), origin="1899-12-30"),
    # Add space between time and 'AM/PM' to allow conversion
    sis_startTime = gsub('([0-9])([[:alpha:]])', '\\1 \\2', sis_startTime),
    sis_endTime = gsub('([0-9])([[:alpha:]])', '\\1 \\2', sis_endTime),
    # Combine date and time to create POSIX object
    sis_startTime = paste(sis_completed_dt,as.character(sis_startTime),sep = " "),
    sis_endTime = paste(sis_completed_dt,as.character(sis_endTime),sep = " "),
    # Deal with date formatting across SIS-A update
    start = if_else(sis_completed_dt >= "2016-10-01",
                    true = ymd_hm(sis_startTime),
                    false = ymd_hms(sis_startTime)),
    end = if_else(sis_completed_dt >= "2016-10-01",
                  true = ymd_hm(sis_endTime),
                  false = ymd_hms(sis_endTime)),
    # Truncated arg deals with diff formating of SIS-A field
    sis_cl_dob_dt = mdy_hms(sis_cl_dob_dt, truncated = 3),
    statusChangeDate = mdy_hms(statusChangeDate),
    dateUpdated = mdy_hms(dateUpdated),
    isp_begin_date = mdy(isp_begin_date),
    # Calculated fields using datetime vars
    duration = as.numeric(difftime(end, start, units = "mins")),
    DaysSince = as.POSIXct(today()) - as.POSIXct(sis_completed_dt),
    age = floor((as.POSIXct(sis_completed_dt) - sis_cl_dob_dt)/365.242),
    # Create week and annual dates for grouping
    sis_wk = week(sis_completed_dt),
    sis_yr = year(sis_completed_dt),
    sis_yrwk = floor_date(sis_completed_dt, unit = "week")
  ) %>%
  # Clean Medicaid ID field
  mutate(
    mcaid_id = sis_track_num, # map correct field
    # Trim lead / trail whitespace
    mcaid_id = str_trim(mcaid_id),
    # Remove alpha and special chars
    mcaid_id = str_replace_all(mcaid_id, "[[:alpha:]]", ""),
    mcaid_id = str_replace_all(mcaid_id, "[[:punct:]]", ""),
    # Convert blanks to NA
    mcaid_id = ifelse(mcaid_id == "", yes = NA, no = mcaid_id), 
    # If string > 10 chars, include only last 10 chars
    mcaid_id = ifelse(nchar(as.character(mcaid_id)) > 10,
                      yes = substr(mcaid_id, 
                                   start = nchar(as.character(mcaid_id)) - 9, 
                                   stop = nchar(as.character(mcaid_id))),
                      no = mcaid_id),
    # If string < 10 chars, pad with leading zeroes
    mcaid_id = ifelse(nchar(as.character(mcaid_id)) < 10,
                      yes = sprintf("%010d", as.integer(mcaid_id)),
                      no = mcaid_id),
    # Make 'NA' & 0000000000 to NA
    mcaid_id = ifelse(mcaid_id %in% c("        NA","NA","0000000000"), 
                      yes = NA,
                      no = mcaid_id),
    # Convert to factor
    mcaid_id = as.factor(mcaid_id)
  )

sis_ids <- 
  sis %>% 
  # Filter out assessments which will be expired (> 3 yrs old) by due date
  filter(as.Date("2017-09-30") - as.Date(sis_completed_dt) <= (365 * 3)) %>%
  select(mcaid_id,sis_id) %>%
  ungroup()

rm(sis_full)

