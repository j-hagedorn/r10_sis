# run_sis_complete.R

# Define local directory
directory <- "C:/Users/joshh/OneDrive - TBD Solutions LLC/files/Region10/SIS"

# Manually update file names
sis_src <- paste0(directory,"/SIS Online Output 10-01-19.xlsx")
qi_src <- paste0(directory,"/qi/QI 2019.09.27 FY19.csv")

# Read services data
source("prep/read_svs.R")

# Read QI data
source("prep/readQI.R")

# Read BH-TEDS data
source("prep/read_bhteds.R")

# Read deferrals data
source("prep/read_deferrals.R")

# Make reports and summary
source("prep/getSISelig.R")

