# run_sis_complete.R

# Read services data
source("prep/read_svs.R")

# Read QI data
path <- "C:/Users/joshh/OneDrive - TBD Solutions LLC/files/Region10/SIS/"
qi <- read_csv(paste0(path,"QI 2018-02-15.csv"))
source("prep/readQI.R")

# Read deferrals data
source("prep/read_deferrals.R")

# Make reports and summary
source("prep/getSISelig.R")

