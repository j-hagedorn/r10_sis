# run_sis_complete.R

# Read services data
source("prep/read_svs.R")

# Read QI data
path <- "C:/Users/JoshH.TBDSAD/OneDrive - TBD Solutions LLC/files/Region10/SIS/"
qi <- read_csv(paste0(path,"FY19 QI - 2019.03.31.csv"))
source("prep/readQI.R")

# Read BH-TEDS data
source("prep/read_bhteds.R")

# Read deferrals data
source("prep/read_deferrals.R")

# Make reports and summary
source("prep/getSISelig.R")

