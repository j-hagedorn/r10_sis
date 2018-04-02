# Complete

# Read services data
source("prep/read_svs.R")

# Read QI data
path <- "C:/Users/joshh/OneDrive - TBD Solutions LLC/files/Region10/SIS/"
qi <- read_csv(paste0(path,"QI 2017-09-29.csv"))
source("prep/readQI.R")

# Read deferrals data
source("prep/read_deferrals.R")

# Make reports and summary
# Identify individuals with a completed SIS assessment
sis_ids_fy17 <- 
  sis %>% 
  select(mcaid_id,sis_id,sis_completed_dt) %>%
  # Include assessments occurring before end date
  filter(sis_completed_dt <= as.Date("2017-09-30")) %>%
  group_by(mcaid_id) %>%
  # Include only most recent SIS assessment per ID
  summarize(sis_completed_dt = max(sis_completed_dt)) %>%
  ungroup()

elig_svs_fy17 <-
  svs %>%
  # Include only eligible services
  filter(
    CPT_CD %in% c(
    "T1016","T1017","H0045","S5151","T1005",
    "T2036","T2037","H0036","H0039"
    )
  ) %>%
  filter(
    # Include individuals with Medicaid, including spenddown
    MEDICAID %in% c("Y","S")
    # or those who are eligible for HMP
    | HMP_ELIG == TRUE
  ) %>%
  filter(FROM_DATE <= as.Date("2017-09-30")) %>%
  group_by(MEDICAID_ID,PROVIDER_NAME) %>%
  summarize(
    initial_service = min(FROM_DATE, na.rm = T),
    most_recent_service = max(FROM_DATE, na.rm = T)
  ) %>%
  mutate(eligible_svs = T) %>%
  ungroup()

combined_fy17 <-
  # Start with individuals receiving ANY services
  svs %>%
  select(MEDICAID_ID) %>% distinct() %>%
  # Join to individuals receiving eligible services
  full_join(elig_svs_fy17, by = "MEDICAID_ID") %>%
  full_join(elig_qi, by = "MEDICAID_ID") %>%
  left_join(sis_defer, by = "MEDICAID_ID") %>%
  droplevels() %>%
  # Replace missing logical values with FALSE
  replace_na(replace = list(eligible_svs = F,eligible_qi = F,deferral = F)) %>%
  # Join in SIS data.  If sis-id field is NA, then incomplete
  full_join(sis_ids, by = c("MEDICAID_ID" = "mcaid_id")) %>%
  # Include Medicaid IDs eligible from either file, or who received SIS
  filter(eligible_svs == T | eligible_qi == T | is.na(sis_completed_dt) == F) %>%
  # Remove null IDs
  filter(is.na(MEDICAID_ID) == F) %>%
  mutate(
    sis_eligible = eligible_svs == T & eligible_qi == T,
    sis_complete = is.na(sis_completed_dt) == F,
    sis_overdue  = (sis_completed_dt + (365 * 3)) < today(),
    sis_coming30 = 
      (sis_completed_dt + (365 * 3)) < (today() + 30)
    & (sis_completed_dt + (365 * 3)) >= today() 
  ) %>%
  # Include individuals who are eligible OR who have received a SIS
  filter(sis_eligible == T | sis_complete == T) %>%
  distinct()

# Filter to only include those who need SIS
need_sis_fy17 <-
  combined_fy17 %>%
  filter(
    sis_eligible == T
    & deferral == F
  ) %>%
  filter(
    sis_complete == F 
    | sis_overdue == T
    | sis_coming30 == T
  ) %>%
  mutate(
    status = case_when(
      sis_complete == F ~ "Initial SIS Needed",
      sis_overdue  == T ~ "Reassessment Overdue",
      sis_coming30 == T ~ "Reassessment Due in 30 Days"
    )
  ) %>%
  filter(status %in% c("Initial SIS Needed","Reassessment Overdue")) %>%
  select(MEDICAID_ID:most_recent_service,status) %>%
  arrange(desc(most_recent_service))

# Summarize completion rate

summary_fy17 <-
  combined_fy17 %>%
  filter(
    sis_eligible == T
    & deferral == F
  ) %>%
  group_by(PROVIDER_NAME) %>%
  summarize(
    denominator = sum(sis_eligible),
    completed = sum(sis_complete),
    overdue = sum(sis_overdue, na.rm = T)
  ) %>%
  # Add subtotal line for PIHP
  rbind(
    ., 
    data.frame(
      PROVIDER_NAME = "Region 10", 
      t(colSums(.[2:4]))
    )
  ) %>%
  mutate(
    numerator = completed - overdue,
    percent_complete = round(numerator / denominator * 100, digits = 1)
  ) 

# Create output
write.csv(summary_fy17, file = paste0("output/percent_complete_summary_fy17.csv"))
write.csv(need_sis_fy17, file = paste0("output/need_sis_r10_fy17.csv"))
