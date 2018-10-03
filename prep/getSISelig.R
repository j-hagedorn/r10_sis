# Determination for IDD Consumers who require a SIS Assessment

# 1. Pick a query timeframe based on the most current information for each person.

# 2. Extract all IDD Consumers for the time period.  This uses the data from 1 of 
#    the following methods to obtain all IDD consumers.
#    a.  Use the IDD field in the QI file to see if it is a 1 (yes) if so the 
#        consumer is selected.
#    b.  If the IDD field is blank or null we then look at the three DD Proxy 
#        Measures. The three DD Proxy measures used are:
#        Support with Mobility, Support with Personal Care, and Status of Family/Friend Support.
#        If any of these proxy measures are populated, the consumer is selected.
#   c. Parameters are: 
#       18 years or older, 
#       who have an intellectual or developmental disability, 
#       have Medicaid/Healthy MI Medicaid, and 
#       are currently receiving case management or supports coordination or respite only services.
# 3. Next, check Encounters to get the denominator.
#     a. Case Management: T1017; 
#        Supports Coordination: T1016; 
#        Respite: H0045,S5151, T1005, T2036 or T2037; 
#        Other services H0036 (home based) or H0039 (assertive community treatment).

library(tidyverse); library(lubridate)

# Identify individuals with a completed SIS assessment
sis_ids <- 
  sis %>% 
  select(mcaid_id,sis_id,sis_completed_dt) %>%
  group_by(mcaid_id) %>%
  # Include only most recent SIS assessment per ID
  summarize(sis_completed_dt = max(sis_completed_dt)) %>%
  ungroup()

combined <-
  # Start with individuals receiving ANY services
  svs %>%
  select(MEDICAID_ID) %>% distinct() %>%
  # Join to individuals receiving eligible services
  full_join(elig_svs, by = "MEDICAID_ID") %>%
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
    # SIS assessments completed for eligible individuals
    sis_complete_elig = is.na(sis_completed_dt) == F & sis_eligible == T,
    # SIS assessments completed for ineligible individuals
    sis_complete_inel = is.na(sis_completed_dt) == F & sis_eligible == F,
    sis_overdue  = (sis_completed_dt + (365 * 3)) < today(),
    sis_coming90 = 
      (sis_completed_dt + (365 * 3)) < (today() + 90)
      & (sis_completed_dt + (365 * 3)) >= today() 
  ) %>%
  # Include individuals who are eligible OR who have received a SIS
  filter(sis_eligible == T | sis_complete_inel == T)  %>%
  left_join(open_date, by = c("MEDICAID_ID" = "medicaid_id")) %>%
  mutate(agency_admission_date = as.Date(agency_admission_date)) %>%
  distinct() 
  
# Filter to only include those who need SIS
need_sis <-
  combined %>%
  filter(is.na(PROVIDER_NAME) == F) %>%
  select(MEDICAID_ID,PROVIDER_NAME,agency_admission_date,most_recent_service,sis_completed_dt:sis_coming90,deferral) %>%
  mutate(sis_complete_any = sis_complete_elig == T | sis_complete_inel == T) %>%
  gather(field,val,sis_eligible:sis_complete_any) %>%
  mutate(
    status = case_when(
      field == "sis_complete_any" & val == F  ~ "Initial SIS Needed",
      field == "sis_complete_inel" & val == T  ~ "SIS completed but ineligible",
      field == "sis_overdue"       & val == T  ~ "Reassessment Overdue",
      field == "sis_coming90"      & val == T  ~ "Reassessment Due in 90 Days",
      field == "deferral"          & val == T  ~ "Reassessment Due in 90 Days"
    )
  ) %>%
  filter(is.na(status) == F) %>%
  group_by(MEDICAID_ID) %>%
  arrange(desc(most_recent_service)) 

# Summarize completion rate

summary <-
  combined %>%
  filter(is.na(PROVIDER_NAME) == F)  %>%
  group_by(PROVIDER_NAME) %>%
  summarize(
    eligible       = sum(sis_eligible),
    deferred       = sum(deferral),
    completed_elig = sum(sis_complete_elig),
    completed_inel = sum(sis_complete_inel),
    overdue        = sum(sis_overdue, na.rm = T)
  ) %>%
  # Add subtotal line for PIHP
  rbind(
    ., 
    data.frame(
      PROVIDER_NAME = "Region 10", 
      t(colSums(.[2:6]))
    )
  ) %>%
  mutate(
    denominator      = eligible + completed_inel - deferred,
    numerator        = completed_elig + completed_inel - overdue,
    percent_complete = round(numerator / denominator * 100, digits = 1)
  ) %>%
  select(
    CMHSP = PROVIDER_NAME,
    `Individuals eligible or received SIS (Denominator)` = denominator,
    `Individuals with a current SIS (Numerator)` = numerator,
    `Individuals eligible to receive SIS` = eligible,
    `Eligible individuals with a completed SIS` = completed_elig, 
    `Ineligible individuals with a completed SIS` = completed_inel,
    `Individuals with an expired SIS` = overdue,
    `Individuals who refused to take SIS` = deferred,
    `Individuals with a current SIS / Individuals eligible for SIS` = percent_complete 
  )

# Create output
write_csv(summary, path = paste0("output/percent_complete_summary_",Sys.Date(),".csv"))

# Render summary report for high-level review
rmarkdown::render(
  input = "prep/sis_complete_summary.Rmd",
  output_file = paste0("sis_complete_summary",Sys.Date(),".pdf"),
  output_dir = "output",
  params = list(summary_tbl = summary, report_date = Sys.Date())
)

write_csv(need_sis, path = paste0("output/need_sis_r10_",Sys.Date(),".csv"))

# Create a list of dataframes, one for each level of the 'agency' variable
# Then, output these to a sub-folder of the output as .csv files

listDf <- 
  need_sis %>% 
  group_by(PROVIDER_NAME) %>% 
  dplyr::arrange(desc(most_recent_service)) %>%
  do(vals = data.frame(.)) %>% 
  dplyr::select(vals) %>% 
  lapply(function(x) {(x)})

for (i in listDf$vals) {
  write_csv(i, 
            path = paste0("output/per_cmh/need_sis_",
                          unique(i$PROVIDER_NAME),"_",
                          Sys.Date(),".csv")
  )
}
  


