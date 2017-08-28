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

elig_qi <-
  qi %>%
  filter(DISABILITYDD != "2") %>%
  filter(
    # Include if IDD field is a 1 (yes) 
    DISABILITYDD == "1" 
    # or one of the 3 proxy measures noted is populated
    | is.na(MOBILITY) == F
    | is.na(PERSONAL) == F
    | is.na(SUPPORT_SYSTEM) == F
  ) %>%
  mutate(
    # Compute current age
    age = interval(DATE_OF_BIRTH, Sys.Date())/duration(num = 1, units = "years")
  ) %>%
  # Include only individuals 18 and over
  filter(age >= 18)


elig_svs <- c("T1016","T1017","H0045","S5151","T1005","T2036","T2037","H0036","H0039")

completed <-
  svs %>%
  # Include only Medicaid IDs identified from the QI file
  filter(MEDICAID_ID %in% elig_qi$MEDICAID_ID) %>%
  # Include only eligible services
  filter(CPT_CD %in% elig_svs) %>%
  filter(
    # Include individuals with Medicaid, including spenddown
    MEDICAID %in% c("Y","S")
    # or those who are eligible for HMP
    | HMP_ELIG == TRUE
  ) %>%
  group_by(MEDICAID_ID,PROVIDER_NAME) %>%
  summarize(
    initial_service = min(FROM_DATE),
    most_recent_service = max(FROM_DATE)
  ) %>%
  ungroup() %>%
  # Join in SIS data.  If sis-id field is NA, then incomplete
  left_join(sis_ids, by = c("MEDICAID_ID" = "mcaid_id"))  
  

#Filter to only include those who need SIS
need_sis <-
  completed %>%
  filter(is.na(sis_id) == T) %>%
  arrange(desc(most_recent_service))

# Summarize completion rate

summary <-
  completed %>%
  group_by(PROVIDER_NAME) %>%
  summarize(
    denominator = n_distinct(MEDICAID_ID),
    numerator = n_distinct(sis_id)
  ) %>%
  mutate(
    percent_complete = round(numerator / denominator * 100, digits = 1)
  ) 

# How many people who have received a SIS are no longer in the list of eligibles?

  # People who have received a SIS
  unique(sis_ids$mcaid_id) %>%
  # who are not on the list of eligible
  setdiff(unique(completed$MEDICAID_ID)) %>% 
  n_distinct()
  
# Create output
write.csv(summary, file = paste0("output/percent_complete_summary_",Sys.Date(),".csv"))
write.csv(need_sis, file = paste0("output/need_sis_r10_",Sys.Date(),".csv"))

# Create a list of dataframes, one for each level of the 'agency' variable
# Then, output these to a sub-folder of the output as .csv files

listDf <- 
  need_sis %>% 
  group_by(PROVIDER_NAME) %>% 
  dplyr::arrange(desc(most_recent_service)) %>%
  do(vals=data.frame(.)) %>% 
  dplyr::select(vals) %>% 
  lapply(function(x) {(x)})

for (i in listDf$vals){
  write.csv(i, 
            file = paste0("output/per_cmh/need_sis_",
                          unique(i$PROVIDER_NAME),"_",
                          Sys.Date(),".csv")
  )
}
  


