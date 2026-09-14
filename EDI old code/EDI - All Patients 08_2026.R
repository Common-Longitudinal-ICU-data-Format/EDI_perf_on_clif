
# *************** EDI in ALL PATIENTS - CLIF *******************************
# ***************************************************************************

library(pacman)
p_load(dplyr, tidyverse, haven, lubridate, rio, ggplot2, tidyr, caret, pROC, Metrics, ggtern, survival, timeROC, forecast, mgcv, ggridges, ggplot2, broom, binom, boot, tibble, arrow, ggVennDiagram, patchwork)


# ****** COHORT INCLUSION CRITERIA *******
  # Age >=18 at time of admission (already accounted for in CLIF)
  # Initial Encounter Location in ED, Observation, or IP 
  # >=1 IP or observation status during hospitalization
  # >=1 EDI score recorded between arrival and discharge
  # For patients whose first location is the ED, the 2nd location must not be ICU or Surgery

# Time 0 = 1st EDI score filed 


# ****** VARIABLES *******

# *** DEMO
  # AGE_AT_ADMISSION (clif_hospitalization.age_at_admission)
  # SEX (clif_patient.sex_category)
  # RACE (clif_patient.race_category)
  # ETHNICIY (clif_patient.ethnicity_category)


# ** OUTCOMES
  # HOSPITAL_LOS (clif_hospitalization.discharge_dttm - clif_hospitalization.admission_dttm)
  # ICU_LOS (clif_adt.location_category = ICU -> max(out_dttm) - min(in_dttm))

  # ICU_TIME - (clif_adt.location_category = ICU -> min(in_dttm))

  # VENT_TIME (clif_respiratory_support.device_category = IMV -> min(recorded_dttm)

  # VASOPRESSOR_TIME (clif_medication_admin_continuous.med_group = vasoactives -> min(admin_dttm))
  
  # DEATH_TIME - if clif_patient.death_dttm between clif_hospitalization.discharge_dttm and clif_hospitalization.admit_dttm -> DEATH_DTTM ; if clif_hospitalization.discharge_category = 'Expired' or 'Hospice' -> DISCHARGE_DTTM


# ** EDI
  # EDI_SCORE
  # SCORE_TIME




# ********************************************************************************************************************************************************************************************************************************
# 1.) IMPORT/CLEAN DATA

# ** HOSPITALIZATION INFO ** 
hosp <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Emory CLIF Data/clif_hospitalization.parquet") %>% 
  select(patient_id, hospitalization_id, admission_dttm, discharge_dttm, age_at_admission, discharge_category) 


# ** DEMOGRAPHICS ** 
patient <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Emory CLIF Data/clif_patient.parquet") %>% 
  select(patient_id, death_dttm, sex_category, race_category, ethnicity_category)

demo <- hosp %>%
  select(patient_id, hospitalization_id, age_at_admission) %>%
  inner_join(patient %>% select(-death_dttm), by = "patient_id") %>%
  mutate(race_category_clean = case_when(race_category %in% c("American Indian or Alaska Native",
                                                              "Native Hawaiian or Other Pacific Islander",
                                                              "Other") ~ "Other/Multiracial", 
                                         TRUE ~ race_category)) %>%
  distinct()



# ** OUTCOMES **
#icu
icu <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Emory CLIF Data/clif_adt.parquet") %>%
  filter(location_category == "icu") %>%
  group_by(hospitalization_id) %>%
  summarize(
    icu_time = min(in_dttm),
    icu_discharge_time = max(out_dttm),
    icu_los = as.numeric(icu_discharge_time - icu_time, units = "days"),
    .groups = "drop")

#ventilator 
vent <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Emory CLIF Data/clif_respiratory_support.parquet") %>%
  filter(device_category == "IMV") %>%
  group_by(hospitalization_id) %>%
  summarize(vent_time = min(recorded_dttm), .groups = "drop") 

#vasopressors 
vasopressor <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Emory CLIF Data/clif_medication_admin_continuous.parquet") %>%
  filter(med_group == 'vasoactives') %>%
  group_by(hospitalization_id) %>%
  summarize(vasopressor_time = min(admin_dttm), .groups = "drop") 


#mortality = clif_patient: death_dttm is during hosp. OR clif_hospitalization: discharge dispo of 'Expired' or 'Hospice'
mortality <- hosp %>%
  select(patient_id, hospitalization_id, admission_dttm, discharge_dttm, discharge_category) %>%
  inner_join(patient %>% select(patient_id, death_dttm), by = "patient_id") %>% distinct()

mortality <- mortality %>%
  group_by(hospitalization_id) %>%
  mutate(death_during_hosp = !is.na(death_dttm) & death_dttm >= admission_dttm & death_dttm <= discharge_dttm,
         death_dispo = discharge_category %in% c("Expired", "Hospice"),
         death_time = case_when(death_during_hosp ~ death_dttm,   # prioritize actual death time if both happen
                                death_dispo ~ discharge_dttm,
                                TRUE ~ as.POSIXct(NA)))

mortality <- mortality %>% select(patient_id, hospitalization_id, death_time)

#los
hosp_los <- hosp %>%
  select(-age_at_admission, -discharge_category) %>%
  mutate(hosp_los = as.numeric(difftime(discharge_dttm, admission_dttm, units = "days")))

#surgery during encounter
surgery <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Emory CLIF Data/clif_adt.parquet") %>%
  filter(location_category == "procedural") %>%
  select(hospitalization_id, in_dttm, out_dttm) %>% distinct() %>%
  inner_join(hosp, by='hospitalization_id') %>%
  group_by(hospitalization_id) %>%
  filter(in_dttm >= admission_dttm & out_dttm <= discharge_dttm) %>%
  mutate(surgery = 1) %>% select(hospitalization_id, surgery) %>% distinct()


#COMBINE DEMO/OUTCOMES
demo_outcomes <- demo %>% left_join(icu, by='hospitalization_id') %>%
                          left_join(vent, by='hospitalization_id') %>%
                          left_join(vasopressor, by='hospitalization_id') %>%
                          left_join(mortality, by=c('hospitalization_id', 'patient_id')) %>%
                          left_join(hosp_los, by=c('hospitalization_id', 'patient_id')) %>%
                          left_join(surgery, by='hospitalization_id')

#write_parquet(demo_outcomes, "/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Dr. Siva Bhavani/EDI Prediction Model/ALL PATIENTS/Saved Data/demo_outcomes.parquet")






# ********************************************************************************************************************************************************************************************************************************
# 2.) REFINE COHORT
adt <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Emory CLIF Data/clif_adt.parquet")

# location at some point in IP
adt2 <- adt %>%
  group_by(hospitalization_id) %>%
  mutate(ever_ward = any(location_category %in% "ward")) %>%
  ungroup() %>%
  filter(ever_ward) %>%
  select(-ever_ward)

# 1st location in ED, obs, or IP 
adt3 <- adt2 %>%
  arrange(hospitalization_id, in_dttm) %>%
  group_by(hospitalization_id) %>%
  mutate(
    first_location = location_category[1],
    second_location = location_category[which(location_category != location_category[1])[1]]) %>%
  ungroup() %>%
  filter(first_location %in% c("ed", "ward")) 

# If 1st location is ED, 2nd location cannot be icu or surgery
adt4 <- adt3 %>%
  filter(first_location %in% c("ed", "ward"),
        !(first_location == "ed" & second_location %in% c("icu", "procedural")))

#Keep pat, enc, and times
adt5 <- adt4 %>%
  select(hospitalization_id) %>%
  inner_join(hosp %>% select(patient_id, hospitalization_id, admission_dttm, discharge_dttm),
    by = "hospitalization_id") %>% distinct()
#write_parquet(adt5, "/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Dr. Siva Bhavani/EDI Prediction Model/ALL PATIENTS/Saved Data/adt5.parquet")


# ***** EDI  ****
edi <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Emory CLIF Data/clif_scores.parquet") %>% filter(model_id == '30434503') #model_name == 'IP Deterioration Index'
edi2 <- edi %>% inner_join(adt5, by='hospitalization_id')

# 1+ EDI score after arrival & before discharge
edi2 <- edi2 %>%
  mutate(arrival_to_score = difftime(recorded_dttm, admission_dttm, units = "hours"),
         score_to_discharge = difftime(recorded_dttm, discharge_dttm, units = "hours"))

edi3 <- edi2 %>% filter(arrival_to_score >= 0 & score_to_discharge <= 0)

#Bring in demographics 
edi4 <- edi3 %>% select(-admission_dttm, -discharge_dttm) %>% inner_join(demo_outcomes, by=c('patient_id', 'hospitalization_id'))
#write_parquet(edi4, "/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Dr. Siva Bhavani/EDI Prediction Model/ALL PATIENTS/Saved Data/edi4.parquet")

# length(unique(adt$hospitalization_id)) #1,482,960 # hospitalizations
# length(unique(adt2$hospitalization_id)) #470,147 # 1+ IP or observation status during hospitalization
# length(unique(adt3$hospitalization_id)) #377,747 # Initial Encounter Location in ED, Observation, or IP
# length(unique(adt4$hospitalization_id)) #347,595 # If first location is the ED, the 2nd location must not be ICU or Surgery
# length(unique(edi4$hospitalization_id)) #72,224 # 1+ EDI score recorded between arrival and discharge

# Save Flowsheet findings
flowsheet_summary <- data.frame(
  Step = c(
    "All hospitalizations",
    "1+ IP or observation status during hospitalization",
    "Initial encounter location in ED, Observation, or IP",
    "If first location is ED, second location not ICU or Surgery",
    "1+ EDI score recorded between arrival and discharge"),
  Dataset = c("adt", "adt2", "adt3", "adt4", "edi4"),
  Hospitalizations = c(
    length(unique(adt$hospitalization_id)),
    length(unique(adt2$hospitalization_id)),
    length(unique(adt3$hospitalization_id)),
    length(unique(adt4$hospitalization_id)),
    length(unique(edi4$hospitalization_id))))

write.csv(flowsheet_summary, "/Users/mcschwi/Desktop/flowsheet_summary.csv", row.names = FALSE)







# ********************************************************************************************************************************************************************************************************************************
# 3.) DATA ANALYSIS

#edi4 <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Dr. Siva Bhavani/EDI Prediction Model/ALL PATIENTS/Saved Data/edi4.parquet")


# **************************** TABLE 1: DEMOGRAPHICS ****************************
# 1.) ALL ENCOUNTERS 
table1 <- edi4 %>% select(-recorded_dttm, -score_value, -arrival_to_score, -score_to_discharge) %>% ungroup() %>% distinct() #1 row per enc
# write_parquet(table1, "/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Dr. Siva Bhavani/EDI Prediction Model/ALL PATIENTS/Saved Data/table1.parquet")

# 2.) MAX THRESHOLD >=40 
edi_GE_40 <- edi4 %>% 
  group_by(hospitalization_id) %>% 
  filter(max(score_value) >= 40) %>% 
  select(-recorded_dttm, -score_value, -arrival_to_score, -score_to_discharge) %>% 
  ungroup() %>% distinct() 
#length(unique(edi_GE_40$hospitalization_id)) #27,017

# MAX THRESHOLD <40 
edi_LT_40 <- edi4 %>% 
  group_by(hospitalization_id) %>% 
  filter(max(score_value) < 40) %>% 
  select(-recorded_dttm, -score_value, -arrival_to_score, -score_to_discharge) %>% 
  ungroup() %>% distinct() 
#length(unique(edi_LT_40$hospitalization_id)) #45,207



# SUMMARIZE RESULTS
get_table1 <- function(df, cohort){
  age <- quantile(df$age_at_admission, c(.25, .5, .75), na.rm = TRUE)
  los <- quantile(df$hosp_los, c(.25, .5, .75), na.rm = TRUE)
  tibble(
    Cohort = cohort,
    N = nrow(df),
    
    Age_Q1 = age[1],
    Age_Median = age[2],
    Age_Q3 = age[3],
    
    Female_n = sum(df$sex_category == "Female", na.rm = TRUE),
    Female_pct = mean(df$sex_category == "Female", na.rm = TRUE) * 100,
    
    Hispanic_n = sum(df$ethnicity_category == "Hispanic or Latino", na.rm = TRUE),
    Hispanic_pct = mean(df$ethnicity_category == "Hispanic or Latino", na.rm = TRUE) * 100,
    
    White_n = sum(df$race_category_clean == "White", na.rm = TRUE),
    White_pct = mean(df$race_category_clean == "White", na.rm = TRUE) * 100,
    
    Black_n = sum(df$race_category_clean == "Black or African American", na.rm = TRUE),
    Black_pct = mean(df$race_category_clean == "Black or African American", na.rm = TRUE) * 100,
    
    HospLOS_Q1 = los[1],
    HospLOS_Median = los[2],
    HospLOS_Q3 = los[3],
    
    ICU_n = sum(!is.na(df$icu_time)),
    ICU_pct = mean(!is.na(df$icu_time)) * 100,
    
    Mortality_n = sum(!is.na(df$death_time)),
    Mortality_pct = mean(!is.na(df$death_time)) * 100,
    
    Vasopressor_n = sum(!is.na(df$vasopressor_time)),
    Vasopressor_pct = mean(!is.na(df$vasopressor_time)) * 100,
    
    Vent_n = sum(!is.na(df$vent_time)),
    Vent_pct = mean(!is.na(df$vent_time)) * 100,
    
    Surgery_n = sum(!is.na(df$surgery)),
    Surgery_pct = mean(!is.na(df$surgery)) * 100
  )
}

# Save all results
table1_data <- bind_rows(
  get_table1(table1, "All"),
  get_table1(edi_GE_40, "Max EDI ≥40"),
  get_table1(edi_LT_40, "Max EDI <40"))

table1_data

#Save Fig 1 Dataframe
write.csv(table1_data, "/Users/mcschwi/Desktop/table1_data.csv")


# **************************** TABLE 2: EDI PERFORMANCE EVALUATION ****************************

threshold <- 40.0

# ************************** OUTCOME 1: ICU **************************
#edi4 <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Dr. Siva Bhavani/EDI Prediction Model/ALL PATIENTS/Saved Data/edi4.parquet")
icu <- edi4 %>% filter(is.na(icu_time) | icu_time > recorded_dttm)

# 1.) TIME HORIZON 4 
edi_4 <- icu %>% select(hospitalization_id, recorded_dttm, score_value, icu_time) %>%
  mutate(Score_Time_Horizon_4 = recorded_dttm + hours(4),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(4),
         Prediction_4 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_4 = if_else(!is.na(icu_time) & icu_time >= recorded_dttm & icu_time <= Score_Time_Horizon_4, 1L, 0L)) 

perf_data <- edi_4 %>% mutate(Outcome = Truth_4, Predicted = Prediction_4, Score = score_value) %>%
  filter(!is.na(Outcome), !is.na(Predicted), Outcome %in% c(0,1)) %>% ungroup()

TP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 1)
FN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 1)
TN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 0)
FP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 0)
incidence <- mean(perf_data$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(perf_data$Outcome, perf_data$Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

icu_4 <- results %>% mutate(Outcome = "ICU", Horizon = "4h")
print(results)


# 2.) TIME HORIZON 12 
edi_12 <- icu %>% select(hospitalization_id, recorded_dttm, score_value, icu_time) 
edi_12 <- edi_12 %>%
  mutate(Score_Time_Horizon_12 = recorded_dttm + hours(12),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(12),
         Prediction_12 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_12 = if_else(!is.na(icu_time) & icu_time >= recorded_dttm & icu_time <= Score_Time_Horizon_12, 1L, 0L)) 

perf_data <- edi_12 %>% mutate(Outcome = Truth_12, Predicted = Prediction_12, Score = score_value) %>%
  filter(!is.na(Outcome), !is.na(Predicted), Outcome %in% c(0,1)) %>% ungroup()

# NOT BOOTSTRAPPING DUE TO 1 mill+ rows
TP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 1)
FN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 1)
TN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 0)
FP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 0)
incidence <- mean(perf_data$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(perf_data$Outcome, perf_data$Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

icu_12 <- results %>% mutate(Outcome = "ICU", Horizon = "12h")
print(results)



# 3.) ENCOUNTER LEVEL (Before Event)
hospital_level <- icu %>%
  group_by(hospitalization_id) %>%
  summarise(
    Outcome = as.integer(any(!is.na(icu_time))),
    Max_Score = max(score_value, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Predicted = Max_Score >= threshold)

TP <- sum(hospital_level$Predicted == 1 & hospital_level$Outcome == 1)
FN <- sum(hospital_level$Predicted == 0 & hospital_level$Outcome == 1)
TN <- sum(hospital_level$Predicted == 0 & hospital_level$Outcome == 0)
FP <- sum(hospital_level$Predicted == 1 & hospital_level$Outcome == 0)
incidence <- mean(hospital_level$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(hospital_level$Outcome, hospital_level$Max_Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

icu_hosp <- results %>% mutate(Outcome = "ICU", Horizon = "Hospital-Level")
print(results)


# ************************** OUTCOME 2: VASOPRESSORS **************************
vaso <- edi4 %>% filter(is.na(vasopressor_time) | vasopressor_time > recorded_dttm)

# 1.) TIME HORIZON 4 
edi_4 <- vaso %>% select(hospitalization_id, recorded_dttm, score_value, vasopressor_time) 
edi_4 <- edi_4 %>%
  mutate(Score_Time_Horizon_4 = recorded_dttm + hours(4),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(4),
         Prediction_4 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_4 = if_else(!is.na(vasopressor_time) & vasopressor_time >= recorded_dttm & vasopressor_time <= Score_Time_Horizon_4, 1L, 0L)) 

perf_data <- edi_4 %>% mutate(Outcome = Truth_4, Predicted = Prediction_4, Score = score_value) %>%
  filter(!is.na(Outcome), !is.na(Predicted), Outcome %in% c(0,1)) %>% ungroup()

TP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 1)
FN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 1)
TN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 0)
FP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 0)
incidence <- mean(perf_data$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(perf_data$Outcome, perf_data$Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

vaso_4 <- results %>% mutate(Outcome = "Vasopressors", Horizon = "4h")
print(results)



# 2.) TIME HORIZON 12 
edi_12 <- vaso %>% select(hospitalization_id, recorded_dttm, score_value, vasopressor_time)
edi_12 <- edi_12 %>%
  mutate(Score_Time_Horizon_12 = recorded_dttm + hours(12),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(12),
         Prediction_12 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_12 = if_else(!is.na(vasopressor_time) & vasopressor_time >= recorded_dttm & vasopressor_time <= Score_Time_Horizon_12, 1L, 0L)) 

perf_data <- edi_12 %>% mutate(Outcome = Truth_12, Predicted = Prediction_12, Score = score_value) %>%
  filter(!is.na(Outcome), !is.na(Predicted), Outcome %in% c(0,1)) %>% ungroup()

TP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 1)
FN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 1)
TN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 0)
FP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 0)
incidence <- mean(perf_data$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(perf_data$Outcome, perf_data$Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

vaso_12 <- results %>% mutate(Outcome = "Vasopressors", Horizon = "12h")
print(results)


# 3.) ENCOUNTER LEVEL (Before Event)
hospital_level <- vaso %>%
  group_by(hospitalization_id) %>%
  summarise(
    Outcome = as.integer(any(!is.na(vasopressor_time))),
    Max_Score = max(score_value, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Predicted = Max_Score >= threshold)

TP <- sum(hospital_level$Predicted == 1 & hospital_level$Outcome == 1)
FN <- sum(hospital_level$Predicted == 0 & hospital_level$Outcome == 1)
TN <- sum(hospital_level$Predicted == 0 & hospital_level$Outcome == 0)
FP <- sum(hospital_level$Predicted == 1 & hospital_level$Outcome == 0)
incidence <- mean(hospital_level$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(hospital_level$Outcome, hospital_level$Max_Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

vaso_hosp <- results %>% mutate(Outcome = "Vasopressors", Horizon = "Hospital-Level")
print(results)





# ************************** OUTCOME 3: VENTILATOR **************************
vent <- edi4 %>% filter(is.na(vent_time) | vent_time > recorded_dttm)

# 1.) TIME HORIZON 4 
edi_4 <- vent %>% select(hospitalization_id, recorded_dttm, score_value, vent_time) 

edi_4 <- edi_4 %>%
  mutate(Score_Time_Horizon_4 = recorded_dttm + hours(4),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(4),
         Prediction_4 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_4 = if_else(!is.na(vent_time) & vent_time >= recorded_dttm & vent_time <= Score_Time_Horizon_4, 1L, 0L)) 

perf_data <- edi_4 %>% mutate(Outcome = Truth_4, Predicted = Prediction_4, Score = score_value) %>%
  filter(!is.na(Outcome), !is.na(Predicted), Outcome %in% c(0,1)) %>% ungroup()

TP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 1)
FN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 1)
TN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 0)
FP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 0)
incidence <- mean(perf_data$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(perf_data$Outcome, perf_data$Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

vent_4 <- results %>% mutate(Outcome = "Ventilator", Horizon = "4h")
print(results)




# 2.) TIME HORIZON 12 
edi_12 <- vent %>% select(hospitalization_id, recorded_dttm, score_value, vent_time) 

edi_12 <- edi_12 %>%
  mutate(Score_Time_Horizon_12 = recorded_dttm + hours(12),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(12),
         Prediction_12 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_12 = if_else(!is.na(vent_time) & vent_time >= recorded_dttm & vent_time <= Score_Time_Horizon_12, 1L, 0L)) 

perf_data <- edi_12 %>% mutate(Outcome = Truth_12, Predicted = Prediction_12, Score = score_value) %>%
  filter(!is.na(Outcome), !is.na(Predicted), Outcome %in% c(0,1)) %>% ungroup()

TP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 1)
FN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 1)
TN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 0)
FP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 0)
incidence <- mean(perf_data$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(perf_data$Outcome, perf_data$Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

vent_12 <- results %>% mutate(Outcome = "Ventilator", Horizon = "12h")
print(results)



# 3.) ENCOUNTER LEVEL (Before Event)
hospital_level <- vent %>%
  group_by(hospitalization_id) %>%
  summarise(
    Outcome = as.integer(any(!is.na(vent_time))),
    Max_Score = max(score_value, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Predicted = Max_Score >= threshold)

TP <- sum(hospital_level$Predicted == 1 & hospital_level$Outcome == 1)
FN <- sum(hospital_level$Predicted == 0 & hospital_level$Outcome == 1)
TN <- sum(hospital_level$Predicted == 0 & hospital_level$Outcome == 0)
FP <- sum(hospital_level$Predicted == 1 & hospital_level$Outcome == 0)
incidence <- mean(hospital_level$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(hospital_level$Outcome, hospital_level$Max_Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

vent_hosp <- results %>% mutate(Outcome = "Ventilator", Horizon = "Hospital-Level")
print(results)





# ************************** OUTCOME 4: MORTALITY **************************
mortality <- edi4 %>% filter(is.na(death_time) | death_time > recorded_dttm)

# 1.) TIME HORIZON 4 
edi_4 <- mortality %>% select(hospitalization_id, recorded_dttm, score_value, death_time) 
edi_4 <- edi_4 %>%
  mutate(Score_Time_Horizon_4 = recorded_dttm + hours(4),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(4),
         Prediction_4 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_4 = if_else(!is.na(death_time) & death_time >= recorded_dttm & death_time <= Score_Time_Horizon_4, 1L, 0L)) 

perf_data <- edi_4 %>% mutate(Outcome = Truth_4, Predicted = Prediction_4, Score = score_value) %>%
  filter(!is.na(Outcome), !is.na(Predicted), Outcome %in% c(0,1)) %>% ungroup()

TP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 1)
FN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 1)
TN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 0)
FP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 0)
incidence <- mean(perf_data$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(perf_data$Outcome, perf_data$Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

mortality_4 <- results %>% mutate(Outcome = "In-hospital Mortality", Horizon = "4h")
print(results)


# 2.) TIME HORIZON 12 
edi_12 <- mortality %>% select(hospitalization_id, recorded_dttm, score_value, death_time) 
edi_12 <- edi_12 %>%
  mutate(Score_Time_Horizon_12 = recorded_dttm + hours(12),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(12),
         Prediction_12 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_12 = if_else(!is.na(death_time) & death_time >= recorded_dttm & death_time <= Score_Time_Horizon_12, 1L, 0L)) 

perf_data <- edi_12 %>% mutate(Outcome = Truth_12, Predicted = Prediction_12, Score = score_value) %>%
  filter(!is.na(Outcome), !is.na(Predicted), Outcome %in% c(0,1)) %>% ungroup()

TP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 1)
FN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 1)
TN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 0)
FP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 0)
incidence <- mean(perf_data$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(perf_data$Outcome, perf_data$Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

mortality_12 <- results %>% mutate(Outcome = "In-hospital Mortality", Horizon = "12h")
print(results)



# 3.) ENCOUNTER LEVEL (Before Event)
hospital_level <- mortality %>%
  group_by(hospitalization_id) %>%
  summarise(
    Outcome = as.integer(any(!is.na(death_time))),
    Max_Score = max(score_value, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Predicted = Max_Score >= threshold)

TP <- sum(hospital_level$Predicted == 1 & hospital_level$Outcome == 1)
FN <- sum(hospital_level$Predicted == 0 & hospital_level$Outcome == 1)
TN <- sum(hospital_level$Predicted == 0 & hospital_level$Outcome == 0)
FP <- sum(hospital_level$Predicted == 1 & hospital_level$Outcome == 0)
incidence <- mean(hospital_level$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(hospital_level$Outcome, hospital_level$Max_Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

mortality_hosp <- results %>% mutate(Outcome = "In-hospital Mortality", Horizon = "Hospital-Level")
print(results)




# ************************** OUTCOME 5: ANY OF THE 4 **************************

# ADVERSE_EVENT_TIME = Earliest time among the events, NA if none occurred
edi4 <- edi4 %>%
  group_by(hospitalization_id) %>%
  mutate(adverse_outcome = if_else(!is.na(icu_time) | !is.na(vent_time) | !is.na(vasopressor_time) | !is.na(death_time), 1, 0),
         adverse_outcome_time = if_else(adverse_outcome == 1, pmin(icu_time, vent_time, vasopressor_time, death_time, na.rm = TRUE), as.POSIXct(NA)))

adverse <- edi4 %>% filter(is.na(adverse_outcome_time) | adverse_outcome_time > recorded_dttm)



# 1.) TIME HORIZON 4 
edi_4 <- adverse %>% select(hospitalization_id, recorded_dttm, score_value, adverse_outcome_time) 

edi_4 <- edi_4 %>%
  mutate(Score_Time_Horizon_4 = recorded_dttm + hours(4),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(4),
         Prediction_4 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_4 = if_else(!is.na(adverse_outcome_time) & adverse_outcome_time >= recorded_dttm & adverse_outcome_time <= Score_Time_Horizon_4, 1L, 0L)) 

perf_data <- edi_4 %>% mutate(Outcome = Truth_4, Predicted = Prediction_4, Score = score_value) %>%
  filter(!is.na(Outcome), !is.na(Predicted), Outcome %in% c(0,1)) %>% ungroup()

TP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 1)
FN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 1)
TN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 0)
FP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 0)
incidence <- mean(perf_data$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(perf_data$Outcome, perf_data$Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

adverse_4 <- results %>% mutate(Outcome = "Adverse Outcome", Horizon = "4h")
print(results)



# 2.) TIME HORIZON 12 
edi_12 <- adverse %>% select(hospitalization_id, recorded_dttm, score_value, adverse_outcome_time) 
edi_12 <- edi_12 %>%
  mutate(Score_Time_Horizon_12 = recorded_dttm + hours(12),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(12),
         Prediction_12 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_12 = if_else(!is.na(adverse_outcome_time) & adverse_outcome_time >= recorded_dttm & adverse_outcome_time <= Score_Time_Horizon_12, 1L, 0L)) 

perf_data <- edi_12 %>% mutate(Outcome = Truth_12, Predicted = Prediction_12, Score = score_value) %>%
  filter(!is.na(Outcome), !is.na(Predicted), Outcome %in% c(0,1)) %>% ungroup()

TP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 1)
FN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 1)
TN <- sum(perf_data$Predicted == 0 & perf_data$Outcome == 0)
FP <- sum(perf_data$Predicted == 1 & perf_data$Outcome == 0)
incidence <- mean(perf_data$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(perf_data$Outcome, perf_data$Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

adverse_12 <- results %>% mutate(Outcome = "Adverse Outcome", Horizon = "12h")
print(results)



# 3.) ENCOUNTER LEVEL (Before Event)
hospital_level <- adverse %>%
  group_by(hospitalization_id) %>%
  summarise(
    Outcome = as.integer(any(!is.na(adverse_outcome_time))),
    Max_Score = max(score_value, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Predicted = Max_Score >= threshold)

TP <- sum(hospital_level$Predicted == 1 & hospital_level$Outcome == 1)
FN <- sum(hospital_level$Predicted == 0 & hospital_level$Outcome == 1)
TN <- sum(hospital_level$Predicted == 0 & hospital_level$Outcome == 0)
FP <- sum(hospital_level$Predicted == 1 & hospital_level$Outcome == 0)
incidence <- mean(hospital_level$Outcome)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
ppv <- TP / (TP + FP)
npv <- TN / (TN + FN)
nne <- 1 / ppv
sens_ci <- prop.test(TP, TP + FN)$conf.int
spec_ci <- prop.test(TN, TN + FP)$conf.int
ppv_ci  <- prop.test(TP, TP + FP)$conf.int
npv_ci  <- prop.test(TN, TN + FN)$conf.int
roc_obj <- pROC::roc(hospital_level$Outcome, hospital_level$Max_Score, quiet = TRUE)
auc_val <- as.numeric(roc_obj$auc)
auc_ci <- pROC::ci.auc(roc_obj, method = "delong")

results <- tibble(
  Metric = c("Incidence", "AUC", "Sensitivity", "Specificity", "PPV", "NPV", "NNE"),
  Estimate = c(incidence, auc_val, sensitivity, specificity, ppv, npv, nne),
  CI_Lower = c(NA, auc_ci[1], sens_ci[1], spec_ci[1], ppv_ci[1], npv_ci[1], NA),
  CI_Upper = c(NA, auc_ci[3], sens_ci[2], spec_ci[2], ppv_ci[2], npv_ci[2], NA))

adverse_hosp <- results %>% mutate(Outcome = "Adverse Outcome", Horizon = "Hospital-Level")
print(results)




# ************************** FIGURE 1: FOREST PLOT FOR ALL METRICS  **************************
fig_1_data <- bind_rows(
  icu_hosp, icu_12, icu_24,
  vaso_hosp, vaso_12, vaso_24,
  vent_hosp, vent_12, vent_24,
  mortality_hosp, mortality_12, mortality_24,
  adverse_hosp, adverse_12, adverse_24)
# write_parquet(fig_1_data, "/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Dr. Siva Bhavani/EDI Prediction Model/ALL PATIENTS/Saved Data/fig_1_data.parquet")

fig_1_df <- fig_1_data %>%
  filter(Metric %in% c("AUC", "Sensitivity", "Specificity", "PPV")) %>%
  mutate(
    Horizon = factor(Horizon, levels = c("4h", "12h", "Hospital-Level")),
    Outcome = factor(Outcome, levels = c("Adverse Outcome", "ICU", "Vasopressors",
                                        "Ventilator", "In-hospital Mortality")),
    site = "emory")

fig_1 <- ggplot(fig_1_df, aes(x = Estimate, y = Outcome)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = CI_Lower,
                     xmax = CI_Upper),
                 height = 0.2) +
  facet_grid(Metric ~ Horizon) +
  geom_vline(data = subset(fig_1_df, Metric == "AUC"),
             xintercept = 0.5,
             linetype = "dashed",
             color = "gray50") +
  coord_cartesian(xlim = c(0,1)) +
  scale_y_discrete(limits = rev(levels(fig_1_df$Outcome))) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, family = "Times New Roman"),
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 14, family = "Times New Roman"),
    axis.title.y = element_text(size = 14, family = "Times New Roman"),
    strip.text = element_text(face = "bold", size = 14, family = "Times New Roman"),
    strip.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()) +
  labs(
    title = "Performance of the EDI for Adverse Clinical Outcomes - Emory",
    x = "Performance (Estimate with 95% CI)",
    y = NULL)

#Save Figure 1
ggsave(filename = "/Users/mcschwi/Desktop/fig_1.png",
  plot = fig_1,
  width = 12,
  height = 6,
  dpi = 300)

#Save Fig 1 Dataframe
write.csv(fig_1_df, "/Users/mcschwi/Desktop/fig_1_df.csv")






# ************************** FIGURE 2: CALIBRATION PLOT FOR ADVERSE OUTCOME (ENCOUNTER-LEVEL)  **************************

adverse_cal <- adverse %>%
  group_by(hospitalization_id) %>%
  summarise(
    Outcome = as.integer(any(!is.na(adverse_outcome_time))),
    Max_Score = max(score_value, na.rm = TRUE),
    .groups = "drop")

# Fit logistic model
cal_model <- glm(
  Outcome ~ Max_Score,
  data = adverse_cal,
  family = binomial)

# Predicted probabilities
fig_2_df <- adverse_cal %>%
  mutate(pred_prob = predict(
        cal_model,
        newdata = adverse_cal,
        type = "response"),
      site = "emory")

#plot - stops at .25 - still dont quite understand
fig_2 <- fig_2_df %>%
  mutate(bin = ntile(pred_prob, 10)) %>%
  group_by(bin) %>%
  summarise(
    mean_pred = mean(pred_prob),
    obs_rate = mean(Outcome)) %>%
  ggplot(aes(mean_pred, obs_rate)) +
  geom_point(size = 3) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(
    plot.title = element_text(family = "Times New Roman", face = "bold", size = 18,hjust = 0.5),
    axis.text.x = element_text(family = "Times New Roman", size = 14),
    axis.text.y = element_text(family = "Times New Roman", size = 14),
    axis.title.x = element_text(family = "Times New Roman", size = 14),
    axis.title.y = element_text(family = "Times New Roman", size = 14)) +
  labs(x = "Predicted Risk (EDI)",
      y = "Observed Rutcome Rate",
      title = "Calibration of EDI for Adverse Outcome")

#Save Figure 2
ggsave(filename = "/Users/mcschwi/Desktop/fig_2.png",
       plot = fig_2,
       width = 12,
       height = 6,
       dpi = 300)

#Save Fig 2 Dataframe
write.csv(fig_2_df, "/Users/mcschwi/Desktop/fig_2_df.csv")





# ************************** FIGURE 3: UPSET PLOT OF ADVERSE EVENT  **************************
p_load(UpSetR)

fig_3_data <- edi4 %>%
  group_by(hospitalization_id) %>%
  summarise(
    site = "emory",
    icu = any(!is.na(icu_time)),
    vaso = any(!is.na(vasopressor_time)),
    vent = any(!is.na(vent_time)),
    mortality = any(!is.na(death_time)),
    .groups = "drop")

upset_data <- fig_3_data %>%
  transmute(
    ICU = as.integer(icu),
    Vasopressors = as.integer(vaso),
    Ventilator = as.integer(vent),
    Mortality = as.integer(mortality)) %>%
  as.data.frame()

UpSetR::upset(upset_data,
              sets = c("ICU", "Vasopressors", "Ventilator", "Mortality"),
              order.by = "freq",
              mainbar.y.label = "Number of Encounters",
              sets.x.label = "Total Encounters per Outcome",
              text.scale = 1.5)

grid::grid.text(
  "Intersections of ICU, Ventilator, Vasopressor, and In-hospital Mortality Outcomes",
  x = 0.5, y = 0.98,
  gp = grid::gpar(fontface = "bold", fontsize = 16, fontfamily = "Times New Roman"))

# Save plots
png("/Users/mcschwi/Desktop/fig_3.png", width = 12, height = 7, units = "in", res = 300)
UpSetR::upset(upset_data,
              sets = c("ICU", "Vasopressors", "Ventilator", "Mortality"),
              order.by = "freq",
              mainbar.y.label = "Number of Encounters",
              sets.x.label = "Number of Encounters per Outcome",
              text.scale = 1.5)
grid::grid.text("Intersections of ICU, Ventilator, Vasopressor, and In-hospital Mortality Outcomes",
  x = 0.5, y = 0.98,
  gp = grid::gpar(fontface = "bold", fontsize = 16))
dev.off()

#Save Data
fig_3_df <- fig_3_data %>%
  count(icu, vaso, vent, mortality, name = "N") %>%
  arrange(desc(N)) %>%
  rowwise() %>%
  mutate(Combination = paste(
    c("ICU", "Vasopressors", "Ventilator", "Mortality")[c(icu, vaso, vent, mortality)],
    collapse = " + ")) %>%
  ungroup() %>%
  mutate(Combination = if_else(Combination == "", "None", Combination)) %>%
  select(Combination, N)
write.csv(fig_3_df, "/Users/mcschwi/Desktop/fig_3_df.csv")




# ************************** FIGURE 4: MEAN SCORE UNTIL EVENT - no event = random 24 hours  **************************

outcome_colors <- c(
  "Adverse Outcome"        = "#F8766D",  # matches default ggplot order for these 5 levels
  "ICU"                    = "#A3A500",
  "Vasopressors"           = "#00BF7D",
  "Ventilator"             = "#00B0F6",
  "In-hospital Mortality"  = "#E76BF3",
  "No Event"               = "gray50")

window_hours <- 24
bin_width    <- 1

build_trajectory <- function(df, anchor_col, group_label) {
  df %>%
    filter(!is.na(.data[[anchor_col]])) %>%
    mutate(hours_to_anchor = as.numeric(difftime(.data[[anchor_col]], recorded_dttm, units = "hours"))) %>%
    filter(hours_to_anchor >= 0, hours_to_anchor < window_hours + bin_width) %>%
    mutate(hours_bin = -floor(hours_to_anchor / bin_width) * bin_width,
           Group = group_label)
}

# EVENT GROUPS
traj_icu   <- build_trajectory(edi4, "icu_time",         "ICU")
traj_vent  <- build_trajectory(edi4, "vent_time",         "Ventilator")
traj_vaso  <- build_trajectory(edi4, "vasopressor_time",  "Vasopressors")
traj_death <- build_trajectory(edi4, "death_time",        "In-hospital Mortality")

# COMPOSITE ("ANY EVENT") GROUP — use adverse_outcome_time already built in edi4
traj_composite <- build_trajectory(edi4, "adverse_outcome_time", "Adverse Outcome")

# NO-EVENT CONTROL GROUP — random 24h window
no_event_df <- edi4 %>% filter(is.na(icu_time), is.na(vent_time), is.na(vasopressor_time), is.na(death_time))

set.seed(123)

no_event_anchors <- no_event_df %>%
  group_by(hospitalization_id) %>%
  summarise(min_time = min(recorded_dttm, na.rm = TRUE),
            max_time = max(recorded_dttm, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(earliest_valid_anchor = min_time + hours(window_hours)) %>%
  filter(earliest_valid_anchor <= max_time) %>%
  rowwise() %>%
  mutate(random_anchor_dttm = as.POSIXct(
    runif(1, min = as.numeric(earliest_valid_anchor), max = as.numeric(max_time)),
    origin = "1970-01-01", tz = tz(min_time))) %>%
  ungroup() %>%
  select(hospitalization_id, random_anchor_dttm)

no_event_df <- no_event_df %>% inner_join(no_event_anchors, by = "hospitalization_id")

traj_control <- build_trajectory(no_event_df, "random_anchor_dttm", "No Event")

# COMBINE
traj_all <- bind_rows(traj_icu, traj_vent, traj_vaso, traj_death, traj_composite, traj_control)

# AVERAGE SCORE PER ENCOUNTER, PER HOUR BIN
per_encounter <- traj_all %>%
  group_by(hospitalization_id, Group, hours_bin) %>%
  summarise(score_value = mean(score_value, na.rm = TRUE), .groups = "drop")

# SUMMARIZE ACROSS ENCOUNTERS
fig_4_df <- per_encounter %>%
  group_by(Group, hours_bin) %>%
  summarise(
    mean_score   = mean(score_value, na.rm = TRUE),
    se_score     = sd(score_value, na.rm = TRUE) / sqrt(n()),
    n_encounters = n(),
    .groups = "drop") %>%
  filter(n_encounters >= min_n) %>%
  mutate(Group = factor(Group, levels = c("Adverse Outcome", "ICU", "Vasopressors",
                                          "Ventilator", "In-hospital Mortality", "No Event"))) %>% 
  rename(Outcome = Group)

# PLOT
fig_4 <- ggplot(fig_4_df, aes(x = hours_bin, y = mean_score, color = Outcome, fill = Outcome)) +
  geom_ribbon(aes(ymin = mean_score - 1.96 * se_score,
                  ymax = mean_score + 1.96 * se_score),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(breaks = seq(-window_hours, 0, by = 6),
                     limits = c(-window_hours, 0),
                     expand = c(0, 0)) +
  scale_color_manual(values = outcome_colors) +
  scale_fill_manual(values = outcome_colors) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, family = "Times New Roman"),
    axis.text = element_text(size = 12, family = "Times New Roman"),
    axis.title = element_text(size = 14, family = "Times New Roman"),
    legend.text = element_text(size = 12, family = "Times New Roman"),
    legend.title = element_text(size = 12, family = "Times New Roman")) +
  labs(
    x = "Hours Before Event",
    y = "Mean EDI Score",
    title = "Mean EDI Score Trajectory Leading Up to Adverse Events",
    color = "Outcome",
    fill = "Outcome")

ggsave(filename = "/Users/mcschwi/Desktop/fig_4.png", plot = fig_4, width = 12, height = 6, dpi = 300)
write.csv(fig_traj_df, "/Users/mcschwi/Desktop/fig_4_df.csv", row.names = FALSE)











# ************************** FIGURE 5: ENCOUNTER-LEVEL PERFORMANCE BY THRESHOLD  **************************

thresholds <- seq(0, 100, by = 5)

# Build hosp-level Outcome/Max_Score directly from edi4, selecting only needed columns first
compute_threshold_metrics <- function(data, event_time_col, outcome_label, thresholds) {
  filtered <- data %>%
    select(hospitalization_id, recorded_dttm, score_value, all_of(event_time_col)) %>%
    filter(is.na(.data[[event_time_col]]) | .data[[event_time_col]] > recorded_dttm)
  
  hosp_df <- filtered %>%
    group_by(hospitalization_id) %>%
    summarise(
      Outcome   = as.integer(any(!is.na(.data[[event_time_col]]))),
      Max_Score = max(score_value, na.rm = TRUE),
      .groups = "drop")
  
  rm(filtered)
  gc(verbose = FALSE)
  
  map_dfr(thresholds, function(thr) {
    Predicted <- as.integer(hosp_df$Max_Score >= thr)
    TP <- sum(Predicted == 1 & hosp_df$Outcome == 1)
    FN <- sum(Predicted == 0 & hosp_df$Outcome == 1)
    TN <- sum(Predicted == 0 & hosp_df$Outcome == 0)
    FP <- sum(Predicted == 1 & hosp_df$Outcome == 0)
    
    tibble(
      Threshold   = thr,
      Outcome     = outcome_label,
      Sensitivity = TP / (TP + FN),
      Specificity = TN / (TN + FP),
      PPV         = TP / (TP + FP),
      NPV         = TN / (TN + FN))
  })
}

# RUN ONE OUTCOME AT A TIME, straight from edi4 — no duplicated global tables
threshold_perf <- bind_rows(
  compute_threshold_metrics(edi4, "icu_time",             "ICU",                   thresholds),
  compute_threshold_metrics(edi4, "vasopressor_time",     "Vasopressors",          thresholds),
  compute_threshold_metrics(edi4, "vent_time",            "Ventilator",            thresholds),
  compute_threshold_metrics(edi4, "death_time",           "In-hospital Mortality", thresholds),
  compute_threshold_metrics(edi4, "adverse_outcome_time", "Adverse Outcome",       thresholds)) %>%
  mutate(Outcome = factor(Outcome, levels = c("Adverse Outcome", "ICU", "Vasopressors",
                                              "Ventilator", "In-hospital Mortality")))

# Reshape to long format (skip if already done)
threshold_perf_long <- threshold_perf %>%
  pivot_longer(cols = c(Sensitivity, Specificity, PPV, NPV),
               names_to = "Metric",
               values_to = "Value")

# Redefine the function (the NEW version, no metric_col)
plot_threshold_metric <- function(metric_name) {
  plot_df <- threshold_perf_long %>% filter(Metric == metric_name)
  
  ggplot(plot_df, aes(x = Threshold, y = Value, color = Outcome)) +
    geom_line(linewidth = 1) +
    scale_x_continuous(breaks = seq(0, 100, by = 20)) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, family = "Times New Roman"),
      axis.text = element_text(size = 12, family = "Times New Roman"),
      axis.title = element_text(size = 14, family = "Times New Roman"),
      legend.text = element_text(size = 14, family = "Times New Roman"),
      legend.title = element_text(size = 14, family = "Times New Roman")) +
    labs(
      x = "EDI Threshold",
      y = "Value",
      title = metric_name,
      color = "Outcome")
}

fig_sens <- plot_threshold_metric("Sensitivity")
fig_spec <- plot_threshold_metric("Specificity")
fig_ppv  <- plot_threshold_metric("PPV")
fig_npv  <- plot_threshold_metric("NPV")

fig_5 <- (fig_sens + fig_spec) / (fig_ppv + fig_npv) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Encounter-Level Performance of EDI",
    theme = theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5, family = "Times New Roman"))) &
  theme(legend.position = "bottom")

# Save dataframe and plot
ggsave("/Users/mcschwi/Desktop/fig_5.png", fig_5, width = 14, height = 10, dpi = 300)
threshold_perf_long <- threshold_perf_long %>% mutate(site = "emory")
write.csv(threshold_perf_long, "/Users/mcschwi/Desktop/fig_5_df.csv", row.names = FALSE)




# ************************** FIGURE 6: LEAD TIME BY THRESHOLD **************************

compute_lead_time <- function(data, event_time_col, outcome_label, thresholds) {
  base_df <- data %>%
    select(hospitalization_id, recorded_dttm, score_value, all_of(event_time_col)) %>%
    filter(!is.na(.data[[event_time_col]]), recorded_dttm <= .data[[event_time_col]])
  
  map_dfr(thresholds, function(thr) {
    above_thr <- base_df %>% filter(score_value >= thr)
    
    if (nrow(above_thr) == 0) {
      return(tibble(Threshold = thr, Outcome = outcome_label,
                    Mean_Lead_Time = NA_real_, CI_Lower = NA_real_, CI_Upper = NA_real_, N = 0))
    }
    
    crossings <- above_thr %>%
      group_by(hospitalization_id) %>%
      summarise(
        first_crossing = min(recorded_dttm),
        event_time      = first(.data[[event_time_col]]),
        .groups = "drop") %>%
      mutate(lead_time_hours = as.numeric(difftime(event_time, first_crossing, units = "hours")))
    
    n <- nrow(crossings)
    mean_lt <- mean(crossings$lead_time_hours)
    se_lt   <- sd(crossings$lead_time_hours) / sqrt(n)
    
    tibble(
      Threshold = thr,
      Outcome = outcome_label,
      Mean_Lead_Time = mean_lt,
      CI_Lower = mean_lt - 1.96 * se_lt,
      CI_Upper = mean_lt + 1.96 * se_lt,
      N = n)
  })
}

min_n <- 20

fig_6_df <- bind_rows(
  compute_lead_time(edi4, "icu_time",             "ICU",                   thresholds),
  compute_lead_time(edi4, "vasopressor_time",     "Vasopressors",          thresholds),
  compute_lead_time(edi4, "vent_time",            "Ventilator",            thresholds),
  compute_lead_time(edi4, "death_time",           "In-hospital Mortality", thresholds),
  compute_lead_time(edi4, "adverse_outcome_time", "Adverse Outcome",       thresholds)) %>%
  mutate(Outcome = factor(Outcome, levels = c("Adverse Outcome", "ICU", "Vasopressors",
                                              "Ventilator", "In-hospital Mortality"))) %>%
  filter(N >= min_n)

fig_6 <- ggplot(fig_6_df, aes(x = Threshold, y = Mean_Lead_Time, color = Outcome, fill = Outcome)) +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 80, by = 20), limits = c(0, 80)) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, family = "Times New Roman"),
    axis.text = element_text(size = 12, family = "Times New Roman"),
    axis.title = element_text(size = 14, family = "Times New Roman"),
    legend.text = element_text(size = 12, family = "Times New Roman"),
    legend.title = element_text(size = 12, family = "Times New Roman")) +
  labs(
    x = "EDI Threshold",
    y = "Lead Time (min)",
    title = "Mean Lead Time Among Threshold-Crossing Encounters",
    color = "Outcome",
    fill = "Outcome")

fig_6

ggsave("/Users/mcschwi/Desktop/fig_6.png", fig_6, width = 10, height = 6, dpi = 300)
write.csv(fig_6_df, "/Users/mcschwi/Desktop/fig_6_df.csv", row.names = FALSE)
