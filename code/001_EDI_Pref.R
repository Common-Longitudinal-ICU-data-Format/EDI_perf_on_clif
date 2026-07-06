
# *************** EDI in ALL PATIENTS - CLIF *******************************

# ***************************************************************************

library(pacman)
p_load(dplyr, tidyverse, haven, lubridate, rio, ggplot2, tidyr, caret, pROC, Metrics, ggtern, survival, timeROC, forecast, mgcv, ggridges, ggplot2, broom, binom, boot, tibble, arrow, ggVennDiagram)


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
  
  # DEATH_TIME - if clif_patient.death_dttm <= 30 days of clif_hospitalization.admit_dttm -> DEATH_DTTM ; if clif_hospitalization.discharge_category = 'Expired' or 'Hospice' <= 30 days of clif_hospitalization.admit_dttm -> DISCHARGE_DTTM


# ** EDI
  # EDI_SCORE
  # SCORE_TIME




# ********************************************************************************************************************************************************************************************************************************
# 1.) IMPORT/CLEAN DATA

# ** HOSPITALIZATION INFO ** = clif_adt
hosp <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Emory CLIF Data/clif_hospitalization.parquet") %>% 
  select(patient_id, hospitalization_id, admission_dttm, discharge_dttm, age_at_admission, discharge_category) 


# ** DEMOGRAPHICS ** = clif_patient 
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


#mortality_30 = clif_patient: death_dttm is within 30 days of arrival OR clif_hospitalization: discharge dispo of 'Expired' or 'Hospice' within 30 days of arrival
mortality_30 <- hosp %>%
  select(patient_id, hospitalization_id, admission_dttm, discharge_dttm, discharge_category) %>%
  inner_join(patient %>% select(patient_id, death_dttm), by = "patient_id") %>% distinct()

mortality_30 <- mortality_30 %>%
  group_by(hospitalization_id) %>%
  mutate(death_within_30 = !is.na(death_dttm) & death_dttm <= admission_dttm + days(30),
         death_dispo_within_30 = discharge_category %in% c("Expired", "Hospice") & discharge_dttm <= admission_dttm + days(30),
         death_time = case_when(death_within_30 ~ death_dttm,   # prioritize actual death time if both happen
                                death_dispo_within_30 ~ discharge_dttm,
                                TRUE ~ as.POSIXct(NA)))

mortality_30 <- mortality_30 %>% select(patient_id, hospitalization_id, death_time)

#los
hosp_los <- hosp %>%
  select(-age_at_admission, -discharge_category) %>%
  mutate(hosp_los = as.numeric(difftime(discharge_dttm, admission_dttm, units = "days")))


#COMBINE DEMO/OUTCOMES
demo_outcomes <- demo %>% left_join(icu, by='hospitalization_id') %>%
                          left_join(vent, by='hospitalization_id') %>%
                          left_join(vasopressor, by='hospitalization_id') %>%
                          left_join(mortality_30, by=c('hospitalization_id', 'patient_id')) %>%
                          left_join(hosp_los, by=c('hospitalization_id', 'patient_id'))

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



# ********************************************************************************************************************************************************************************************************************************
# 3.) DATA ANALYSIS

#edi4 <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Dr. Siva Bhavani/EDI Prediction Model/ALL PATIENTS/Saved Data/edi4.parquet")


# **************************** TABLE 1: DEMOGRAPHICS ****************************
# 1.) ALL ENCOUNTERS 
table1 <- edi4 %>% select(-recorded_dttm, -score_value, -arrival_to_score, -score_to_discharge) %>% ungroup() %>% distinct() #1 row per enc
# write_parquet(table1, "/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Dr. Siva Bhavani/EDI Prediction Model/ALL PATIENTS/Saved Data/table1.parquet")


quantile(table1$age_at_admission,  probs = c(0.25, 0.5, 0.75),  na.rm = TRUE) #age
table1 %>% filter(!is.na(sex_category)) %>% count(sex_category, name = "N") %>% mutate(Percent = round(100 * N / sum(N), 1)) #sex - female
table1 %>% filter(!is.na(ethnicity_category)) %>% count(ethnicity_category, name = "N") %>% mutate(Percent = round(100 * N / sum(N), 1)) #ethnicity - hispanic
table1 %>% filter(!is.na(race_category_clean)) %>% count(race_category_clean, name = "N") %>% mutate(Percent = round(100 * N / sum(N), 1)) #race
quantile(table1$hosp_los, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) #Hosp LOS

table1 %>% summarise(N = n(),
                    ICU_n = sum(!is.na(icu_time)),
                    ICU_pct = mean(!is.na(icu_time)) * 100,
                    Mortality30_n = sum(!is.na(death_time)),
                    Mortality30_pct = mean(!is.na(death_time)) * 100,
                    Vasopressor_n = sum(!is.na(vasopressor_time)),
                    Vasopressor_pct = mean(!is.na(vasopressor_time)) * 100,
                    Vent_n = sum(!is.na(vent_time)),
                    Vent_pct = mean(!is.na(vent_time)) * 100)


# 2.) MAX THRESHOLD >=40 
edi_GE_40 <- edi4 %>% 
  group_by(hospitalization_id) %>% 
  filter(max(score_value) >= 40) %>% 
  select(-recorded_dttm, -score_value, -arrival_to_score, -score_to_discharge) %>% 
  ungroup() %>% distinct() 
#length(unique(edi_GE_40$hospitalization_id)) #27,017

quantile(edi_GE_40$age_at_admission,  probs = c(0.25, 0.5, 0.75),  na.rm = TRUE) #age
edi_GE_40 %>% filter(!is.na(sex_category)) %>% count(sex_category, name = "N") %>% mutate(Percent = round(100 * N / sum(N), 1)) #sex - female
edi_GE_40 %>% filter(!is.na(ethnicity_category)) %>% count(ethnicity_category, name = "N") %>% mutate(Percent = round(100 * N / sum(N), 1)) #ethnicity - hispanic
edi_GE_40 %>% filter(!is.na(race_category_clean)) %>% count(race_category_clean, name = "N") %>% mutate(Percent = round(100 * N / sum(N), 1)) #race
quantile(edi_GE_40$hosp_los, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) #Hosp LOS

edi_GE_40 %>% summarise(N = n(),
                     ICU_n = sum(!is.na(icu_time)),
                     ICU_pct = mean(!is.na(icu_time)) * 100,
                     Mortality30_n = sum(!is.na(death_time)),
                     Mortality30_pct = mean(!is.na(death_time)) * 100,
                     Vasopressor_n = sum(!is.na(vasopressor_time)),
                     Vasopressor_pct = mean(!is.na(vasopressor_time)) * 100,
                     Vent_n = sum(!is.na(vent_time)),
                     Vent_pct = mean(!is.na(vent_time)) * 100)


# 3.) MAX THRESHOLD <40 
edi_LT_40 <- edi4 %>% 
  group_by(hospitalization_id) %>% 
  filter(max(score_value) < 40) %>% 
  select(-recorded_dttm, -score_value, -arrival_to_score, -score_to_discharge) %>% 
  ungroup() %>% distinct() 
#length(unique(edi_LT_40$hospitalization_id)) #45,207

quantile(edi_LT_40$age_at_admission,  probs = c(0.25, 0.5, 0.75),  na.rm = TRUE) #age
edi_LT_40 %>% filter(!is.na(sex_category)) %>% count(sex_category, name = "N") %>% mutate(Percent = round(100 * N / sum(N), 1)) #sex - female
edi_LT_40 %>% filter(!is.na(ethnicity_category)) %>% count(ethnicity_category, name = "N") %>% mutate(Percent = round(100 * N / sum(N), 1)) #ethnicity - hispanic
edi_LT_40 %>% filter(!is.na(race_category_clean)) %>% count(race_category_clean, name = "N") %>% mutate(Percent = round(100 * N / sum(N), 1)) #race
quantile(edi_LT_40$hosp_los, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) #Hosp LOS

edi_LT_40 %>% summarise(N = n(),
                        ICU_n = sum(!is.na(icu_time)),
                        ICU_pct = mean(!is.na(icu_time)) * 100,
                        Mortality30_n = sum(!is.na(death_time)),
                        Mortality30_pct = mean(!is.na(death_time)) * 100,
                        Vasopressor_n = sum(!is.na(vasopressor_time)),
                        Vasopressor_pct = mean(!is.na(vasopressor_time)) * 100,
                        Vent_n = sum(!is.na(vent_time)),
                        Vent_pct = mean(!is.na(vent_time)) * 100)




# **************************** TABLE 2: EDI PERFORMANCE EVALUATION ****************************

threshold <- 40.0

# ************************** OUTCOME 1: ICU **************************
#edi4 <- read_parquet("/Users/mcschwi/Library/CloudStorage/OneDrive-EmoryUniversity/Documents/Dr. Siva Bhavani/EDI Prediction Model/ALL PATIENTS/Saved Data/edi4.parquet")
icu <- edi4 %>% filter(is.na(icu_time) | icu_time > recorded_dttm)

# 1.) TIME HORIZON 12 
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



# 2.) TIME HORIZON 24 
edi_24 <- icu %>% select(hospitalization_id, recorded_dttm, score_value, icu_time) %>%
  mutate(Score_Time_Horizon_24 = recorded_dttm + hours(24),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(24),
         Prediction_24 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_24 = if_else(!is.na(icu_time) & icu_time >= recorded_dttm & icu_time <= Score_Time_Horizon_24, 1L, 0L)) 

perf_data <- edi_24 %>% mutate(Outcome = Truth_24, Predicted = Prediction_24, Score = score_value) %>%
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

icu_24 <- results %>% mutate(Outcome = "ICU", Horizon = "24h")
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

# 1.) TIME HORIZON 12 
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




# 2.) TIME HORIZON 24 
edi_24 <- vaso %>% select(hospitalization_id, recorded_dttm, score_value, vasopressor_time) 
edi_24 <- edi_24 %>%
  mutate(Score_Time_Horizon_24 = recorded_dttm + hours(24),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(24),
         Prediction_24 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_24 = if_else(!is.na(vasopressor_time) & vasopressor_time >= recorded_dttm & vasopressor_time <= Score_Time_Horizon_24, 1L, 0L)) 

perf_data <- edi_24 %>% mutate(Outcome = Truth_24, Predicted = Prediction_24, Score = score_value) %>%
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

vaso_24 <- results %>% mutate(Outcome = "Vasopressors", Horizon = "24h")
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

# 1.) TIME HORIZON 12 
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




# 2.) TIME HORIZON 24 
edi_24 <- vent %>% select(hospitalization_id, recorded_dttm, score_value, vent_time) 

edi_24 <- edi_24 %>%
  mutate(Score_Time_Horizon_24 = recorded_dttm + hours(24),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(24),
         Prediction_24 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_24 = if_else(!is.na(vent_time) & vent_time >= recorded_dttm & vent_time <= Score_Time_Horizon_24, 1L, 0L)) 

perf_data <- edi_24 %>% mutate(Outcome = Truth_24, Predicted = Prediction_24, Score = score_value) %>%
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

vent_24 <- results %>% mutate(Outcome = "Ventilator", Horizon = "24h")
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

# 1.) TIME HORIZON 12 
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

mortality_12 <- results %>% mutate(Outcome = "30-Day Mortality", Horizon = "12h")
print(results)



# 2.) TIME HORIZON 24 
edi_24 <- mortality %>% select(hospitalization_id, recorded_dttm, score_value, death_time) 
edi_24 <- edi_24 %>%
  mutate(Score_Time_Horizon_12 = recorded_dttm + hours(24),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(24),
         Prediction_24 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_24 = if_else(!is.na(death_time) & death_time >= recorded_dttm & death_time <= Score_Time_Horizon_12, 1L, 0L)) 

perf_data <- edi_24 %>% mutate(Outcome = Truth_24, Predicted = Prediction_24, Score = score_value) %>%
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

mortality_24 <- results %>% mutate(Outcome = "30-Day Mortality", Horizon = "24h")
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

mortality_hosp <- results %>% mutate(Outcome = "30-Day Mortality", Horizon = "Hospital-Level")
print(results)




# ************************** OUTCOME 5: ANY OF THE 4 **************************

# ADVERSE_EVENT_TIME = Earliest time among the events, NA if none occurred
edi4 <- edi4 %>%
  group_by(hospitalization_id) %>%
  mutate(adverse_outcome = if_else(!is.na(icu_time) | !is.na(vent_time) | !is.na(vasopressor_time) | !is.na(death_time), 1, 0),
         adverse_outcome_time = if_else(adverse_outcome == 1, pmin(icu_time, vent_time, vasopressor_time, death_time, na.rm = TRUE), as.POSIXct(NA)))

adverse <- edi4 %>% filter(is.na(adverse_outcome_time) | adverse_outcome_time > recorded_dttm)


# 1.) TIME HORIZON 12 
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



# 2.) TIME HORIZON 24 
edi_24 <- adverse %>% select(hospitalization_id, recorded_dttm, score_value, adverse_outcome_time) 

edi_24 <- edi_24 %>%
  mutate(Score_Time_Horizon_12 = recorded_dttm + hours(24),
         Prediction = if_else(score_value >= threshold, 1, 0)) %>% 
  group_by(hospitalization_id) %>%
  arrange(recorded_dttm) %>%
  mutate(prediction_start_times = if_else(Prediction == 1, recorded_dttm, as.POSIXct(NA)),
         prediction_start_times = zoo::na.locf(prediction_start_times, na.rm = FALSE),
         prediction_end_time = prediction_start_times + hours(24),
         Prediction_24 = if_else(!is.na(prediction_start_times) & recorded_dttm <= prediction_end_time, 1L, 0L),
         Truth_24 = if_else(!is.na(adverse_outcome_time) & adverse_outcome_time >= recorded_dttm & adverse_outcome_time <= Score_Time_Horizon_12, 1L, 0L)) 

perf_data <- edi_24 %>% mutate(Outcome = Truth_24, Predicted = Prediction_24, Score = score_value) %>%
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

adverse_24 <- results %>% mutate(Outcome = "Adverse Outcome", Horizon = "24h")
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
    Horizon = factor(Horizon, levels = c("12h", "24h", "Hospital-Level")),
    Outcome = factor(Outcome, levels = c("Adverse Outcome", "ICU", "Vasopressors",
                                        "Ventilator", "30-Day Mortality")),
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
    plot.subtitle = element_text(size = 14, hjust = 0.5, family = "Times New Roman"),
    axis.text.x = element_text(size = 14, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    strip.text = element_text(face = "bold", size = 14, family = "Times New Roman"),
    strip.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()) +
  labs(
    title = "Performance of the EDI for Adverse Clinical Outcomes - Emory",
    subtitle = "EDI Threshold = 40; N = 72,224",
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


# ************************** IDEA 1: CALIBRATION PLOT FOR ADVERSE OUTCOME - ENCOUNTER-LEVEL  **************************

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
  labs(x = "Predicted risk (EDI)",
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



# ************************** IDEA 2: VENN DIAGRAM OF ADVERSE EVENT  **************************
fig_3_df <- edi4 %>%
  group_by(hospitalization_id) %>%
  summarise(
    site = "emory",
    icu = any(!is.na(icu_time)),
    vaso = any(!is.na(vasopressor_time)),
    vent = any(!is.na(vent_time)),
    mortality = any(!is.na(death_time)),
    .groups = "drop")

venn_list <- list(
  ICU = which(fig_3_df$icu == 1),
  Vasopressors = which(fig_3_df$vaso == 1),
  Ventilator = which(fig_3_df$vent == 1),
  Mortality = which(fig_3_df$mortality == 1))

fig_3 <- ggVennDiagram(venn_list) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_void() +
  theme(
    plot.title = element_text(
      family = "Times New Roman",
      face = "bold",
      size = 20,
      hjust = 0.5)) +
  labs(title = "Overlap of Components of Adverse Outcome Composite")

#Save Figure 3
ggsave(filename = "/Users/mcschwi/Desktop/fig_3.png",
       plot = fig_3,
       width = 12,
       height = 6,
       dpi = 300)

#Save Fig 3 Dataframe
write.csv(fig_3_df, "/Users/mcschwi/Desktop/fig_3_df.csv")






