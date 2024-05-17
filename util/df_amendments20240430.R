# df amendments 30.04.24 ---- 
# renaming study for easier reference
df$studlab[df$studlab=='A multicentre comparative... (1994) - 89915739'] <- 'UK Moclobomide Study Group (1994)'

#error corrections from data extraction, all corrected in original source (EPPI) and reflected in data/human file
# bymaster 2011
indices_to_modify <- which(grepl("^Bymaster \\(2011\\)", df$studlab) & df$treatment == "amitifadine")
if (length(indices_to_modify) > 0) {
  df$anhedonia_baseline_mean[indices_to_modify] <- 12.7 } # entered as 12.8 
indices_to_modify <- which(grepl("^Bymaster \\(2011\\)", df$studlab) & df$treatment == "amitifadine")
if (length(indices_to_modify) > 0) {
  df$anhedonia_followup_n[indices_to_modify] <- 30 } #entered as 33 
# coleman 1999
indices_to_modify <- which(grepl("^Coleman \\(1999\\)", df$studlab) & df$treatment == "bupropion")
if (length(indices_to_modify) > 0) {
     df$r_tolerability[indices_to_modify] <- 7 } # entered as 8 
# coleman 2001
indices_to_modify <- which(grepl("^Coleman \\(2001\\)", df$studlab) & df$treatment == "bupropion")
if (length(indices_to_modify) > 0) {
  df$r_acceptability[indices_to_modify] <- 56 } # entered as 13 
indices_to_modify <- which(grepl("^Coleman \\(2001\\)", df$studlab) & df$treatment == "bupropion")
if (length(indices_to_modify) > 0) {
  df$r_tolerability[indices_to_modify] <- 13 } # entered as NA
indices_to_modify <- which(grepl("^Coleman \\(2001\\)", df$studlab) & df$treatment == "placebo")
if (length(indices_to_modify) > 0) {
  df$r_tolerability[indices_to_modify] <- 5 } # entered as NA 
# koshino  2013
indices_to_modify <- which(grepl("^Koshino \\(2013\\)", df$studlab) & df$age_mean == "37.50")
if (length(indices_to_modify) > 0) {
  df$r_tolerability[indices_to_modify] <- 9 } # entered as 12 
# hewett 2010a
indices_to_modify <- which(grepl("^Coleman \\(2001\\)", df$studlab) & df$treatment == "placebo")
if (length(indices_to_modify) > 0) {
  df$anxiety_baseline_sd[indices_to_modify] <- 6.8 } # entered as 6.81 
# raskin 1972
indices_to_modify <- which(grepl("^Raskin \\(1972\\)", df$studlab) & df$treatment == "placebo")
if (length(indices_to_modify) > 0) {
  df$r_tolerability[indices_to_modify] <- 0 } # entered as NA 
# ravaris 1976
indices_to_modify <- which(grepl("^Ravaris \\(1976\\)", df$studlab) & df$age_mean == 41.2)
if (length(indices_to_modify) > 0) {
  df$r_tolerability[indices_to_modify] <- 0 } # entered as NA 
indices_to_modify <- which(grepl("^Ravaris \\(1976\\)", df$studlab) & df$treatment == "placebo")
if (length(indices_to_modify) > 0) {
  df$r_tolerability[indices_to_modify] <- 0 } # entered as NA 
 # jarrett 1999
indices_to_modify <- which(grepl("^Jarrett \\(1999\\)", df$studlab) & df$treatment == "phenelzine")
if (length(indices_to_modify) > 0) {
  df$r_tolerability[indices_to_modify] <- NA } # entered as 5
indices_to_modify <- which(grepl("^Jarrett \\(1999\\)", df$studlab) & df$treatment == "placebo")
if (length(indices_to_modify) > 0) {
  df$r_tolerability[indices_to_modify] <- NA } # entered as 15
indices_to_modify <- which(grepl("^Jarrett \\(1999\\)", df$studlab) & df$treatment == "phenelzine")
if (length(indices_to_modify) > 0) {
  df$tx_duration[indices_to_modify] <- 10 } # entered as 8.8
# iosifescu 2022
indices_to_modify <- which(grepl("^Iosifescu \\(2022\\)", df$studlab) & df$treatment == "placebo")
if (length(indices_to_modify) > 0) {
  df$n_randomised[indices_to_modify] <- 164 } # entered as 162
indices_to_modify <- which(grepl("^Iosifescu \\(2022\\)", df$studlab) & df$treatment == "dextromethorphan-bupropion")
if (length(indices_to_modify) > 0) {
  df$n_randomised[indices_to_modify] <- 163 } # entered as 156
# delbello 2014
indices_to_modify <- which(grepl("^DelBello \\(2014\\)", df$studlab) & df$treatment == "placebo")
if (length(indices_to_modify) > 0) {
  df$tx_duration[indices_to_modify] <- 12 } # entered as 8

#change columns to numeric
df[[3]] <- as.numeric(as.character(df[[3]]))
df[[16]] <- as.numeric(as.character(df[[16]]))


# code to add clayton 2006a BUP arm to df
new_row <- data.frame(
  studlab = "Clayton (2006a) - 98623053", year = 2006, n_randomised = 141, treatment_details = "bupropion XL", treatment = "bupropion",
  age_mean = 36.5, female_n = 85, dose_delivered_min = 300, dose_delivered_mean = 323, dose_delivered_max = 450, 
  dose_planned_min = 300, dose_planned_max = 450, tx_duration = 8, study_duration = "15 months", 
  r_acceptability = 32, r_tolerability = 4, anhedonia_baseline_scale = NA, anhedonia_baseline_direction = NA, 
  anhedonia_baseline_n = NA, anhedonia_baseline_mean = NA, anhedonia_baseline_sd = NA, anhedonia_followup_scale = NA, 
  anhedonia_followup_direction = NA, anhedonia_followup_n = NA, anhedonia_followup_mean = NA, anhedonia_followup_sd = NA, 
  anhedonia_followup_type = NA, anhedonia_followup_time = NA, anxiety_baseline_scale = NA, anxiety_baseline_direction = NA, 
  anxiety_baseline_n = NA, anxiety_baseline_mean = NA, anxiety_baseline_sd = NA, anxiety_followup_scale = NA, 
  anxiety_followup_direction = NA, anxiety_followup_n = NA, anxiety_followup_mean = NA, anxiety_followup_sd = NA, 
  anxiety_followup_type = NA, anxiety_followup_time = NA, reward_baseline_scale = NA, reward_baseline_direction = NA, 
  reward_baseline_n = NA, reward_baseline_mean = NA, reward_baseline_sd = NA, reward_followup_scale = NA, 
  reward_followup_direction = NA, reward_followup_n = NA, reward_followup_mean = NA, reward_followup_sd = NA, 
  reward_followup_type = NA, reward_followup_time = NA, AE = "Headache=43;Dry mouth=32;Nausea=32;Insomnia=21;Constipation=11;Diarrhea=11;Abnormal dreams=10;Nasopharyngitis=9;Decreased appetite=9;Irritability=8;Pharyngolaryngeal pain=7;Dizziness=5;Fatigue=5;Somnolence=4;Nasal Congestion=4;Yawning=1",
  female_prop = 0.6028369, arm = "active", treatment_old = "buproprion XL", nausea = 32, headache = 43, 
  insomnia = 21, dry_mouth = 32, constipation = 11, dizziness = 5, vomiting = NA
)
df <- bind_rows(df, new_row)

# code to add clayton 2006a PLA arm to df
new_row <- data.frame(
  studlab = "Clayton (2006a) - 98623053", year = 2006, n_randomised = 141, treatment_details = "placebo", treatment = "placebo",
  age_mean = 35.1, female_n = 88, dose_delivered_min = NA, dose_delivered_mean = NA, dose_delivered_max = NA, 
  dose_planned_min = NA, dose_planned_max = NA, tx_duration = 8, study_duration = "15 months", 
  r_acceptability = 39, r_tolerability = 7, anhedonia_baseline_scale = NA, anhedonia_baseline_direction = NA, 
  anhedonia_baseline_n = NA, anhedonia_baseline_mean = NA, anhedonia_baseline_sd = NA, anhedonia_followup_scale = NA, 
  anhedonia_followup_direction = NA, anhedonia_followup_n = NA, anhedonia_followup_mean = NA, anhedonia_followup_sd = NA, 
  anhedonia_followup_type = NA, anhedonia_followup_time = NA, anxiety_baseline_scale = NA, anxiety_baseline_direction = NA, 
  anxiety_baseline_n = NA, anxiety_baseline_mean = NA, anxiety_baseline_sd = NA, anxiety_followup_scale = NA, 
  anxiety_followup_direction = NA, anxiety_followup_n = NA, anxiety_followup_mean = NA, anxiety_followup_sd = NA, 
  anxiety_followup_type = NA, anxiety_followup_time = NA, reward_baseline_scale = NA, reward_baseline_direction = NA, 
  reward_baseline_n = NA, reward_baseline_mean = NA, reward_baseline_sd = NA, reward_followup_scale = NA, 
  reward_followup_direction = NA, reward_followup_n = NA, reward_followup_mean = NA, reward_followup_sd = NA, 
  reward_followup_type = NA, reward_followup_time = NA, AE = "Headache=38;Dry mouth=13;Nausea=21;Insomnia=7;Constipation=7;Diarrhea=10;Abnormal dreams=8;Nasopharyngitis=3;Decreased appetite=7;Irritability=4;Anxiety=3;Pharyngolaryngeal pain=8;Dizziness=7;Fatigue=9;Somnolence=6;Nasal Congestion=9;Yawning=3",
  female_prop = 0.6241135, arm = "placebo", treatment_old = "placebo", nausea = 21, headache = 38, 
  insomnia = 7, dry_mouth = 13, constipation = 17, dizziness = 7, vomiting = NA
)
df <- bind_rows(df, new_row)

# code to add clayton 2006b BUP arm to df
new_row <- data.frame(
  studlab = "Clayton (2006b) - 87998176", year = 2006, n_randomised = 138, treatment_details = "bupropion XL", treatment = "bupropion",
  age_mean = 37.0, female_n = 76, dose_delivered_min = 300, dose_delivered_mean = 309, dose_delivered_max = 450, 
  dose_planned_min = 300, dose_planned_max = 450, tx_duration = 8, study_duration = "16 months", 
  r_acceptability = 36, r_tolerability = 13, anhedonia_baseline_scale = NA, anhedonia_baseline_direction = NA, 
  anhedonia_baseline_n = NA, anhedonia_baseline_mean = NA, anhedonia_baseline_sd = NA, anhedonia_followup_scale = NA, 
  anhedonia_followup_direction = NA, anhedonia_followup_n = NA, anhedonia_followup_mean = NA, anhedonia_followup_sd = NA, 
  anhedonia_followup_type = NA, anhedonia_followup_time = NA, anxiety_baseline_scale = NA, anxiety_baseline_direction = NA, 
  anxiety_baseline_n = NA, anxiety_baseline_mean = NA, anxiety_baseline_sd = NA, anxiety_followup_scale = NA, 
  anxiety_followup_direction = NA, anxiety_followup_n = NA, anxiety_followup_mean = NA, anxiety_followup_sd = NA, 
  anxiety_followup_type = NA, anxiety_followup_time = NA, reward_baseline_scale = NA, reward_baseline_direction = NA, 
  reward_baseline_n = NA, reward_baseline_mean = NA, reward_baseline_sd = NA, reward_followup_scale = NA, 
  reward_followup_direction = NA, reward_followup_n = NA, reward_followup_mean = NA, reward_followup_sd = NA, 
  reward_followup_type = NA, reward_followup_time = NA, AE = "Headache=33;Dry mouth=30;Nausea=17;Constipation=15;Dizziness=13;Insomnia=18;Diarrhea=10;Fatigue=7;Abdominal pain-upper=6;Tremor=6;Irritability=6;Dyspepsia=5;Flatulence=4;Somnolence=4;Palpitations=4;Upper respiratory tract infection=3;Toothache=2;Back pain=2;Sinus congestion=2",
  female_prop = 0.5507246, arm = "active", treatment_old = "buproprion", nausea = 17, headache = 33, 
  insomnia = 18, dry_mouth = 30, constipation = 15, dizziness = 13, vomiting = NA
)
df <- bind_rows(df, new_row)

# code to add clayton 2006b PLA arm to df
new_row <- data.frame(
  studlab = "Clayton (2006b) - 87998176", year = 2006, n_randomised = 137, treatment_details = "placebo", treatment = "placebo",
  age_mean = 37.0, female_n = 76, dose_delivered_min = NA, dose_delivered_mean = NA, dose_delivered_max = NA, 
  dose_planned_min = NA, dose_planned_max = NA, tx_duration = 8, study_duration = "16 months", 
  r_acceptability = 27, r_tolerability = 6, anhedonia_baseline_scale = NA, anhedonia_baseline_direction = NA, 
  anhedonia_baseline_n = NA, anhedonia_baseline_mean = NA, anhedonia_baseline_sd = NA, anhedonia_followup_scale = NA, 
  anhedonia_followup_direction = NA, anhedonia_followup_n = NA, anhedonia_followup_mean = NA, anhedonia_followup_sd = NA, 
  anhedonia_followup_type = NA, anhedonia_followup_time = NA, anxiety_baseline_scale = NA, anxiety_baseline_direction = NA, 
  anxiety_baseline_n = NA, anxiety_baseline_mean = NA, anxiety_baseline_sd = NA, anxiety_followup_scale = NA, 
  anxiety_followup_direction = NA, anxiety_followup_n = NA, anxiety_followup_mean = NA, anxiety_followup_sd = NA, 
  anxiety_followup_type = NA, anxiety_followup_time = NA, reward_baseline_scale = NA, reward_baseline_direction = NA, 
  reward_baseline_n = NA, reward_baseline_mean = NA, reward_baseline_sd = NA, reward_followup_scale = NA, 
  reward_followup_direction = NA, reward_followup_n = NA, reward_followup_mean = NA, reward_followup_sd = NA, 
  reward_followup_type = NA, reward_followup_time = NA, AE = "Headache=38;Dry mouth=17;Nausea=14;Constipation=9;Dizziness=5;Insomnia=14;Diarrhea=12;Fatigue=7;Abdominal pain-upper=1;Tremor=2;Irritability=6;Dyspepsia=12;Flatulence=3;Somnolence=8;Palpitations=6;Upper respiratory tract infection=5;Toothache=6;Back pain=6;Sinus congestion=6;Stomach discomfort=6;Pain in extremity=6",
  female_prop = 0.5547445, arm = "placebo", treatment_old = "placebo", nausea = 14, headache = 38, 
  insomnia = 14, dry_mouth = 17, constipation = 9, dizziness = 5, vomiting = NA
)

df <- bind_rows(df, new_row)