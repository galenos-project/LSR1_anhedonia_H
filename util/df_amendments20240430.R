# df amendments 30.04.24 ---- 
# renaming study for easier reference
df$studlab[df$studlab=='A multicentre comparative... (1994) - 89915739'] <- 'UK Moclobomide Study Group (1994)'

#error corrections from data extraction, all corrected in original source (EPPI) and reflected in data/human file
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
# bymaster 2011
indices_to_modify <- which(grepl("^Bymaster \\(2011\\)", df$studlab) & df$treatment == "amitifadine")
if (length(indices_to_modify) > 0) {
  df$anhedonia_baseline_mean[indices_to_modify] <- 12.7 } # entered as 12.7 
indices_to_modify <- which(grepl("^Bymaster \\(2011\\)", df$studlab) & df$treatment == "amitifadine")
if (length(indices_to_modify) > 0) {
  df$anhedonia_followup_n[indices_to_modify] <- 30 } #entered as 33 
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
# iosifescu 2022
indices_to_modify <- which(grepl("^Iosifescu \\(2022\\)", df$studlab) & df$treatment == "placebo")
if (length(indices_to_modify) > 0) {
  df$n_randomised[indices_to_modify] <- "164" } # entered as 162
indices_to_modify <- which(grepl("^Iosifescu \\(2022\\)", df$studlab) & df$treatment == "dextromethorphan-bupropion")
if (length(indices_to_modify) > 0) {
  df$n_randomised[indices_to_modify] <- "163" } # entered as 156

#change columns to numeric
df[[3]] <- as.numeric(as.character(df[[3]]))
df[[16]] <- as.numeric(as.character(df[[16]]))

