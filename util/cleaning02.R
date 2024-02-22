rm(list = setdiff(ls(), lsf.str()))

library(readxl)
library(tidyr)
library(dplyr)

# set wd ----
setwd("~/Documents/GitHub/LSR1_anhedonia_H")

# load df ----
df.master <- read.csv('data/human/LSR1 - EPPI export - 20240131.csv')

# rds ----
if (dir.exists('rds/')==F) {
  dir.create('rds/')
  dir.create('rds/human/')
}

# amend GSK1994
df.master$grp1ArmName[df.master$grp1ArmName=='Bupropion 100-300' & df.master$ItemId_x=='89913899' & df.master$Participant_N==148] <- 'Placebo'
df.master$ArmTitle[df.master$grp1ArmName=='Placebo' & df.master$ItemId_x=='89913899'] <- 'Placebo '
df.master$Participant_N[df.master$grp1ArmName=='Bupropion 100-300' & df.master$ItemId_x=='89913899' & df.master$Participant_N==146] <- 144
df.master$Arm_N.dropouts..any.reason.[df.master$ItemId_x=='89913899' & df.master$grp1ArmName=='Placebo'] <- 48
df.master$Arm_N.dropouts..adverse.events.[df.master$ItemId_x=='89913899' & df.master$grp1ArmName=='Placebo'] <- 3
df.master$Arm_N.participants[df.master$ItemId_x=='89913899' & df.master$grp1ArmName=='Placebo'] <- 154
df.master$Arm_Mean.Age[df.master$ItemId_x=='89913899' & df.master$grp1ArmName=='Placebo'] <- 38.2
df.master$Arm_N.females[df.master$ItemId_x=='89913899' & df.master$grp1ArmName=='Placebo'] <- 99
df.master$Arm_Min[df.master$ItemId_x=='89913899' & df.master$grp1ArmName=='Placebo'] <- NA
df.master$Arm_Max[df.master$ItemId_x=='89913899' & df.master$grp1ArmName=='Placebo'] <- NA
df.master$Arm_Min.1[df.master$ItemId_x=='89913899' & df.master$grp1ArmName=='Placebo'] <- NA
df.master$Arm_Max.1[df.master$ItemId_x=='89913899' & df.master$grp1ArmName=='Placebo'] <- NA
df.master$grp1ArmName[df.master$grp1ArmName=='Placebo '] <- 'Placebo'
df.master$Arm_AE.details[df.master$ItemId_x=='89913899' & df.master$grp1ArmName=='Placebo' & df.master$ItemTimepointValue==8] <- df.master$Arm_AE.details[df.master$ItemId_x=='89913899' & df.master$grp1ArmName=='Placebo' & df.master$ItemTimepointValue==0]
df.master$InterventionText[df.master$grp1ArmName=='Placebo' & df.master$ItemId_x=='89913899'] <- 'Placebo'

# amend GSK1993
df.master$grp1ArmName[df.master$grp1ArmName=='Bupropion SR 100mg' & df.master$ItemId_x=='89913900' & df.master$Participant_N==114] <- 'Bupropion SR 200mg'
df.master$InterventionText[df.master$grp1ArmName=='Bupropion SR 200mg' & df.master$ItemId_x=='89913900'] <- 'Bupropion SR 200mg'
df.master$ArmTitle[df.master$grp1ArmName=='Bupropion SR 200mg' & df.master$ItemId_x=='89913900'] <- 'Bupropion SR 200mg'
df.master$Arm_N.dropouts..any.reason.[df.master$ItemId_x=='89913900' & df.master$grp1ArmName=='Bupropion SR 200mg'] <- 42
df.master$Arm_N.dropouts..adverse.events.[df.master$ItemId_x=='89913900' & df.master$grp1ArmName=='Bupropion SR 200mg'] <- 9
df.master$Arm_N.participants[df.master$ItemId_x=='89913900' & df.master$grp1ArmName=='Bupropion SR 200mg'] <- 120
df.master$Arm_N.females[df.master$ItemId_x=='89913900' & df.master$grp1ArmName=='Bupropion SR 200mg'] <- 65
df.master$Arm_Min[df.master$ItemId_x=='89913900' & df.master$grp1ArmName=='Bupropion SR 200mg'] <- NA
df.master$Arm_Max[df.master$ItemId_x=='89913900' & df.master$grp1ArmName=='Bupropion SR 200mg'] <- NA
df.master$Arm_Min.1[df.master$ItemId_x=='89913900' & df.master$grp1ArmName=='Bupropion SR 200mg'] <- NA
df.master$Arm_Max.1[df.master$ItemId_x=='89913900' & df.master$grp1ArmName=='Bupropion SR 200mg'] <- NA
df.master$Arm_AE.details[df.master$ItemId_x=='89913900' & df.master$grp1ArmName=='Bupropion SR 200mg' & df.master$ItemTimepointValue==8] <- df.master$Arm_AE.details[df.master$ItemId_x=='89913900' & df.master$grp1ArmName=='Bupropion SR 200mg' & df.master$ItemTimepointValue==0]

# var select
df.master$studlab <- paste0(df.master$ShortTitle_x, ' - ', df.master$ItemId_x)
df.master$score.type <- ifelse(is.na(df.master$Mean), NA, 'endpoint')
for (i in c('change', 'Change', 'improvement')) {
  df.master$score.type[which(grepl(i, df.master$OutcomeDescription))] <- 'change'
}
df <- df.master[, c(which(colnames(df.master)=='studlab'), 
                    which(colnames(df.master)=='Year'),
                    which(colnames(df.master)=='Arm_N.participants'),
                    which(colnames(df.master)=='InterventionText'),
                    which(colnames(df.master)=='ArmTitle'),
                    which(colnames(df.master)=='ItemTimepointValue'),
                    which(colnames(df.master)=='OutcomeText'),
                    which(colnames(df.master)=='Participant_N'),
                    which(colnames(df.master)=='Mean'),
                    which(colnames(df.master)=='SD...SE...CI.lower'),
                    which(colnames(df.master)=='score.type'),
                    which(colnames(df.master)=='Arm_N.dropouts..any.reason.'),
                    which(colnames(df.master)=='Arm_N.dropouts..adverse.events.'),
                    which(colnames(df.master)=='Arm_Mean.Age'),
                    which(colnames(df.master)=='Arm_N.females'),
                    which(colnames(df.master)=='Arm_Min'),
                    which(colnames(df.master)=='Arm_Mean'),
                    which(colnames(df.master)=='Arm_Max'),
                    which(colnames(df.master)=='Arm_Min.1'),
                    which(colnames(df.master)=='Arm_Max.1'),
                    which(colnames(df.master)=='Arm_Treatment.duration'),
                    which(colnames(df.master)=='Study_Study.length'),
                    which(colnames(df.master)=='Arm_AE.details')
                    )]

colnames(df) <- c('studlab',
                  'year',
                  'n_randomised',
                  'treatment_details',
                  'treatment',
                  'week', 
                  'c_scale', 
                  'c_n', 
                  'c_mean', 
                  'c_sd', 
                  'c_type', 
                  'r_acceptability', 
                  'r_tolerability', 
                  'age_mean', 
                  'female_n', 
                  'dose_delivered_min', 
                  'dose_delivered_mean', 
                  'dose_delivered_max', 
                  'dose_planned_min', 
                  'dose_planned_max', 
                  'tx_duration', 
                  'study_duration',
                  'AE')

# var clean
df[df=='tick'] = NA
df[df=='untick'] = NA
df[df=='na'] = NA
df$treatment <- tolower(df$treatment)
df$c_scale <- gsub("^\\*", "", df$c_scale)
df$c_scale <- gsub('"', "", df$c_scale)
df$dose_delivered_min <- gsub('mg', "", df$dose_delivered_min)
df$dose_delivered_mean <- gsub('mg', "", df$dose_delivered_mean)
df$dose_delivered_max <- gsub('mg', "", df$dose_delivered_max)
df$dose_planned_min <- gsub('mg', "", df$dose_planned_min)
df$dose_planned_max <- gsub('mg', "", df$dose_planned_max)
df$tx_duration <- gsub(' weeks', "", df$tx_duration)
df$tx_duration <- gsub('weeks', "", df$tx_duration)
df$tx_duration <- gsub(' week', "", df$tx_duration)
df[, c(2, 3, 6, 8:10, 12:20)] <- lapply(df[, c(2, 3, 6, 8:10, 12:20)], as.numeric)
df[df==99999] = NA

# compass
scale.compass <- data.frame(matrix(ncol = 1, nrow = length(unique(df$c_scale[!is.na(df$c_scale)]))))
colnames(scale.compass) <- 'direction'
rownames(scale.compass) <- sort(unique(df$c_scale))
scale.compass$direction <- 'negative'
scale.compass$direction[rownames(scale.compass) %in% c('Motivation and Energy Inventory', 
                                                       'Motivation and Energy Inventory (MEI)')] <- 'positive'

# check
unique(df$studlab[df$r_tolerability > df$r_acceptability])
sort(unique(df$week))
sort(unique(df$c_scale))

# multiple timepoints
for (i in unique(df$studlab)) {
  df1 <- df[df$studlab==i, ]
  df1 <- df1[!df1$week==0, ]
  if (length(unique(df1$week[!is.na(df1$week)])) > 1) {
    cat(paste0('Multiple timepoints for study ',
               unique(df1$studlab), 
               '.'), 
        sep = '\n')
  }
}
rm(df1)

# MEMO: CHECK THE NAME OF SCALES
unique(df$c_scale)

# multiple scales per timepoint
df <- df[!(df$studlab=='Ravaris (1976) - 87999576' & df$c_scale!='SHID - anxiety total subscale'), ]
df <- df[!(df$studlab=='Jefferson (2006) - 87998173' & df$c_scale=='IDS-IVR-30 (energy/pleasure/interest)'), ]
df <- df[!(df$studlab=='Jefferson (2006) - 87998173' & df$c_scale=='IDS-IVR-30 (anxiety)'), ]
df <- df[!(df$studlab=='GlaxoSmithKline (1980) - 89913897' & df$c_scale=='Zung SAS '), ]
df <- df[!(df$studlab=='GlaxoSmithKline (1980) - 89913897' & df$c_scale=='HAMD-21 Anxiety/Somatization Item'), ]
df <- df[!(df$studlab=='Giller (1982) - 87999463' & df$c_scale=='Covi Anxiety Scale Verbal'), ]
df <- df[!(df$studlab=='Giller (1982) - 87999463' & df$c_scale=='Covi Anxiety Scale Somatic'), ]
df <- df[!(df$studlab=='Davidson (1988) - 87999240' & df$c_scale=='SCL-58 Anxiety subscale'), ]
df <- df[!(df$studlab=='Davidson (1988) - 87999240' & df$c_scale=='SCL-90 Anxiety subscale'), ]
df <- df[!(df$studlab=='Casacchia (1984) - 87999374' & df$c_scale=='HRSD - somatic anxiety item'), ]
df <- df[!(df$studlab=='Learned (2012a) - 90047003' & df$week==1), ]

# scales
anhedonia.scales <- c('IDS-C-30 (energy/pleasure/interest)', 
                      'IDS-IVR-30 (energy/pleasure/interest)', 
                      'MADRS Anhedonia factor', 
                      'MASQ Anhedonia', 
                      'Motivation and Energy Inventory', 
                      'Motivation and Energy Inventory (MEI)', 
                      'Zung - anhedonia')
anxiety.scales <- c('Covi Anxiety Scale Somatic', 
                    'Covi Anxiety Scale Total', 
                    'Covi Anxiety Scale Verbal', 
                    'HAM-A', 
                    'HAMD- Anxiety item', 
                    'HAMD-21 Anxiety/Somatization Item', 
                    'HRSD - psychic anxiety item', 
                    'HRSD - somatic anxiety item', 
                    'HSC - anxiety scale', 
                    'IDS-C-30 (anxiety)', 
                    'IDS-IVR-30 (anxiety)', 
                    'MASQ - anxiety ', 
                    'NPRL - anxiety', 
                    'SCL-58 Anxiety subscale', 
                    'SCL-90 Anxiety subscale', 
                    'SHID - anxiety total subscale', 
                    'SHID - psychic anxiety', 
                    'SHID - situational anxiety', 
                    'SHID - somatic anxiety', 
                    'SRT Anxiety Subscale', 
                    'Zung SAS ')
reward.scales <- NULL
depression.scales <- c('HAM-D',
                       'MADRS')

# new df
names_new.df <- c('studlab',
                  'year',
                  'n_randomised',
                  'treatment_details',
                  'treatment',
                  'age_mean', 
                  'female_n', 
                  'dose_delivered_min', 
                  'dose_delivered_mean', 
                  'dose_delivered_max', 
                  'dose_planned_min', 
                  'dose_planned_max', 
                  'tx_duration', 
                  'study_duration',
                  'r_acceptability',
                  'r_tolerability', 
                  'anhedonia_baseline_scale', 
                  'anhedonia_baseline_direction', 
                  'anhedonia_baseline_n', 
                  'anhedonia_baseline_mean', 
                  'anhedonia_baseline_sd', 
                  'anhedonia_followup_scale', 
                  'anhedonia_followup_direction', 
                  'anhedonia_followup_n', 
                  'anhedonia_followup_mean', 
                  'anhedonia_followup_sd', 
                  'anhedonia_followup_type', 
                  'anhedonia_followup_time', 
                  'anxiety_baseline_scale', 
                  'anxiety_baseline_direction', 
                  'anxiety_baseline_n', 
                  'anxiety_baseline_mean', 
                  'anxiety_baseline_sd', 
                  'anxiety_followup_scale', 
                  'anxiety_followup_direction', 
                  'anxiety_followup_n', 
                  'anxiety_followup_mean', 
                  'anxiety_followup_sd', 
                  'anxiety_followup_type', 
                  'anxiety_followup_time', 
                  'reward_baseline_scale', 
                  'reward_baseline_direction', 
                  'reward_baseline_n', 
                  'reward_baseline_mean', 
                  'reward_baseline_sd', 
                  'reward_followup_scale', 
                  'reward_followup_direction', 
                  'reward_followup_n', 
                  'reward_followup_mean', 
                  'reward_followup_sd', 
                  'reward_followup_type', 
                  'reward_followup_time',
                  'AE')
new.df <- data.frame(matrix(ncol = length(names_new.df), nrow = 0))
colnames(new.df) <- names_new.df

# split df
for (i in unique(df$studlab)) {
  df1 <- df[df$studlab==i, ]
  if (nrow(df1) > 1) {
    if (!any(is.na(unique(df1$treatment)))) {
      for (j in unique(df1$treatment)) {
        df2 <- df1[df1$treatment==j, ]
        new.df[nrow(new.df)+1, 'studlab'] <- i
        new.df[nrow(new.df), 'year'] <- unique(df2$year)
        new.df[nrow(new.df), 'treatment'] <- j
        new.df[nrow(new.df), 'treatment_details'] <- unique(df2$treatment_details)
        new.df[nrow(new.df), 'n_randomised'] <- unique(df2$n_randomised)
        new.df[nrow(new.df), 'age_mean'] <- unique(df2$age_mean)
        new.df[nrow(new.df), 'female_n'] <- unique(df2$female_n)
        new.df[nrow(new.df), 'dose_delivered_min'] <- unique(df2$dose_delivered_min)
        new.df[nrow(new.df), 'dose_delivered_mean'] <- unique(df2$dose_delivered_mean)
        new.df[nrow(new.df), 'dose_delivered_max'] <- unique(df2$dose_delivered_max)
        new.df[nrow(new.df), 'dose_planned_min'] <- unique(df2$dose_planned_min)
        new.df[nrow(new.df), 'dose_planned_max'] <- unique(df2$dose_planned_max)
        new.df[nrow(new.df), 'tx_duration'] <- unique(df2$tx_duration)
        new.df[nrow(new.df), 'study_duration'] <- unique(df2$study_duration)
        new.df[nrow(new.df), 'r_acceptability'] <- unique(df2$r_acceptability)
        new.df[nrow(new.df), 'r_tolerability'] <- unique(df2$r_tolerability)
        new.df[nrow(new.df), 'AE'] <- unique(df2$AE)
        # anhedonia baseline
        if (length(unique(df2$c_scale[df2$week==0 & df2$c_scale %in% anhedonia.scales])) >= 1) {
          new.df[nrow(new.df), 'anhedonia_baseline_scale'] <- unique(df2$c_scale[df2$week==0 & df2$c_scale %in% anhedonia.scales])
          new.df[nrow(new.df), 'anhedonia_baseline_direction'] <- scale.compass$direction[rownames(scale.compass)==unique(df2$c_scale[df2$week==0 & df2$c_scale %in% anhedonia.scales])]
          new.df[nrow(new.df), 'anhedonia_baseline_n'] <- unique(df2$c_n[df2$week==0 & df2$c_scale %in% anhedonia.scales])
          new.df[nrow(new.df), 'anhedonia_baseline_mean'] <- unique(df2$c_mean[df2$week==0 & df2$c_scale %in% anhedonia.scales])
          new.df[nrow(new.df), 'anhedonia_baseline_sd'] <- unique(df2$c_sd[df2$week==0 & df2$c_scale %in% anhedonia.scales])
        }
        # anxiety baseline
        if (length(unique(df2$c_scale[df2$week==0 & df2$c_scale %in% anxiety.scales])) >= 1) {
          new.df[nrow(new.df), 'anxiety_baseline_scale'] <- unique(df2$c_scale[df2$week==0 & df2$c_scale %in% anxiety.scales])
          new.df[nrow(new.df), 'anxiety_baseline_direction'] <- scale.compass$direction[rownames(scale.compass)==unique(df2$c_scale[df2$week==0 & df2$c_scale %in% anxiety.scales])]
          new.df[nrow(new.df), 'anxiety_baseline_n'] <- unique(df2$c_n[df2$week==0 & df2$c_scale %in% anxiety.scales])
          new.df[nrow(new.df), 'anxiety_baseline_mean'] <- unique(df2$c_mean[df2$week==0 & df2$c_scale %in% anxiety.scales])
          new.df[nrow(new.df), 'anxiety_baseline_sd'] <- unique(df2$c_sd[df2$week==0 & df2$c_scale %in% anxiety.scales])
        }
        # reward baseline
        if (length(unique(df2$c_scale[df2$week==0 & df2$c_scale %in% reward.scales])) >= 1) {
          new.df[nrow(new.df), 'reward_baseline_scale'] <- unique(df2$c_scale[df2$week==0 & df2$c_scale %in% reward.scales])
          new.df[nrow(new.df), 'reward_baseline_direction'] <- scale.compass$direction[rownames(scale.compass)==unique(df2$c_scale[df2$week==0 & df2$c_scale %in% reward.scales])]
          new.df[nrow(new.df), 'reward_baseline_n'] <- unique(df2$c_n[df2$week==0 & df2$c_scale %in% reward.scales])
          new.df[nrow(new.df), 'reward_baseline_mean'] <- unique(df2$c_mean[df2$week==0 & df2$c_scale %in% reward.scales])
          new.df[nrow(new.df), 'reward_baseline_sd'] <- unique(df2$c_sd[df2$week==0 & df2$c_scale %in% reward.scales])
        }
        # anhedonia followup
        if (length(unique(df2$c_scale[df2$week > 0 & df2$c_scale %in% anhedonia.scales])) >= 1) {
          new.df[nrow(new.df), 'anhedonia_followup_scale'] <- unique(df2$c_scale[df2$week==max(df2$week) & df2$c_scale %in% anhedonia.scales])
          new.df[nrow(new.df), 'anhedonia_followup_direction'] <- scale.compass$direction[rownames(scale.compass)==unique(df2$c_scale[df2$week==max(df2$week) & df2$c_scale %in% anhedonia.scales])]
          new.df[nrow(new.df), 'anhedonia_followup_n'] <- unique(df2$c_n[df2$week==max(df2$week) & df2$c_scale %in% anhedonia.scales])
          new.df[nrow(new.df), 'anhedonia_followup_mean'] <- unique(df2$c_mean[df2$week==max(df2$week) & df2$c_scale %in% anhedonia.scales])
          new.df[nrow(new.df), 'anhedonia_followup_sd'] <- unique(df2$c_sd[df2$week==max(df2$week) & df2$c_scale %in% anhedonia.scales])
          new.df[nrow(new.df), 'anhedonia_followup_type'] <- unique(df2$c_type[df2$week==max(df2$week) & df2$c_scale %in% anhedonia.scales])
        }
        # anxiety followup
        if (length(unique(df2$c_scale[df2$week > 0 & df2$c_scale %in% anxiety.scales])) >= 1) {
          new.df[nrow(new.df), 'anxiety_followup_scale'] <- unique(df2$c_scale[df2$week==max(df2$week) & df2$c_scale %in% anxiety.scales])
          new.df[nrow(new.df), 'anxiety_followup_direction'] <- scale.compass$direction[rownames(scale.compass)==unique(df2$c_scale[df2$week==max(df2$week) & df2$c_scale %in% anxiety.scales])]
          new.df[nrow(new.df), 'anxiety_followup_n'] <- unique(df2$c_n[df2$week==max(df2$week) & df2$c_scale %in% anxiety.scales])
          new.df[nrow(new.df), 'anxiety_followup_mean'] <- unique(df2$c_mean[df2$week==max(df2$week) & df2$c_scale %in% anxiety.scales])
          new.df[nrow(new.df), 'anxiety_followup_sd'] <- unique(df2$c_sd[df2$week==max(df2$week) & df2$c_scale %in% anxiety.scales])
          new.df[nrow(new.df), 'anxiety_followup_type'] <- unique(df2$c_type[df2$week==max(df2$week) & df2$c_scale %in% anxiety.scales])
        }
        # reward followup
        if (length(unique(df2$c_scale[df2$week > 0 & df2$c_scale %in% reward.scales])) >= 1) {
          new.df[nrow(new.df), 'reward_followup_scale'] <- unique(df2$c_scale[df2$week==max(df2$week) & df2$c_scale %in% reward.scales])
          new.df[nrow(new.df), 'reward_followup_direction'] <- scale.compass$direction[rownames(scale.compass)==unique(df2$c_scale[df2$week==max(df2$week) & df2$c_scale %in% reward.scales])]
          new.df[nrow(new.df), 'reward_followup_n'] <- unique(df2$c_n[df2$week==max(df2$week) & df2$c_scale %in% reward.scales])
          new.df[nrow(new.df), 'reward_followup_mean'] <- unique(df2$c_mean[df2$week==max(df2$week) & df2$c_scale %in% reward.scales])
          new.df[nrow(new.df), 'reward_followup_sd'] <- unique(df2$c_sd[df2$week==max(df2$week) & df2$c_scale %in% reward.scales])
          new.df[nrow(new.df), 'reward_followup_type'] <- unique(df2$c_type[df2$week==max(df2$week) & df2$c_scale %in% reward.scales])
        }
      }
    }
  } else {
    cat(paste0('Study ', 
               df1$studlab, 
               ': single arm study.'), 
        sep = '\n')
  }
}

# remove data not usable
new.df[new.df$studlab=='Botte (1992) - 87999081', c('anxiety_followup_scale', 'anxiety_followup_n', 'anxiety_followup_mean', 'anxiety_followup_sd', 'anxiety_followup_type')] <- NA

# small amendments
new.df$reward_baseline_mean <- as.numeric(new.df$reward_baseline_mean)
new.df$tx_duration[new.df$studlab=='Parnetti (1993) - 90055317'] <- 90/7
new.df$tx_duration[new.df$studlab=='GlaxoSmithKline (1985) - 89913880'] <- 4
new.df$tx_duration <- as.numeric(new.df$tx_duration)
new.df$female_prop <- new.df$female_n/new.df$n_randomised
new.df$treatment <- ifelse(new.df$treatment=='placebo ', 'placebo', new.df$treatment)
new.df$arm <- ifelse(new.df$treatment=='placebo', 'placebo', 'active')
new.df <- new.df[with(new.df, order(studlab, arm)), ]
new.df$treatment <- gsub("^\\*", "", new.df$treatment)
new.df$treatment[new.df$treatment=='dextromethorphan- bupropion'] <- 'dextromethorphan-bupropion'
new.df$treatment_old <- new.df$treatment
new.df$treatment <- stringr::word(new.df$treatment, 1)
new.df$treatment[new.df$treatment=='minipramine'] <- 'minaprine'
new.df <- new.df[!(new.df$studlab=='GlaxoSmithKline (1985) - 89913880' & new.df$n_randomised==85), ]

# add new data Koshino 2003 on anhedonia 
## placebo
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_baseline_scale'] <- 'MADRS item 8'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_baseline_direction'] <- 'negative'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_baseline_n'] <- 186
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_baseline_mean'] <- 3.7
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_baseline_sd'] <- 1.06

new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_followup_scale'] <- 'MADRS item 8'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_followup_direction'] <- 'negative'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_followup_n'] <- 186
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_followup_mean'] <- 2
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_followup_sd'] <- 1.45
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_followup_type'] <- 'endpoint'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='placebo', 'anhedonia_followup_time'] <- '8'

## bupropion 150mg
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_baseline_scale'] <- 'MADRS item 8'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_baseline_direction'] <- 'negative'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_baseline_n'] <- 190
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_baseline_mean'] <- 3.7
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_baseline_sd'] <- 1.16

new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_followup_scale'] <- 'MADRS item 8'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_followup_direction'] <- 'negative'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_followup_n'] <- 190
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_followup_mean'] <- 2
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_followup_sd'] <- 1.45
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_followup_type'] <- 'endpoint'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==150, 'anhedonia_followup_time'] <- '8'

## bupropion 300mg
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_baseline_scale'] <- 'MADRS item 8'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_baseline_direction'] <- 'negative'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_baseline_n'] <- 188
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_baseline_mean'] <- 3.7
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_baseline_sd'] <- 1.09

new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_followup_scale'] <- 'MADRS item 8'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_followup_direction'] <- 'negative'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_followup_n'] <- 188
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_followup_mean'] <- 2.1
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_followup_sd'] <- 1.55
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_followup_type'] <- 'endpoint'
new.df[new.df$studlab=='Koshino (2013) - 87997374' & new.df$treatment=='bupropion' & new.df$dose_delivered_max==300, 'anhedonia_followup_time'] <- '8'

# implement AEs
df.ae <- xlsx::read.xlsx('data/human/LSR1 - AE df - 20231112.xlsx', sheetIndex = 1)
df.ae$identifier <- paste0(df.ae$studlab, df.ae$treatment, df.ae$dose_planned_min, df.ae$dose_planned_max)
new.df$identifier <- paste0(new.df$studlab, new.df$treatment, new.df$dose_planned_min, new.df$dose_planned_max)
new.df$nausea <- NA
new.df$headache <- NA
new.df$insomnia <- NA
new.df$dry_mouth <- NA
new.df$constipation <- NA
new.df$dizziness <- NA
new.df$vomiting <- NA
for (i in 1:nrow(df.ae)) {
  if (df.ae$identifier[i] %in% new.df$identifier) {
    new.df$nausea[new.df$identifier==df.ae$identifier[i]] <- df.ae$nausea[i]
    new.df$headache[new.df$identifier==df.ae$identifier[i]] <- df.ae$headache[i]
    new.df$insomnia[new.df$identifier==df.ae$identifier[i]] <- df.ae$insomnia[i]
    new.df$dry_mouth[new.df$identifier==df.ae$identifier[i]] <- df.ae$dry_mouth[i]
    new.df$constipation[new.df$identifier==df.ae$identifier[i]] <- df.ae$constipation[i]
    new.df$dizziness[new.df$identifier==df.ae$identifier[i]] <- df.ae$dizziness[i]
    new.df$vomiting[new.df$identifier==df.ae$identifier[i]] <- df.ae$vomiting[i]
  }
}

new.df <- dplyr::select(new.df, -identifier)

# save new.df
saveRDS(new.df, 'rds/human/df.rds')

