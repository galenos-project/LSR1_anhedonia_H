rm(list = setdiff(ls(), lsf.str()))
library(meta)
library(dplyr)
library(readxl)
library(kableExtra)

# functions ----
source('util/util for markdown.R')

# create folders ----
outcomes <- c('anhedonia', 
              'anxiety', 
              'acceptability', 
              'tolerability', 
              'constipation',
              'dizziness',
              'dry_mouth',
              'headache',
              'insomnia',
              'nausea',
              'vomiting')

# load df ----
df <- readRDS('rds/human/df.rds')

# amend df ----
source('util/df_amendments20240430.R')
df$tx_duration[df$studlab=='Ravaris (1976) - 87999576'] <- 6

# anhedonia ----
{
  df.anhedonia <- df[!is.na(df$anhedonia_followup_mean), c('studlab', 
                                                           'treatment', 
                                                           'treatment_details', 
                                                           'anhedonia_followup_n', 
                                                           'anhedonia_followup_mean', 
                                                           'anhedonia_followup_sd', 
                                                           'anhedonia_followup_type', 
                                                           'anhedonia_followup_scale', 
                                                           'anhedonia_followup_direction', 
                                                           'age_mean',
                                                           'female_prop',
                                                           'anhedonia_baseline_mean',
                                                           'anxiety_baseline_mean',
                                                           'reward_baseline_mean',
                                                           'tx_duration',
                                                           'arm')]
  df.anhedonia$anhedonia_followup_mean[df.anhedonia$anhedonia_followup_direction=='positive'] <- df.anhedonia$anhedonia_followup_mean[df.anhedonia$anhedonia_followup_direction=='positive']*-1
  colnames(df.anhedonia) <- c('studlab', 
                              'treatment', 
                              'treatment_details', 
                              'n', 
                              'mean', 
                              'sd', 
                              'anhedonia_followup_type', 
                              'anhedonia_followup_scale', 
                              'anhedonia_followup_direction', 
                              'age_mean',
                              'female_prop',
                              'anhedonia_baseline_mean',
                              'anxiety_baseline_mean',
                              'reward_baseline_mean',
                              'tx_duration',
                              'arm')
  df.anhedonia <- remove.onearm(df.anhedonia)
  df.anhedonia.merged <- collapse.arms.continuous(df.anhedonia)
  df.pw.anhedonia <- pairwise(studlab = studlab,
                                       treat = treatment,
                                       n = n,
                                       mean = mean,
                                       sd = sd,
                                       data = df.anhedonia.merged,
                                       sm = 'SMD')
  df.pw.anhedonia$age <- rowMeans(df.pw.anhedonia[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.anhedonia$female_prop <- rowMeans(df.pw.anhedonia[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.anhedonia$anhedonia_baseline <- rowMeans(df.pw.anhedonia[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.anhedonia$anxiety_baseline <- rowMeans(df.pw.anhedonia[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.anhedonia$reward_baseline <- rowMeans(df.pw.anhedonia[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  df.pw.anhedonia$colour <- ifelse(df.pw.anhedonia$treat1 %in% IMAO, 
                                   'red', 
                                   ifelse(df.pw.anhedonia$treat1 %in% RIMA,
                                          'orange',
                                          ifelse(df.pw.anhedonia$treat1 %in% DRI,
                                                 'deepskyblue',
                                                 ifelse(df.pw.anhedonia$treat1 %in% agonist,
                                                        'purple',
                                                        ifelse(df.pw.anhedonia$treat1 %in% vescicular,
                                                               'green4',
                                                               'grey'
                                                        )))))
  pwma.anhedonia <- metacont(n.e = n1, 
                             mean.e = mean1, 
                             sd.e = sd1, 
                             n.c = n2, 
                             mean.c = mean2, 
                             sd.c = sd2, 
                             studlab = studlab, 
                             data = df.pw.anhedonia, 
                             sm = 'SMD', 
                             method.smd = 'Hedges', 
                             comb.fixed = F, 
                             comb.random = T, 
                             title = 'Anhedonia',
                             prediction = T, 
                             hakn = T, 
                             method.tau = 'REML')
}

# anxiety ----
{
  df.anxiety <- df[!is.na(df$anxiety_followup_mean), c('studlab', 
                                                       'treatment', 
                                                       'treatment_details', 
                                                       'anxiety_followup_n', 
                                                       'anxiety_followup_mean', 
                                                       'anxiety_followup_sd', 
                                                       'anxiety_followup_type', 
                                                       'anxiety_followup_scale', 
                                                       'anxiety_followup_direction', 
                                                       'age_mean',
                                                       'female_prop',
                                                       'anhedonia_baseline_mean',
                                                       'anxiety_baseline_mean',
                                                       'reward_baseline_mean',
                                                       'tx_duration',
                                                       'arm')]
  df.anxiety$anxiety_followup_mean[df.anxiety$anxiety_followup_direction=='positive'] <- df.anxiety$anxiety_followup_mean[df.anxiety$anxiety_followup_direction=='positive']*-1
  colnames(df.anxiety) <- c('studlab', 
                            'treatment', 
                            'treatment_details', 
                            'n', 
                            'mean', 
                            'sd', 
                            'anxiety_followup_type', 
                            'anxiety_followup_scale', 
                            'anxiety_followup_direction',
                            'age_mean',
                            'female_prop',
                            'anhedonia_baseline_mean',
                            'anxiety_baseline_mean',
                            'reward_baseline_mean',
                            'tx_duration',
                            'arm')
  df.anxiety <- remove.onearm(df.anxiety)
  df.anxiety.merged <- collapse.arms.continuous(df.anxiety)
  df.pw.anxiety <- pairwise(studlab = studlab,
                                     treat = treatment,
                                     n = n,
                                     mean = mean,
                                     sd = sd,
                                     data = df.anxiety.merged,
                                     sm = 'SMD')
  df.pw.anxiety$age <- rowMeans(df.pw.anxiety[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.anxiety$age_reg <- (df.pw.anxiety$age - min(df.pw.anxiety$age, na.rm = T))/10
  df.pw.anxiety$tx_duration_reg <- 
    df.pw.anxiety$tx_duration - min(df.pw.anxiety$tx_duration, na.rm = T)
  df.pw.anxiety$female_prop <- rowMeans(df.pw.anxiety[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.anxiety$anhedonia_baseline <- rowMeans(df.pw.anxiety[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.anxiety$anxiety_baseline <- rowMeans(df.pw.anxiety[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.anxiety$reward_baseline <- rowMeans(df.pw.anxiety[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  
  df.pw.anxiety$colour <- ifelse(df.pw.anxiety$treat1 %in% IMAO, 
                                 'red', 
                                 ifelse(df.pw.anxiety$treat1 %in% RIMA,
                                        'orange',
                                        ifelse(df.pw.anxiety$treat1 %in% DRI,
                                               'deepskyblue',
                                               ifelse(df.pw.anxiety$treat1 %in% agonist,
                                                      'purple',
                                                      ifelse(df.pw.anxiety$treat1 %in% vescicular,
                                                             'green4',
                                                             'grey'
                                                      )))))
  df.pw.anxiety$group <- ifelse(df.pw.anxiety$colour=='red',
                                'MAOI',
                                ifelse(df.pw.anxiety$colour=='orange',
                                       'RIMA',
                                       ifelse(df.pw.anxiety$colour=='deepskyblue',
                                              '*DRI',
                                              ifelse(df.pw.anxiety$colour=='purple',
                                                     'Agonist',
                                                     ifelse(df.pw.anxiety$colour=='green4',
                                                            'Vesicle transport facilitator',
                                                            'Other')))))
  
  pwma.anxiety <- metacont(n.e = n1, 
                           mean.e = mean1, 
                           sd.e = sd1, 
                           n.c = n2, 
                           mean.c = mean2, 
                           sd.c = sd2, 
                           studlab = studlab, 
                           data = df.pw.anxiety, 
                           sm = 'SMD', 
                           method.smd = 'Hedges', 
                           comb.fixed = F, 
                           comb.random = T, 
                           title = 'Anxiety',
                           prediction = T, 
                           hakn = T, 
                           method.tau = 'REML',
                           subgroup = group)
  
  df.pw.anxiety2 <- df.pw.anxiety
  df.pw.anxiety2 <- df.pw.anxiety2[!is.na(df.pw.anxiety2$TE), ]
  
  pwma.anxiety2 <- metacont(n.e = n1, 
                            mean.e = mean1, 
                            sd.e = sd1, 
                            n.c = n2, 
                            mean.c = mean2, 
                            sd.c = sd2, 
                            studlab = studlab, 
                            data = df.pw.anxiety2, 
                            sm = 'SMD', 
                            method.smd = 'Hedges', 
                            common = F, 
                            random = T, 
                            title = 'Anxiety',
                            prediction = T, 
                            hakn = T, 
                            method.tau = 'REML',
                            subgroup = group)
}

# acceptability ----
{
  df.acc <- df[!is.na(df$r_acceptability), c('studlab', 
                                             'treatment', 
                                             'treatment_details', 
                                             'n_randomised', 
                                             'r_acceptability',
                                             'age_mean',
                                             'female_prop',
                                             'anhedonia_baseline_mean',
                                             'anxiety_baseline_mean',
                                             'reward_baseline_mean',
                                             'tx_duration',
                                             'arm')]
  colnames(df.acc) <- c('studlab', 
                        'treatment', 
                        'treatment_details', 
                        'n', 
                        'r',
                        'age_mean',
                        'female_prop',
                        'anhedonia_baseline_mean',
                        'anxiety_baseline_mean',
                        'reward_baseline_mean',
                        'tx_duration',
                        'arm')
  df.acc$treatment_details <- tolower(df.acc$treatment_details)
  df.acc$treatment_details <- ifelse(df.acc$treatment_details=='placebo', 'placebo', 'active')
  df.acc <- df.acc[-(is.na(df.acc$n)), ]
  df.acc <- remove.onearm(df.acc)
  df.acc.merged <- collapse.arms.binary(df.acc)
  df.pw.acc <- pairwise(studlab = studlab,
                                 treat = treatment,
                                 n = n,
                                 event = r,
                                 data = df.acc.merged,
                                 sm = 'OR')
  df.pw.acc$age <- rowMeans(df.pw.acc[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.acc$age_reg <- (df.pw.acc$age - min(df.pw.acc$age, na.rm = T))/10
  df.pw.acc$tx_duration_reg <- 
    df.pw.acc$tx_duration - min(df.pw.acc$tx_duration, na.rm = T)  
  df.pw.acc$female_prop <- rowMeans(df.pw.acc[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.acc$anhedonia_baseline <- rowMeans(df.pw.acc[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.acc$anxiety_baseline <- rowMeans(df.pw.acc[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.acc$reward_baseline <- rowMeans(df.pw.acc[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  
  df.pw.acc$colour <- ifelse(df.pw.acc$treat1 %in% IMAO, 
                             'red', 
                             ifelse(df.pw.acc$treat1 %in% RIMA,
                                    'orange',
                                    ifelse(df.pw.acc$treat1 %in% DRI,
                                           'deepskyblue',
                                           ifelse(df.pw.acc$treat1 %in% agonist,
                                                  'purple',
                                                  ifelse(df.pw.acc$treat1 %in% vescicular,
                                                         'green4',
                                                         'grey'
                                                  )))))
  df.pw.acc$group <- ifelse(df.pw.acc$colour=='red',
                            'MAOI',
                            ifelse(df.pw.acc$colour=='orange',
                                   'RIMA',
                                   ifelse(df.pw.acc$colour=='deepskyblue',
                                          '*DRI',
                                          ifelse(df.pw.acc$colour=='purple',
                                                 'Agonist',
                                                 ifelse(df.pw.acc$colour=='green4',
                                                        'Vesicle transport facilitator',
                                                        'Other')))))
  pwma.acc <- metabin(n.e = n1, 
                      event.e = event1, 
                      n.c = n2, 
                      event.c = event2, 
                      studlab = studlab, 
                      data = df.pw.acc, 
                      sm = 'OR', 
                      comb.fixed = F, 
                      comb.random = T, 
                      title = 'Acceptability',
                      prediction = T, 
                      hakn = T, 
                      method.tau = 'REML',
                      subgroup = group)

## EER ----
acc.cer.pw <- metaprop(event = r, 
                       n = n, 
                       studlab = studlab, 
                       data = df.acc.merged[df.acc.merged$treatment=='placebo' & (!is.na(df.acc.merged$n)),], 
                       backtransf = T, 
                       comb.fixed = F, 
                       comb.random = T, 
                       hakn = T, 
                       method = 'GLMM')
acc.rate.pla <- exp(acc.cer.pw$TE.random)/(1+exp(acc.cer.pw$TE.random))
acc.odds.pla <- acc.rate.pla/(1-acc.rate.pla)
acc.TE <- exp(pwma.acc$TE.random)
acc.eer <- round(acc.TE*acc.odds.pla/(1+acc.TE*acc.odds.pla), digits = 3)
acc.eer
}

# tolerability ----
{
  df.tol <- df[!is.na(df$r_tolerability), c('studlab', 
                                            'treatment', 
                                            'treatment_details', 
                                            'n_randomised', 
                                            'r_tolerability',
                                            'age_mean',
                                            'female_prop',
                                            'anhedonia_baseline_mean',
                                            'anxiety_baseline_mean',
                                            'reward_baseline_mean',
                                            'tx_duration',
                                            'arm')]
  colnames(df.tol) <- c('studlab', 
                        'treatment', 
                        'treatment_details', 
                        'n', 
                        'r',
                        'age_mean',
                        'female_prop',
                        'anhedonia_baseline_mean',
                        'anxiety_baseline_mean',
                        'reward_baseline_mean',
                        'tx_duration',
                        'arm')
  df.tol$treatment_details <- tolower(df.tol$treatment_details)
  df.tol$treatment_details <- ifelse(df.tol$treatment_details=='placebo', 'placebo', 'active')
  df.tol <- remove.onearm(df.tol)
  df.tol.merged <- collapse.arms.binary(df.tol)
  df.pw.tol <- pairwise(studlab = studlab,
                                 treat = treatment,
                                 n = n,
                                 event = r,
                                 data = df.tol.merged,
                                 sm = 'OR')
  df.pw.tol$age <- rowMeans(df.pw.tol[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.tol$age_reg <- (df.pw.tol$age - min(df.pw.tol$age, na.rm = T))/10
  df.pw.tol$tx_duration_reg <- df.pw.tol$tx_duration - min(df.pw.tol$tx_duration, na.rm = T)  
  df.pw.tol$female_prop <- rowMeans(df.pw.tol[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.tol$anhedonia_baseline <- rowMeans(df.pw.tol[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.tol$anxiety_baseline <- rowMeans(df.pw.tol[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.tol$reward_baseline <- rowMeans(df.pw.tol[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  df.pw.tol$colour <- ifelse(df.pw.tol$treat1 %in% IMAO, 
                             'red', 
                             ifelse(df.pw.tol$treat1 %in% RIMA,
                                    'orange',
                                    ifelse(df.pw.tol$treat1 %in% DRI,
                                           'deepskyblue',
                                           ifelse(df.pw.tol$treat1 %in% agonist,
                                                  'purple',
                                                  ifelse(df.pw.tol$treat1 %in% vescicular,
                                                         'green4',
                                                         'grey'
                                                  )))))
  df.pw.tol$group <- ifelse(df.pw.tol$colour=='red',
                            'MAOI',
                            ifelse(df.pw.tol$colour=='orange',
                                   'RIMA',
                                   ifelse(df.pw.tol$colour=='deepskyblue',
                                          '*DRI',
                                          ifelse(df.pw.tol$colour=='purple',
                                                 'Agonist',
                                                 ifelse(df.pw.tol$colour=='green4',
                                                        'Vesicle transport facilitator',
                                                        'Other')))))
  pwma.tol <- metabin(n.e = n1, 
                      event.e = event1, 
                      n.c = n2, 
                      event.c = event2, 
                      studlab = studlab, 
                      data = df.pw.tol, 
                      sm = 'OR', 
                      comb.fixed = F, 
                      comb.random = T, 
                      title = 'tolerability',
                      prediction = T, 
                      hakn = T, 
                      method.tau = 'REML',
                      subgroup = group)
  
  ## EER ----
  tol.cer.pw <- metaprop(event = r, 
                         n = n, 
                         studlab = studlab, 
                         data = df.tol.merged[df.tol.merged$treatment=='placebo',], 
                         backtransf = T, 
                         comb.fixed = F, 
                         comb.random = T, 
                         hakn = T, 
                         method = 'GLMM')
  tol.rate.pla <- exp(tol.cer.pw$TE.random)/(1+exp(tol.cer.pw$TE.random))
  tol.odds.pla <- tol.rate.pla/(1-tol.rate.pla)
  tol.TE <- exp(pwma.tol$TE.random)
  tol.eer <- round(tol.TE*tol.odds.pla/(1+tol.TE*tol.odds.pla), digits = 3)
  tol.eer
}

# nausea ----
{
  df.nausea <- df[!is.na(df$nausea), c('studlab', 
                                       'treatment', 
                                       'treatment_details', 
                                       'n_randomised', 
                                       'nausea',
                                       'age_mean',
                                       'female_prop',
                                       'anhedonia_baseline_mean',
                                       'anxiety_baseline_mean',
                                       'reward_baseline_mean',
                                       'tx_duration',
                                       'arm')]
  colnames(df.nausea) <- c('studlab', 
                           'treatment', 
                           'treatment_details', 
                           'n', 
                           'r',
                           'age_mean',
                           'female_prop',
                           'anhedonia_baseline_mean',
                           'anxiety_baseline_mean',
                           'reward_baseline_mean',
                           'tx_duration',
                           'arm')
  df.nausea$treatment_details <- tolower(df.nausea$treatment_details)
  df.nausea$treatment_details <- ifelse(df.nausea$treatment_details=='placebo', 'placebo', 'active')
  df.nausea <- remove.onearm(df.nausea)
  df.nausea.merged <- collapse.arms.binary(df.nausea)
  df.pw.nausea <- pairwise(studlab = studlab,
                                    treat = treatment,
                                    n = n,
                                    event = r,
                                    data = df.nausea.merged,
                                    sm = 'OR')
  df.pw.nausea$age <- rowMeans(df.pw.nausea[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.nausea$age_reg <- (df.pw.nausea$age - min(df.pw.nausea$age, na.rm = T))/10
  df.pw.nausea$tx_duration_reg <- df.pw.nausea$tx_duration - min(df.pw.nausea$tx_duration, na.rm = T)  
  df.pw.nausea$female_prop <- rowMeans(df.pw.nausea[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.nausea$anhedonia_baseline <- rowMeans(df.pw.nausea[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.nausea$anxiety_baseline <- rowMeans(df.pw.nausea[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.nausea$reward_baseline <- rowMeans(df.pw.nausea[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  df.pw.nausea$colour <- ifelse(df.pw.nausea$treat1 %in% IMAO, 
                                'red', 
                                ifelse(df.pw.nausea$treat1 %in% RIMA,
                                       'orange',
                                       ifelse(df.pw.nausea$treat1 %in% DRI,
                                              'deepskyblue',
                                              ifelse(df.pw.nausea$treat1 %in% agonist,
                                                     'purple',
                                                     ifelse(df.pw.nausea$treat1 %in% vescicular,
                                                            'green4',
                                                            'grey'
                                                     )))))
  df.pw.nausea$group <- ifelse(df.pw.nausea$colour=='red',
                               'MAOI',
                               ifelse(df.pw.nausea$colour=='orange',
                                      'RIMA',
                                      ifelse(df.pw.nausea$colour=='deepskyblue',
                                             '*DRI',
                                             ifelse(df.pw.nausea$colour=='purple',
                                                    'Agonist',
                                                    ifelse(df.pw.nausea$colour=='green4',
                                                           'Vesicle transport facilitator',
                                                           'Other')))))
  pwma.nausea <- metabin(n.e = n1, 
                         event.e = event1, 
                         n.c = n2, 
                         event.c = event2, 
                         studlab = studlab, 
                         data = df.pw.nausea, 
                         sm = 'OR', 
                         comb.fixed = F, 
                         comb.random = T, 
                         title = 'Nausea',
                         prediction = T, 
                         hakn = T, 
                         method.tau = 'REML',
                         subgroup = group)

## EER ----
nausea.cer.pw <- metaprop(event = r, 
                       n = n, 
                       studlab = studlab, 
                       data = df.nausea.merged[df.nausea.merged$treatment=='placebo',], 
                       backtransf = T, 
                       comb.fixed = F, 
                       comb.random = T, 
                       hakn = T, 
                       method = 'GLMM')
nausea.rate.pla <- exp(nausea.cer.pw$TE.random)/(1+exp(nausea.cer.pw$TE.random))
nausea.odds.pla <- nausea.rate.pla/(1-nausea.rate.pla)
nausea.TE <- exp(pwma.nausea$TE.random)
nausea.eer <- round(nausea.TE*nausea.odds.pla/(1+nausea.TE*nausea.odds.pla), digits = 3)
nausea.eer
}

# headache ----
{
  df.headache <- df[!is.na(df$headache), c('studlab', 
                                           'treatment', 
                                           'treatment_details', 
                                           'n_randomised', 
                                           'headache',
                                           'age_mean',
                                           'female_prop',
                                           'anhedonia_baseline_mean',
                                           'anxiety_baseline_mean',
                                           'reward_baseline_mean',
                                           'tx_duration',
                                           'arm')]
  colnames(df.headache) <- c('studlab', 
                             'treatment', 
                             'treatment_details', 
                             'n', 
                             'r',
                             'age_mean',
                             'female_prop',
                             'anhedonia_baseline_mean',
                             'anxiety_baseline_mean',
                             'reward_baseline_mean',
                             'tx_duration',
                             'arm')
  df.headache$treatment_details <- tolower(df.headache$treatment_details)
  df.headache$treatment_details <- ifelse(df.headache$treatment_details=='placebo', 'placebo', 'active')
  df.headache <- remove.onearm(df.headache)
  df.headache.merged <- collapse.arms.binary(df.headache)
  df.pw.headache <- pairwise(studlab = studlab,
                                      treat = treatment,
                                      n = n,
                                      event = r,
                                      data = df.headache.merged,
                                      sm = 'OR')
  df.pw.headache$age <- rowMeans(df.pw.headache[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.headache$age_reg <- (df.pw.headache$age - min(df.pw.headache$age, na.rm = T))/10
  df.pw.headache$tx_duration_reg <- df.pw.headache$tx_duration - min(df.pw.headache$tx_duration, na.rm = T)  
  df.pw.headache$female_prop <- rowMeans(df.pw.headache[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.headache$anhedonia_baseline <- rowMeans(df.pw.headache[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.headache$anxiety_baseline <- rowMeans(df.pw.headache[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.headache$reward_baseline <- rowMeans(df.pw.headache[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  df.pw.headache$colour <- ifelse(df.pw.headache$treat1 %in% IMAO, 
                                  'red', 
                                  ifelse(df.pw.headache$treat1 %in% RIMA,
                                         'orange',
                                         ifelse(df.pw.headache$treat1 %in% DRI,
                                                'deepskyblue',
                                                ifelse(df.pw.headache$treat1 %in% agonist,
                                                       'purple',
                                                       ifelse(df.pw.headache$treat1 %in% vescicular,
                                                              'green4',
                                                              'grey'
                                                       )))))
  df.pw.headache$group <- ifelse(df.pw.headache$colour=='red',
                                 'MAOI',
                                 ifelse(df.pw.headache$colour=='orange',
                                        'RIMA',
                                        ifelse(df.pw.headache$colour=='deepskyblue',
                                               '*DRI',
                                               ifelse(df.pw.headache$colour=='purple',
                                                      'Agonist',
                                                      ifelse(df.pw.headache$colour=='green4',
                                                             'Vesicle transport facilitator',
                                                             'Other')))))
  pwma.headache <- metabin(n.e = n1, 
                           event.e = event1, 
                           n.c = n2, 
                           event.c = event2, 
                           studlab = studlab, 
                           data = df.pw.headache, 
                           sm = 'OR', 
                           comb.fixed = F, 
                           comb.random = T, 
                           title = 'headache',
                           prediction = T, 
                           hakn = T, 
                           method.tau = 'REML',
                           subgroup = group)
  
  ## EER ----
  headache.cer.pw <- metaprop(event = r, 
                              n = n, 
                              studlab = studlab, 
                              data = df.headache.merged[df.headache.merged$treatment=='placebo',], 
                              backtransf = T, 
                              comb.fixed = F, 
                              comb.random = T, 
                              hakn = T, 
                              method = 'GLMM')
  headache.rate.pla <- exp(headache.cer.pw$TE.random)/(1+exp(headache.cer.pw$TE.random))
  headache.odds.pla <- headache.rate.pla/(1-headache.rate.pla)
  headache.TE <- exp(pwma.headache$TE.random)
  headache.eer <- round(headache.TE*headache.odds.pla/(1+headache.TE*headache.odds.pla), digits = 3)
  headache.eer
}

# insomnia ----
{
  df.insomnia <- df[!is.na(df$insomnia), c('studlab', 
                                           'treatment', 
                                           'treatment_details', 
                                           'n_randomised', 
                                           'insomnia',
                                           'age_mean',
                                           'female_prop',
                                           'anhedonia_baseline_mean',
                                           'anxiety_baseline_mean',
                                           'reward_baseline_mean',
                                           'tx_duration',
                                           'arm')]
  colnames(df.insomnia) <- c('studlab', 
                             'treatment', 
                             'treatment_details', 
                             'n', 
                             'r',
                             'age_mean',
                             'female_prop',
                             'anhedonia_baseline_mean',
                             'anxiety_baseline_mean',
                             'reward_baseline_mean',
                             'tx_duration',
                             'arm')
  df.insomnia$treatment_details <- tolower(df.insomnia$treatment_details)
  df.insomnia$treatment_details <- ifelse(df.insomnia$treatment_details=='placebo', 'placebo', 'active')
  df.insomnia <- remove.onearm(df.insomnia)
  df.insomnia.merged <- collapse.arms.binary(df.insomnia)
  df.pw.insomnia <- pairwise(studlab = studlab,
                                      treat = treatment,
                                      n = n,
                                      event = r,
                                      data = df.insomnia.merged,
                                      sm = 'OR')
  df.pw.insomnia$age <- rowMeans(df.pw.insomnia[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.insomnia$age_reg <- (df.pw.insomnia$age - min(df.pw.insomnia$age, na.rm = T))/10
  df.pw.insomnia$tx_duration_reg <- df.pw.insomnia$tx_duration - min(df.pw.insomnia$tx_duration, na.rm = T)  
  df.pw.insomnia$female_prop <- rowMeans(df.pw.insomnia[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.insomnia$anhedonia_baseline <- rowMeans(df.pw.insomnia[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.insomnia$anxiety_baseline <- rowMeans(df.pw.insomnia[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.insomnia$reward_baseline <- rowMeans(df.pw.insomnia[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  df.pw.insomnia$colour <- ifelse(df.pw.insomnia$treat1 %in% IMAO, 
                                  'red', 
                                  ifelse(df.pw.insomnia$treat1 %in% RIMA,
                                         'orange',
                                         ifelse(df.pw.insomnia$treat1 %in% DRI,
                                                'deepskyblue',
                                                ifelse(df.pw.insomnia$treat1 %in% agonist,
                                                       'purple',
                                                       ifelse(df.pw.insomnia$treat1 %in% vescicular,
                                                              'green4',
                                                              'grey'
                                                       )))))
  df.pw.insomnia$group <- ifelse(df.pw.insomnia$colour=='red',
                                 'MAOI',
                                 ifelse(df.pw.insomnia$colour=='orange',
                                        'RIMA',
                                        ifelse(df.pw.insomnia$colour=='deepskyblue',
                                               '*DRI',
                                               ifelse(df.pw.insomnia$colour=='purple',
                                                      'Agonist',
                                                      ifelse(df.pw.insomnia$colour=='green4',
                                                             'Vesicle transport facilitator',
                                                             'Other')))))
  pwma.insomnia <- metabin(n.e = n1, 
                           event.e = event1, 
                           n.c = n2, 
                           event.c = event2, 
                           studlab = studlab, 
                           data = df.pw.insomnia, 
                           sm = 'OR', 
                           comb.fixed = F, 
                           comb.random = T, 
                           title = 'insomnia',
                           prediction = T, 
                           hakn = T, 
                           method.tau = 'REML',
                           subgroup = group)

## EER ----
insomnia.cer.pw <- metaprop(event = r, 
                          n = n, 
                          studlab = studlab, 
                          data = df.insomnia.merged[df.insomnia.merged$treatment=='placebo',], 
                          backtransf = T, 
                          comb.fixed = F, 
                          comb.random = T, 
                          hakn = T, 
                          method = 'GLMM')
insomnia.rate.pla <- exp(insomnia.cer.pw$TE.random)/(1+exp(insomnia.cer.pw$TE.random))
insomnia.odds.pla <- insomnia.rate.pla/(1-insomnia.rate.pla)
insomnia.TE <- exp(pwma.insomnia$TE.random)
insomnia.eer <- round(insomnia.TE*insomnia.odds.pla/(1+insomnia.TE*insomnia.odds.pla), digits = 3)
insomnia.eer
}

# dry_mouth ----
{
  df.dry_mouth <- df[!is.na(df$dry_mouth), c('studlab', 
                                             'treatment', 
                                             'treatment_details', 
                                             'n_randomised', 
                                             'dry_mouth',
                                             'age_mean',
                                             'female_prop',
                                             'anhedonia_baseline_mean',
                                             'anxiety_baseline_mean',
                                             'reward_baseline_mean',
                                             'tx_duration',
                                             'arm')]
  colnames(df.dry_mouth) <- c('studlab', 
                              'treatment', 
                              'treatment_details', 
                              'n', 
                              'r',
                              'age_mean',
                              'female_prop',
                              'anhedonia_baseline_mean',
                              'anxiety_baseline_mean',
                              'reward_baseline_mean',
                              'tx_duration',
                              'arm')
  df.dry_mouth$treatment_details <- tolower(df.dry_mouth$treatment_details)
  df.dry_mouth$treatment_details <- ifelse(df.dry_mouth$treatment_details=='placebo', 'placebo', 'active')
  df.dry_mouth <- remove.onearm(df.dry_mouth)
  df.dry_mouth.merged <- collapse.arms.binary(df.dry_mouth)
  df.pw.dry_mouth <- pairwise(studlab = studlab,
                                       treat = treatment,
                                       n = n,
                                       event = r,
                                       data = df.dry_mouth.merged,
                                       sm = 'OR')
  df.pw.dry_mouth$age <- rowMeans(df.pw.dry_mouth[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.dry_mouth$age_reg <- (df.pw.dry_mouth$age - min(df.pw.dry_mouth$age, na.rm = T))/10
  df.pw.dry_mouth$tx_duration_reg <- 
    df.pw.dry_mouth$tx_duration - min(df.pw.dry_mouth$tx_duration, na.rm = T)  
  df.pw.dry_mouth$female_prop <- rowMeans(df.pw.dry_mouth[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.dry_mouth$anhedonia_baseline <- rowMeans(df.pw.dry_mouth[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.dry_mouth$anxiety_baseline <- rowMeans(df.pw.dry_mouth[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.dry_mouth$reward_baseline <- rowMeans(df.pw.dry_mouth[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  df.pw.dry_mouth$colour <- ifelse(df.pw.dry_mouth$treat1 %in% IMAO, 
                                   'red', 
                                   ifelse(df.pw.dry_mouth$treat1 %in% RIMA,
                                          'orange',
                                          ifelse(df.pw.dry_mouth$treat1 %in% DRI,
                                                 'deepskyblue',
                                                 ifelse(df.pw.dry_mouth$treat1 %in% agonist,
                                                        'purple',
                                                        ifelse(df.pw.dry_mouth$treat1 %in% vescicular,
                                                               'green4',
                                                               'grey'
                                                        )))))
  df.pw.dry_mouth$group <- ifelse(df.pw.dry_mouth$colour=='red',
                                  'MAOI',
                                  ifelse(df.pw.dry_mouth$colour=='orange',
                                         'RIMA',
                                         ifelse(df.pw.dry_mouth$colour=='deepskyblue',
                                                '*DRI',
                                                ifelse(df.pw.dry_mouth$colour=='purple',
                                                       'Agonist',
                                                       ifelse(df.pw.dry_mouth$colour=='green4',
                                                              'Vesicle transport facilitator',
                                                              'Other')))))
  pwma.dry_mouth <- metabin(n.e = n1, 
                            event.e = event1, 
                            n.c = n2, 
                            event.c = event2, 
                            studlab = studlab, 
                            data = df.pw.dry_mouth, 
                            sm = 'OR', 
                            comb.fixed = F, 
                            comb.random = T, 
                            title = 'dry_mouth',
                            prediction = T, 
                            hakn = T, 
                            method.tau = 'REML',
                            subgroup = group)

## EER ----
dry_mouth.cer.pw <- metaprop(event = r, 
                          n = n, 
                          studlab = studlab, 
                          data = df.dry_mouth.merged[df.dry_mouth.merged$treatment=='placebo',], 
                          backtransf = T, 
                          comb.fixed = F, 
                          comb.random = T, 
                          hakn = T, 
                          method = 'GLMM')
dry_mouth.rate.pla <- exp(dry_mouth.cer.pw$TE.random)/(1+exp(dry_mouth.cer.pw$TE.random))
dry_mouth.odds.pla <- dry_mouth.rate.pla/(1-dry_mouth.rate.pla)
dry_mouth.TE <- exp(pwma.dry_mouth$TE.random)
dry_mouth.eer <- round(dry_mouth.TE*dry_mouth.odds.pla/(1+dry_mouth.TE*dry_mouth.odds.pla), digits = 3)
dry_mouth.eer
}

# constipation ----
{
  df.constipation <- df[!is.na(df$constipation), c('studlab', 
                                                   'treatment', 
                                                   'treatment_details', 
                                                   'n_randomised', 
                                                   'constipation',
                                                   'age_mean',
                                                   'female_prop',
                                                   'anhedonia_baseline_mean',
                                                   'anxiety_baseline_mean',
                                                   'reward_baseline_mean',
                                                   'tx_duration',
                                                   'arm')]
  colnames(df.constipation) <- c('studlab', 
                                 'treatment', 
                                 'treatment_details', 
                                 'n', 
                                 'r',
                                 'age_mean',
                                 'female_prop',
                                 'anhedonia_baseline_mean',
                                 'anxiety_baseline_mean',
                                 'reward_baseline_mean',
                                 'tx_duration',
                                 'arm')
  df.constipation$treatment_details <- tolower(df.constipation$treatment_details)
  df.constipation$treatment_details <- ifelse(df.constipation$treatment_details=='placebo', 'placebo', 'active')
  df.constipation <- remove.onearm(df.constipation)
  df.constipation.merged <- collapse.arms.binary(df.constipation)
  df.pw.constipation <- pairwise(studlab = studlab,
                                          treat = treatment,
                                          n = n,
                                          event = r,
                                          data = df.constipation.merged,
                                          sm = 'OR')
  df.pw.constipation$age <- rowMeans(df.pw.constipation[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.constipation$age_reg <- (df.pw.constipation$age - min(df.pw.constipation$age, na.rm = T))/10
  df.pw.constipation$tx_duration_reg <- 
    df.pw.constipation$tx_duration - min(df.pw.constipation$tx_duration, na.rm = T)  
  df.pw.constipation$female_prop <- rowMeans(df.pw.constipation[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.constipation$anhedonia_baseline <- rowMeans(df.pw.constipation[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.constipation$anxiety_baseline <- rowMeans(df.pw.constipation[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.constipation$reward_baseline <- rowMeans(df.pw.constipation[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  df.pw.constipation$colour <- ifelse(df.pw.constipation$treat1 %in% IMAO, 
                                      'red', 
                                      ifelse(df.pw.constipation$treat1 %in% RIMA,
                                             'orange',
                                             ifelse(df.pw.constipation$treat1 %in% DRI,
                                                    'deepskyblue',
                                                    ifelse(df.pw.constipation$treat1 %in% agonist,
                                                           'purple',
                                                           ifelse(df.pw.constipation$treat1 %in% vescicular,
                                                                  'green4',
                                                                  'grey'
                                                           )))))
  df.pw.constipation$group <- ifelse(df.pw.constipation$colour=='red',
                                     'MAOI',
                                     ifelse(df.pw.constipation$colour=='orange',
                                            'RIMA',
                                            ifelse(df.pw.constipation$colour=='deepskyblue',
                                                   '*DRI',
                                                   ifelse(df.pw.constipation$colour=='purple',
                                                          'Agonist',
                                                          ifelse(df.pw.constipation$colour=='green4',
                                                                 'Vesicle transport facilitator',
                                                                 'Other')))))
  pwma.constipation <- metabin(n.e = n1, 
                               event.e = event1, 
                               n.c = n2, 
                               event.c = event2, 
                               studlab = studlab, 
                               data = df.pw.constipation, 
                               sm = 'OR', 
                               comb.fixed = F, 
                               comb.random = T, 
                               title = 'constipation',
                               prediction = T, 
                               hakn = T, 
                               method.tau = 'REML',
                               subgroup = group)

## EER ----
constipation.cer.pw <- metaprop(event = r, 
                          n = n, 
                          studlab = studlab, 
                          data = df.constipation.merged[df.constipation.merged$treatment=='placebo',], 
                          backtransf = T, 
                          comb.fixed = F, 
                          comb.random = T, 
                          hakn = T, 
                          method = 'GLMM')
constipation.rate.pla <- exp(constipation.cer.pw$TE.random)/(1+exp(constipation.cer.pw$TE.random))
constipation.odds.pla <- constipation.rate.pla/(1-constipation.rate.pla)
constipation.TE <- exp(pwma.constipation$TE.random)
constipation.eer <- round(constipation.TE*constipation.odds.pla/(1+constipation.TE*constipation.odds.pla), digits = 3)
constipation.eer
}

# dizziness ----
{
  df.dizziness <- df[!is.na(df$dizziness), c('studlab', 
                                             'treatment', 
                                             'treatment_details', 
                                             'n_randomised', 
                                             'dizziness',
                                             'age_mean',
                                             'female_prop',
                                             'anhedonia_baseline_mean',
                                             'anxiety_baseline_mean',
                                             'reward_baseline_mean',
                                             'tx_duration',
                                             'arm')]
  colnames(df.dizziness) <- c('studlab', 
                              'treatment', 
                              'treatment_details', 
                              'n', 
                              'r',
                              'age_mean',
                              'female_prop',
                              'anhedonia_baseline_mean',
                              'anxiety_baseline_mean',
                              'reward_baseline_mean',
                              'tx_duration',
                              'arm')
  df.dizziness$treatment_details <- tolower(df.dizziness$treatment_details)
  df.dizziness$treatment_details <- ifelse(df.dizziness$treatment_details=='placebo', 'placebo', 'active')
  df.dizziness <- remove.onearm(df.dizziness)
  df.dizziness.merged <- collapse.arms.binary(df.dizziness)
  df.pw.dizziness <- pairwise(studlab = studlab,
                                       treat = treatment,
                                       n = n,
                                       event = r,
                                       data = df.dizziness.merged,
                                       sm = 'OR')
  df.pw.dizziness$age <- rowMeans(df.pw.dizziness[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.dizziness$age_reg <- (df.pw.dizziness$age - min(df.pw.dizziness$age, na.rm = T))/10
  df.pw.dizziness$tx_duration_reg <- 
    df.pw.dizziness$tx_duration - min(df.pw.dizziness$tx_duration, na.rm = T)  
  df.pw.dizziness$female_prop <- rowMeans(df.pw.dizziness[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.dizziness$anhedonia_baseline <- rowMeans(df.pw.dizziness[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.dizziness$anxiety_baseline <- rowMeans(df.pw.dizziness[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.dizziness$reward_baseline <- rowMeans(df.pw.dizziness[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  df.pw.dizziness$colour <- ifelse(df.pw.dizziness$treat1 %in% IMAO, 
                                   'red', 
                                   ifelse(df.pw.dizziness$treat1 %in% RIMA,
                                          'orange',
                                          ifelse(df.pw.dizziness$treat1 %in% DRI,
                                                 'deepskyblue',
                                                 ifelse(df.pw.dizziness$treat1 %in% agonist,
                                                        'purple',
                                                        ifelse(df.pw.dizziness$treat1 %in% vescicular,
                                                               'green4',
                                                               'grey'
                                                        )))))
  df.pw.dizziness$group <- ifelse(df.pw.dizziness$colour=='red',
                                  'MAOI',
                                  ifelse(df.pw.dizziness$colour=='orange',
                                         'RIMA',
                                         ifelse(df.pw.dizziness$colour=='deepskyblue',
                                                '*DRI',
                                                ifelse(df.pw.dizziness$colour=='purple',
                                                       'Agonist',
                                                       ifelse(df.pw.dizziness$colour=='green4',
                                                              'Vesicle transport facilitator',
                                                              'Other')))))
  pwma.dizziness <- metabin(n.e = n1, 
                            event.e = event1, 
                            n.c = n2, 
                            event.c = event2, 
                            studlab = studlab, 
                            data = df.pw.dizziness, 
                            sm = 'OR', 
                            comb.fixed = F, 
                            comb.random = T, 
                            title = 'dizziness',
                            prediction = T, 
                            hakn = T, 
                            method.tau = 'REML',
                            subgroup = group)
  
  ## EER ----
  dizziness.cer.pw <- metaprop(event = r, 
                               n = n, 
                               studlab = studlab, 
                               data = df.dizziness.merged[df.dizziness.merged$treatment=='placebo' & !(is.na(df.dizziness.merged$n)),], 
                               backtransf = T, 
                               comb.fixed = F, 
                               comb.random = T, 
                               hakn = T, 
                               method = 'GLMM')
  dizziness.rate.pla <- exp(dizziness.cer.pw$TE.random)/(1+exp(dizziness.cer.pw$TE.random))
  dizziness.odds.pla <- dizziness.rate.pla/(1-dizziness.rate.pla)
  dizziness.TE <- exp(pwma.dizziness$TE.random)
  dizziness.eer <- round(dizziness.TE*dizziness.odds.pla/(1+dizziness.TE*dizziness.odds.pla), digits = 3)
  dizziness.eer
}

# vomiting ----
{
  df.vomiting <- df[!is.na(df$vomiting), c('studlab', 
                                           'treatment', 
                                           'treatment_details', 
                                           'n_randomised', 
                                           'vomiting',
                                           'age_mean',
                                           'female_prop',
                                           'anhedonia_baseline_mean',
                                           'anxiety_baseline_mean',
                                           'reward_baseline_mean',
                                           'tx_duration',
                                           'arm')]
  colnames(df.vomiting) <- c('studlab', 
                             'treatment', 
                             'treatment_details', 
                             'n', 
                             'r',
                             'age_mean',
                             'female_prop',
                             'anhedonia_baseline_mean',
                             'anxiety_baseline_mean',
                             'reward_baseline_mean',
                             'tx_duration',
                             'arm')
  df.vomiting$treatment_details <- tolower(df.vomiting$treatment_details)
  df.vomiting$treatment_details <- ifelse(df.vomiting$treatment_details=='placebo', 'placebo', 'active')
  df.vomiting <- remove.onearm(df.vomiting)
  df.vomiting.merged <- collapse.arms.binary(df.vomiting)
  df.pw.vomiting <- pairwise(studlab = studlab,
                                      treat = treatment,
                                      n = n,
                                      event = r,
                                      data = df.vomiting.merged,
                                      sm = 'OR')
  df.pw.vomiting$age <- rowMeans(df.pw.vomiting[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.vomiting$female_prop <- rowMeans(df.pw.vomiting[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.vomiting$anhedonia_baseline <- rowMeans(df.pw.vomiting[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.vomiting$anxiety_baseline <- rowMeans(df.pw.vomiting[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.vomiting$reward_baseline <- rowMeans(df.pw.vomiting[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  df.pw.vomiting$colour <- ifelse(df.pw.vomiting$treat1 %in% IMAO, 
                                  'red', 
                                  ifelse(df.pw.vomiting$treat1 %in% RIMA,
                                         'orange',
                                         ifelse(df.pw.vomiting$treat1 %in% DRI,
                                                'deepskyblue',
                                                ifelse(df.pw.vomiting$treat1 %in% agonist,
                                                       'purple',
                                                       ifelse(df.pw.vomiting$treat1 %in% vescicular,
                                                              'green4',
                                                              'grey'
                                                       )))))
  df.pw.vomiting$group <- ifelse(df.pw.vomiting$colour=='red',
                                 'MAOI',
                                 ifelse(df.pw.vomiting$colour=='orange',
                                        'RIMA',
                                        ifelse(df.pw.vomiting$colour=='deepskyblue',
                                               '*DRI',
                                               ifelse(df.pw.vomiting$colour=='purple',
                                                      'Agonist',
                                                      ifelse(df.pw.vomiting$colour=='green4',
                                                             'Vesicle transport facilitator',
                                                             'Other')))))
  pwma.vomiting <- metabin(n.e = n1, 
                           event.e = event1, 
                           n.c = n2, 
                           event.c = event2, 
                           studlab = studlab, 
                           data = df.pw.vomiting, 
                           sm = 'OR', 
                           comb.fixed = F, 
                           comb.random = T, 
                           title = 'vomiting',
                           prediction = T, 
                           method.tau = 'REML',
                           subgroup = group)
  
  ## EER ----
  vomiting.cer.pw <- metaprop(event = r, 
                              n = n, 
                              studlab = studlab, 
                              data = df.vomiting.merged[df.vomiting.merged$treatment=='placebo',], 
                              backtransf = T, 
                              comb.fixed = F, 
                              comb.random = T, 
                              hakn = T, 
                              method = 'GLMM')
  vomiting.rate.pla <- exp(vomiting.cer.pw$TE.random)/(1+exp(vomiting.cer.pw$TE.random))
  vomiting.odds.pla <- vomiting.rate.pla/(1-vomiting.rate.pla)
  vomiting.TE <- exp(pwma.vomiting$TE.random)
  vomiting.eer <- round(vomiting.TE*vomiting.odds.pla/(1+vomiting.TE*vomiting.odds.pla), digits = 3)
  vomiting.eer
}

# meta-regressions ----
reg.anxiety.age <- metareg(pwma.anxiety, age_reg)
reg.anxiety.female <- metareg(pwma.anxiety, female_prop)
reg.anxiety.anxiety <- metareg(pwma.anxiety, anxiety_baseline)
reg.anxiety.tx <- metareg(pwma.anxiety, tx_duration_reg)
reg.acc.age <- metareg(pwma.acc, age_reg)
reg.acc.female <- metareg(pwma.acc, female_prop)
reg.acc.anxiety <- metareg(pwma.acc, anxiety_baseline)
reg.acc.tx <- metareg(pwma.acc, tx_duration_reg)
reg.tol.age <- metareg(pwma.tol, age_reg)
reg.tol.female <- metareg(pwma.tol, female_prop)
reg.tol.anxiety <- metareg(pwma.tol, anxiety_baseline)
reg.tol.tx <- metareg(pwma.tol, tx_duration_reg)
reg.nausea.age <- metareg(pwma.nausea, age_reg)
reg.nausea.female <- metareg(pwma.nausea, female_prop)
reg.nausea.anxiety <- metareg(pwma.nausea, anxiety_baseline)
reg.nausea.tx <- metareg(pwma.nausea, tx_duration_reg)
reg.headache.age <- metareg(pwma.headache, age_reg)
reg.headache.female <- metareg(pwma.headache, female_prop)
reg.headache.anxiety <- metareg(pwma.headache, anxiety_baseline)
reg.headache.tx <- metareg(pwma.headache, tx_duration_reg)
reg.insomnia.age <- metareg(pwma.insomnia, age_reg)
reg.insomnia.female <- metareg(pwma.insomnia, female_prop)
reg.insomnia.anxiety <- metareg(pwma.insomnia, anxiety_baseline)
reg.insomnia.tx <- metareg(pwma.insomnia, tx_duration_reg)
reg.constipation.age <- metareg(pwma.constipation, age_reg)
reg.constipation.female <- metareg(pwma.constipation, female_prop)
reg.constipation.anxiety <- metareg(pwma.constipation, anxiety_baseline)
reg.constipation.tx <- metareg(pwma.constipation, tx_duration_reg)
reg.dizziness.age <- metareg(pwma.dizziness, age_reg)
reg.dizziness.female <- metareg(pwma.dizziness, female_prop)
reg.dizziness.anxiety <- metareg(pwma.dizziness, anxiety_baseline)
reg.dizziness.tx <- metareg(pwma.dizziness, tx_duration_reg)
reg.dry_mouth.age <- metareg(pwma.dry_mouth, age_reg)
reg.dry_mouth.female <- metareg(pwma.dry_mouth, female_prop)
reg.dry_mouth.anxiety <- metareg(pwma.dry_mouth, anxiety_baseline)
reg.dry_mouth.tx <- metareg(pwma.dry_mouth, tx_duration_reg)

if (reg.anxiety.age$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.anxiety.female$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.anxiety.anxiety$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.anxiety.tx$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.acc.age$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.acc.female$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.acc.anxiety$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.acc.tx$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.tol.age$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.tol.female$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.tol.anxiety$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.tol.tx$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.nausea.age$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.nausea.female$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.nausea.anxiety$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')} # drop
if (reg.nausea.tx$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.headache.age$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.headache.female$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.headache.anxiety$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')} # drop
if (reg.headache.tx$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.insomnia.age$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.insomnia.female$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.insomnia.anxiety$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')} # drop
if (reg.insomnia.tx$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.constipation.age$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.constipation.female$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.constipation.anxiety$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')} # drop
if (reg.constipation.tx$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.dizziness.age$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.dizziness.female$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.dizziness.anxiety$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')} # drop
if (reg.dizziness.tx$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.dry_mouth.age$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.dry_mouth.female$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}
if (reg.dry_mouth.anxiety$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')} # drop
if (reg.dry_mouth.tx$k >= 10) {cat('Keep it.', sep = '\n')} else {cat('[!] Drop it.', sep = '\n')}

rm(reg.nausea.anxiety)
rm(reg.headache.anxiety)
rm(reg.insomnia.anxiety)
rm(reg.constipation.anxiety)
rm(reg.dizziness.anxiety)
rm(reg.dry_mouth.anxiety)

# other ----
{
  df.anhedonia <- df[!is.na(df$anhedonia_followup_mean), c('studlab', 
                                                           'treatment', 
                                                           'treatment_details', 
                                                           'anhedonia_followup_n', 
                                                           'anhedonia_followup_mean', 
                                                           'anhedonia_followup_sd', 
                                                           'anhedonia_followup_type', 
                                                           'anhedonia_followup_scale', 
                                                           'anhedonia_followup_direction', 
                                                           'age_mean',
                                                           'female_prop',
                                                           'anhedonia_baseline_mean',
                                                           'anxiety_baseline_mean',
                                                           'reward_baseline_mean',
                                                           'tx_duration',
                                                           'arm')]
  studies.keep <- df.anhedonia$studlab[df.anhedonia$treatment=='bupropion']
  df.anhedonia <- df.anhedonia[df.anhedonia$studlab %in% studies.keep, ]
  df.anhedonia.bup <- df.anhedonia[df.anhedonia$treatment %in% c('bupropion', 'placebo'), ]
  df.anhedonia.bup <- df.anhedonia.bup[!is.na(df.anhedonia.bup$anhedonia_followup_mean), ]
  df.anhedonia.bup <- df.anhedonia.bup[!is.na(df.anhedonia.bup$anhedonia_followup_direction), ]
  df.anhedonia.bup$anhedonia_followup_mean[df.anhedonia.bup$anhedonia_followup_direction=='positive'] <- df.anhedonia.bup$anhedonia_followup_mean[df.anhedonia.bup$anhedonia_followup_direction=='positive']*-1
  colnames(df.anhedonia.bup) <- c('studlab', 
                              'treatment', 
                              'treatment_details', 
                              'n', 
                              'mean', 
                              'sd', 
                              'anhedonia_followup_type', 
                              'anhedonia_followup_scale', 
                              'anhedonia_followup_direction', 
                              'age_mean',
                              'female_prop',
                              'anhedonia_baseline_mean',
                              'anxiety_baseline_mean',
                              'reward_baseline_mean',
                              'tx_duration',
                              'arm')
  df.anhedonia.bup <- remove.onearm(df.anhedonia.bup)
  df.anhedonia.bup.merged <- collapse.arms.continuous(df.anhedonia.bup)
  df.pw.anhedonia.bup <- pairwise(studlab = studlab,
                                       treat = treatment,
                                       n = n,
                                       mean = mean,
                                       sd = sd,
                                       data = df.anhedonia.bup.merged,
                                       sm = 'SMD')
  df.pw.anhedonia.bup$age <- rowMeans(df.pw.anhedonia.bup[, c('age_mean1', 'age_mean2')], na.rm = T)
  df.pw.anhedonia.bup$female_prop <- rowMeans(df.pw.anhedonia.bup[, c('female_prop1', 'female_prop2')], na.rm = T)
  df.pw.anhedonia.bup$anhedonia_baseline <- rowMeans(df.pw.anhedonia.bup[, c('anhedonia_baseline_mean1', 'anhedonia_baseline_mean2')], na.rm = T)
  df.pw.anhedonia.bup$anxiety_baseline <- rowMeans(df.pw.anhedonia.bup[, c('anxiety_baseline_mean1', 'anxiety_baseline_mean2')], na.rm = T)
  df.pw.anhedonia.bup$reward_baseline <- rowMeans(df.pw.anhedonia.bup[, c('reward_baseline_mean1', 'reward_baseline_mean2')], na.rm = T)
  df.pw.anhedonia.bup$colour <- ifelse(df.pw.anhedonia.bup$treat1 %in% IMAO, 
                                   'red', 
                                   ifelse(df.pw.anhedonia.bup$treat1 %in% RIMA,
                                          'orange',
                                          ifelse(df.pw.anhedonia.bup$treat1 %in% DRI,
                                                 'deepskyblue',
                                                 ifelse(df.pw.anhedonia.bup$treat1 %in% agonist,
                                                        'purple',
                                                        ifelse(df.pw.anhedonia.bup$treat1 %in% vescicular,
                                                               'green4',
                                                               'grey'
                                                        )))))
  pwma.anhedonia.bup <- metacont(n.e = n1, 
                                 mean.e = mean1, 
                                 sd.e = sd1, 
                                 n.c = n2, 
                                 mean.c = mean2, 
                                 sd.c = sd2, 
                                 studlab = studlab, 
                                 data = df.pw.anhedonia.bup, 
                                 sm = 'SMD', 
                                 method.smd = 'Hedges', 
                                 comb.fixed = F, 
                                 comb.random = T, 
                                 title = 'Anhedonia',
                                 prediction = T, 
                                 hakn = T, 
                                 method.tau = 'REML')
}

# drop it if you don't like it ----
names <- c('pwma.anhedonia',
           'pwma.anxiety',
           'pwma.anxiety2',
           'pwma.acc',
           'acc.eer',
           'acc.rate.pla',
           'pwma.tol',
           'tol.eer',
           'tol.rate.pla', 
           'pwma.nausea',
           'nausea.eer', 
           'nausea.rate.pla', 
           'pwma.headache',
           'headache.eer', 
           'headache.rate.pla', 
           'pwma.insomnia',
           'insomnia.eer', 
           'insomnia.rate.pla', 
           'pwma.dry_mouth',
           'dry_mouth.eer', 
           'dry_mouth.rate.pla', 
           'pwma.constipation',
           'constipation.eer', 
           'constipation.rate.pla', 
           'pwma.dizziness',
           'dizziness.eer', 
           'dizziness.rate.pla', 
           'pwma.vomiting', 
           'vomiting.eer',
           'vomiting.rate.pla',
           'df',
           'reg.anxiety.age',
           'reg.anxiety.female',
           'reg.anxiety.anxiety',
           'reg.anxiety.tx',
           'reg.acc.age',
           'reg.acc.female',
           'reg.acc.anxiety',
           'reg.acc.tx',
           'reg.tol.age',
           'reg.tol.female',
           'reg.tol.anxiety',
           'reg.tol.tx',
           'reg.nausea.age',
           'reg.nausea.female',
           'reg.nausea.tx',
           'reg.headache.age',
           'reg.headache.female',
           'reg.headache.tx',
           'reg.insomnia.age',
           'reg.insomnia.female',
           'reg.insomnia.tx',
           'reg.constipation.age',
           'reg.constipation.female',
           'reg.constipation.tx',
           'reg.dizziness.age',
           'reg.dizziness.female',
           'reg.dizziness.tx',
           'reg.dry_mouth.age',
           'reg.dry_mouth.female',
           'reg.dry_mouth.tx',
           'pwma.anhedonia.bup')

rm(list=setdiff(ls(pos=1), names), pos=1)

