rm(list = setdiff(ls(), lsf.str()))
library(meta)

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
  df.pw.anhedonia <- netmeta::pairwise(studlab = studlab,
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
  df.pw.anxiety <- netmeta::pairwise(studlab = studlab,
                                     treat = treatment,
                                     n = n,
                                     mean = mean,
                                     sd = sd,
                                     data = df.anxiety.merged,
                                     sm = 'SMD')
  df.pw.anxiety$age <- rowMeans(df.pw.anxiety[, c('age_mean1', 'age_mean2')], na.rm = T)
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
  df.pw.acc <- netmeta::pairwise(studlab = studlab,
                                 treat = treatment,
                                 n = n,
                                 event = r,
                                 data = df.acc.merged,
                                 sm = 'OR')
  df.pw.acc$age <- rowMeans(df.pw.acc[, c('age_mean1', 'age_mean2')], na.rm = T)
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
  df.pw.tol <- netmeta::pairwise(studlab = studlab,
                                 treat = treatment,
                                 n = n,
                                 event = r,
                                 data = df.tol.merged,
                                 sm = 'OR')
  df.pw.tol$age <- rowMeans(df.pw.tol[, c('age_mean1', 'age_mean2')], na.rm = T)
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
  df.pw.nausea <- netmeta::pairwise(studlab = studlab,
                                    treat = treatment,
                                    n = n,
                                    event = r,
                                    data = df.nausea.merged,
                                    sm = 'OR')
  df.pw.nausea$age <- rowMeans(df.pw.nausea[, c('age_mean1', 'age_mean2')], na.rm = T)
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
  df.pw.headache <- netmeta::pairwise(studlab = studlab,
                                      treat = treatment,
                                      n = n,
                                      event = r,
                                      data = df.headache.merged,
                                      sm = 'OR')
  df.pw.headache$age <- rowMeans(df.pw.headache[, c('age_mean1', 'age_mean2')], na.rm = T)
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
  df.pw.insomnia <- netmeta::pairwise(studlab = studlab,
                                      treat = treatment,
                                      n = n,
                                      event = r,
                                      data = df.insomnia.merged,
                                      sm = 'OR')
  df.pw.insomnia$age <- rowMeans(df.pw.insomnia[, c('age_mean1', 'age_mean2')], na.rm = T)
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
  df.pw.dry_mouth <- netmeta::pairwise(studlab = studlab,
                                       treat = treatment,
                                       n = n,
                                       event = r,
                                       data = df.dry_mouth.merged,
                                       sm = 'OR')
  df.pw.dry_mouth$age <- rowMeans(df.pw.dry_mouth[, c('age_mean1', 'age_mean2')], na.rm = T)
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
  df.pw.constipation <- netmeta::pairwise(studlab = studlab,
                                          treat = treatment,
                                          n = n,
                                          event = r,
                                          data = df.constipation.merged,
                                          sm = 'OR')
  df.pw.constipation$age <- rowMeans(df.pw.constipation[, c('age_mean1', 'age_mean2')], na.rm = T)
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
  df.pw.dizziness <- netmeta::pairwise(studlab = studlab,
                                       treat = treatment,
                                       n = n,
                                       event = r,
                                       data = df.dizziness.merged,
                                       sm = 'OR')
  df.pw.dizziness$age <- rowMeans(df.pw.dizziness[, c('age_mean1', 'age_mean2')], na.rm = T)
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
  df.pw.vomiting <- netmeta::pairwise(studlab = studlab,
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
                           hakn = T, 
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

#this runs all meta regressions for the below outcomes (which more than 10 studies contribute to)

#set meta-regression results container
meta_regressions_results <- list()
meta_analysis_results <- list(
  anxiety_result = pwma.anxiety, 
  acc_result = pwma.acc,
  tol_result = pwma.tol, 
  nausea_result = pwma.nausea,
  headache_result = pwma.headache,
  insomnia_result = pwma.insomnia,
  dry_mouth_result = pwma.dry_mouth,
  constipation_result = pwma.constipation,
  dizziness_result = pwma.dizziness
)
#set meta regression predictors
predictors <- c("age", "female_prop", "anhedonia_baseline", "anxiety_baseline", "reward_baseline", "tx_duration")

# Function to perform a single meta-regression
perform_meta_regression <- function(meta_result, predictor_var) {
  # Correctly constructing the formula using the predictor_var variable
  formula <- as.formula(paste0("~", predictor_var))
  result <- metareg(meta_result, formula)
  return(result)
}
# Iterating over meta-analysis results and predictors
for(meta_result_name in names(meta_analysis_results)) {
  for(predictor_var in predictors) {
    # Constructing a unique key for storing results
    result_key <- paste(meta_result_name, predictor_var, sep = "_")
    # Attempting the meta-regression, catching errors to continue with the next iteration
    result <- tryCatch({
      # Attempt to perform meta-regression
      perform_meta_regression(meta_analysis_results[[meta_result_name]], predictor_var)
    }, error = function(e) {
      # If an error occurs, print a message and return NULL
      message(paste0("Meta-regression failed for ", result_key, ": ", e$message))
      NULL # Return NULL to indicate failure
    }
    )
    # Store the result if the meta-regression was successful; otherwise, result will be NULL
    meta_regressions_results[[result_key]] <- result
  }
}

# drop it if you don't like it ----
names <- c('pwma.anhedonia',
           'pwma.anxiety',
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
           'meta_regressions_results')

rm(list=setdiff(ls(pos=1), names), pos=1)
