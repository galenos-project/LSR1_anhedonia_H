rm(list = setdiff(ls(), lsf.str()))
library(meta)

# set wd ----
setwd("~/Documents/GitHub/LSR1_anhedonia_H")

# functions ----
source('util/util.R')

# welcome ----
system(paste0('say -v Moira ', welcome))

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

if (dir.exists('result/human')==F) {
  dir.create('result/')
  dir.create('result/human')
  dir.create('result/human/non-dopaminergic/')
}
for (i in outcomes) {
  if (dir.exists(paste0('result/human/', i))==F) {
    dir.create(paste0('result/human/', i))
    dir.create(paste0('result/human/', i, '/sensitivity'))
    dir.create(paste0('result/human/', i, '/sub-group'))
  }
}

# load df ----
df <- readRDS('rds/human/df.rds')

# anhedonia ----
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

sink('result/human/anhedonia/descriptive.txt')
cat('# List of studies contributing to the meta-analysis:', sep = '\n')
cat(pwma.anhedonia$studlab, sep = '\n')
cat(paste0(''), sep = '\n')
cat('# List of treatments contributing to the meta-analysis:', sep = '\n')
cat(sort(unique(df.anhedonia$treatment[df.anhedonia$studlab %in% pwma.anhedonia$studlab])), sep = '\n')
cat(paste0(''), sep = '\n')
cat('# Additional details:', sep = '\n')
for (i in sort(unique(df.anhedonia$treatment[df.anhedonia$studlab %in% pwma.anhedonia$studlab]))) {
  cat(paste0(i, 
             ' (k = ', 
             length(unique(df.anhedonia$studlab[df.anhedonia$studlab %in% pwma.anhedonia$studlab & df.anhedonia$treatment == i])), 
             '; n = ', 
             sum(df.anhedonia$n[df.anhedonia$studlab %in% pwma.anhedonia$studlab & df.anhedonia$treatment == i], na.rm = T),
             '; r = ',
             sum(df.anhedonia$r[df.anhedonia$studlab %in% pwma.anhedonia$studlab & df.anhedonia$treatment == i], na.rm = T),
             ').'), 
      sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

sink('result/human/anhedonia/pwma.txt')
pwma.anhedonia
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

png('result/human/anhedonia/forest.png', width = 12.5, height = 5, units = 'in', res = 300)
forest(pwma.anhedonia, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Intervention', 
       label.c = 'Placebo', 
       label.left = 'Favours intervention',
       label.right = "Favours placebo",
       smlab = 'Anhedonia symptoms severity',
       col.square = pwma.anhedonia$data$colour
       #lower.equi = -0.20, upper.equi = 0.20, 
       #col.equi = '#F8F8F8', 
       #fill.lower.equi = c("#E4EBF4", "#F8F8F8"),
       #fill.upper.equi = c("#F8F8F8", "#EFE6ED")
       )
dev.off()

png('result/human/anhedonia/baujat.png', width = 10, height = 10, units = 'in', res = 300)
baujat(pwma.anhedonia)
dev.off()

## funnel ----
png(file = 'result/human/anhedonia/funnel.png', width = 7, height = 6, units = 'in', res = 300)
funnel(pwma.anhedonia, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c('green', 'yellow', 'pink'))
legend(-0.8, 0, c('0.1 > p > 0.05', '0.05 > p > 0.01', '< 0.01'), fill = c('green', 'yellow', 'pink'), bty = 'n')
dev.off()

sink('result/human/anhedonia/funnel linreg.txt')
metabias(pwma.anhedonia, method.bias = 'linreg')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date()), sep = '\n')
sink()

## sub-group ----
### age ----
sink('result/human/anhedonia/sub-group/age.txt')
if (!inherits(try(metareg(pwma.anhedonia, age), silent = T), "try-error")) {
  reg.anhedonia.age <- metareg(pwma.anhedonia, age)
  reg.anhedonia.age
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### sex ----
sink('result/human/anhedonia/sub-group/female.txt')
if (!inherits(try(metareg(pwma.anhedonia, female_prop), silent = T), "try-error")) {
  reg.anhedonia.female <- metareg(pwma.anhedonia, female_prop)
  reg.anhedonia.female
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### anhedonia baseline ----
sink('result/human/anhedonia/sub-group/anhedonia_baseline.txt')
if (!inherits(try(metareg(pwma.anhedonia, anhedonia_baseline), silent = T), "try-error")) {
  reg.anhedonia.anhedonia <- metareg(pwma.anhedonia, anhedonia_baseline)
  reg.anhedonia.anhedonia
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### anxiety baseline ----
sink('result/human/anhedonia/sub-group/anxiety_baseline.txt')
if (!inherits(try(metareg(pwma.anhedonia, anxiety_baseline), silent = T), "try-error")) {
  reg.anhedonia.anxiety <- metareg(pwma.anhedonia, anxiety_baseline)
  reg.anhedonia.anxiety
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### reward baseline ----
sink('result/human/anhedonia/sub-group/reward_baseline.txt')
if (!inherits(try(metareg(pwma.anhedonia, reward_baseline), silent = T), "try-error")) {
  reg.anhedonia.reward <- metareg(pwma.anhedonia, reward_baseline)
  reg.anhedonia.reward
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### tx duration ----
sink('result/human/anhedonia/sub-group/tx_duration.txt')
if (!inherits(try(metareg(pwma.anhedonia, tx_duration), silent = T), "try-error")) {
  reg.anhedonia.txduration <- metareg(pwma.anhedonia, tx_duration)
  reg.anhedonia.txduration
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

# anxiety ----
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

sink('result/human/anxiety/descriptive.txt')
cat('# List of studies contributing to the meta-analysis:', sep = '\n')
cat(pwma.anxiety$studlab, sep = '\n')
cat(paste0(''), sep = '\n')
cat('# List of treatments contributing to the meta-analysis:', sep = '\n')
cat(sort(unique(df.anxiety$treatment[df.anxiety$studlab %in% pwma.anxiety$studlab])), sep = '\n')
cat(paste0(''), sep = '\n')
cat('# Additional details:', sep = '\n')
for (i in sort(unique(df.anxiety$treatment[df.anxiety$studlab %in% pwma.anxiety$studlab]))) {
  cat(paste0(i, 
             ' (k = ', 
             length(unique(df.anxiety$studlab[df.anxiety$studlab %in% pwma.anxiety$studlab & df.anxiety$treatment == i])), 
             '; n = ', 
             sum(df.anxiety$n[df.anxiety$studlab %in% pwma.anxiety$studlab & df.anxiety$treatment == i], na.rm = T),
             '; r = ',
             sum(df.anxiety$r[df.anxiety$studlab %in% pwma.anxiety$studlab & df.anxiety$treatment == i], na.rm = T),
             ').'), 
      sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

sink('result/human/anxiety/pwma.txt')
pwma.anxiety
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

png('result/human/anxiety/forest.png', width = 12.5, height = 7.5, units = 'in', res = 300)
forest(pwma.anxiety, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Intervention', 
       label.c = 'Placebo', 
       label.left = 'Favours intervention',
       label.right = "Favours placebo",
       smlab = 'Anxiety symptoms severity',
       col.square = pwma.anxiety$data$colour
       #lower.equi = -0.20, upper.equi = 0.20, 
       #col.equi = '#F8F8F8', 
       #fill.lower.equi = c("#E4EBF4", "#F8F8F8"),
       #fill.upper.equi = c("#F8F8F8", "#EFE6ED")
)
dev.off()

## funnel ----
png(file = 'result/human/anxiety/funnel.png', width = 7, height = 6, units = 'in', res = 300)
funnel(pwma.anxiety, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c('green', 'yellow', 'pink'))
legend(0.1, 0, c('0.1 > p > 0.05', '0.05 > p > 0.01', '< 0.01'), fill = c('green', 'yellow', 'pink'), bty = 'n')
dev.off()

sink('result/human/anxiety/funnel linreg.txt')
metabias(pwma.anxiety, method.bias = 'linreg')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date()), sep = '\n')
sink()

## sub-group ----
### age ----
sink('result/human/anxiety/sub-group/age.txt')
if (!inherits(try(metareg(pwma.anxiety, age), silent = T), "try-error")) {
  reg.anxiety.age <- metareg(pwma.anxiety, age)
  reg.anxiety.age
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### sex ----
sink('result/human/anxiety/sub-group/female.txt')
if (!inherits(try(metareg(pwma.anxiety, female_prop), silent = T), "try-error")) {
  reg.anxiety.female <- metareg(pwma.anxiety, female_prop)
  reg.anxiety.female
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### anhedonia baseline ----
sink('result/human/anxiety/sub-group/anhedonia_baseline.txt')
if (!inherits(try(metareg(pwma.anxiety, anhedonia_baseline), silent = T), "try-error")) {
  reg.anxiety.anhedonia <- metareg(pwma.anxiety, anhedonia_baseline)
  reg.anxiety.anhedonia
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### anxiety baseline ----
sink('result/human/anxiety/sub-group/anxiety_baseline.txt')
if (!inherits(try(metareg(pwma.anxiety, anxiety_baseline), silent = T), "try-error")) {
  reg.anxiety.anxiety <- metareg(pwma.anxiety, anxiety_baseline)
  reg.anxiety.anxiety
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### reward baseline ----
sink('result/human/anxiety/sub-group/reward_baseline.txt')
if (!inherits(try(metareg(pwma.anxiety, reward_baseline), silent = T), "try-error")) {
  reg.anxiety.reward <- metareg(pwma.anxiety, reward_baseline)
  reg.anxiety.reward
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### tx duration ----
sink('result/human/anxiety/sub-group/tx_duration.txt')
if (!inherits(try(metareg(pwma.anxiety, tx_duration), silent = T), "try-error")) {
  reg.anxiety.txduration <- metareg(pwma.anxiety, tx_duration)
  reg.anxiety.txduration
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

# acceptability ----
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

sink('result/human/acceptability/descriptive.txt')
cat('# List of studies contributing to the meta-analysis:', sep = '\n')
cat(pwma.acc$studlab, sep = '\n')
cat(paste0(''), sep = '\n')
cat('# List of treatments contributing to the meta-analysis:', sep = '\n')
cat(sort(unique(df.acc$treatment[df.acc$studlab %in% pwma.acc$studlab])), sep = '\n')
cat(paste0(''), sep = '\n')
cat('# Additional details:', sep = '\n')
for (i in sort(unique(df.acc$treatment[df.acc$studlab %in% pwma.acc$studlab]))) {
  cat(paste0(i, 
             ' (k = ', 
             length(unique(df.acc$studlab[df.acc$studlab %in% pwma.acc$studlab & df.acc$treatment == i])), 
             '; n = ', 
             sum(df.acc$n[df.acc$studlab %in% pwma.acc$studlab & df.acc$treatment == i], na.rm = T),
             '; r = ',
             sum(df.acc$r[df.acc$studlab %in% pwma.acc$studlab & df.acc$treatment == i], na.rm = T),
             ').'), 
      sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

sink('result/human/acceptability/pwma.txt')
pwma.acc
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

png('result/human/acceptability/forest.png', width = 12.5, height = 15.5, units = 'in', res = 300)
forest(pwma.acc, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Intervention', 
       label.c = 'Placebo', 
       label.left = 'Favours intervention',
       label.right = "Favours placebo",
       smlab = 'Dropout due to any reasons',
       col.square = pwma.acc$data$colour
)
dev.off()

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

sink('result/human/acceptability/rates.txt')
cat(paste0('Control event rate (placebo): ', round(acc.rate.pla, 2)), sep = '\n')
cat(paste0('Experimental event rate (pro-dopaminergic interventions): ', round(acc.eer, 2)), sep = '\n')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

## funnel ----
png(file = 'result/human/acceptability/funnel.png', width = 7, height = 6, units = 'in', res = 300)
funnel(pwma.acc, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c('green', 'yellow', 'pink'))
legend(3, 0, c('0.1 > p > 0.05', '0.05 > p > 0.01', '< 0.01'), fill = c('green', 'yellow', 'pink'), bty = 'n')
dev.off()

sink('result/human/acceptability/funnel linreg.txt')
metabias(pwma.acc, method.bias = 'linreg')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date()), sep = '\n')
sink()

## sub-group ----
### age ----
sink('result/human/acceptability/sub-group/age.txt')
if (!inherits(try(metareg(pwma.acc, age), silent = T), "try-error")) {
  reg.acc.age <- metareg(pwma.acc, age)
  reg.acc.age
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### sex ----
sink('result/human/acceptability/sub-group/female.txt')
if (!inherits(try(metareg(pwma.acc, female_prop), silent = T), "try-error")) {
  reg.acc.female <- metareg(pwma.acc, female_prop)
  reg.acc.female
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### anhedonia baseline ----
sink('result/human/acceptability/sub-group/anhedonia_baseline.txt')
if (!inherits(try(metareg(pwma.acc, anhedonia_baseline), silent = T), "try-error")) {
  reg.acc.anhedonia <- metareg(pwma.acc, anhedonia_baseline)
  reg.acc.anhedonia
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### anxiety baseline ----
sink('result/human/acceptability/sub-group/anxiety_baseline.txt')
if (!inherits(try(metareg(pwma.acc, anxiety_baseline), silent = T), "try-error")) {
  reg.acc.anxiety <- metareg(pwma.acc, anxiety_baseline)
  reg.acc.anxiety
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### reward baseline ----
sink('result/human/acceptability/sub-group/reward_baseline.txt')
if (!inherits(try(metareg(pwma.acc, reward_baseline), silent = T), "try-error")) {
  reg.acc.reward <- metareg(pwma.acc, reward_baseline)
  reg.acc.reward
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### tx duration ----
sink('result/human/acceptability/sub-group/tx_duration.txt')
if (!inherits(try(metareg(pwma.acc, tx_duration), silent = T), "try-error")) {
  reg.acc.txduration <- metareg(pwma.acc, tx_duration)
  reg.acc.txduration
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

# tolerability ----
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

sink('result/human/tolerability/descriptive.txt')
cat('# List of studies contributing to the meta-analysis:', sep = '\n')
cat(pwma.tol$studlab, sep = '\n')
cat(paste0(''), sep = '\n')
cat('# List of treatments contributing to the meta-analysis:', sep = '\n')
cat(sort(unique(df.tol$treatment[df.tol$studlab %in% pwma.tol$studlab])), sep = '\n')
cat(paste0(''), sep = '\n')
cat('# Additional details:', sep = '\n')
for (i in sort(unique(df.tol$treatment[df.tol$studlab %in% pwma.tol$studlab]))) {
  cat(paste0(i, 
             ' (k = ', 
             length(unique(df.tol$studlab[df.tol$studlab %in% pwma.tol$studlab & df.tol$treatment == i])), 
             '; n = ', 
             sum(df.tol$n[df.tol$studlab %in% pwma.tol$studlab & df.tol$treatment == i], na.rm = T),
             '; r = ',
             sum(df.tol$r[df.tol$studlab %in% pwma.tol$studlab & df.tol$treatment == i], na.rm = T),
             ').'), 
      sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

sink('result/human/tolerability/pwma.txt')
pwma.tol
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

png('result/human/tolerability/forest.png', width = 12.5, height = 13.5, units = 'in', res = 300)
forest(pwma.tol, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Intervention', 
       label.c = 'Placebo', 
       label.left = 'Favours intervention',
       label.right = "Favours placebo",
       smlab = 'Dropout due to side effects',
       col.square = pwma.tol$data$colour
)
dev.off()

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

sink('result/human/tolerability/rates.txt')
cat(paste0('Control event rate (placebo): ', round(tol.rate.pla, 2)), sep = '\n')
cat(paste0('Experimental event rate (pro-dopaminergic interventions): ', round(tol.eer, 2)), sep = '\n')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

## funnel ----
png(file = 'result/human/tolerability/funnel.png', width = 7, height = 6, units = 'in', res = 300)
funnel(pwma.tol, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c('green', 'yellow', 'pink'))
legend(4, 0, c('0.1 > p > 0.05', '0.05 > p > 0.01', '< 0.01'), fill = c('green', 'yellow', 'pink'), bty = 'n')
dev.off()

sink('result/human/tolerability/funnel linreg.txt')
metabias(pwma.tol, method.bias = 'linreg')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date()), sep = '\n')
sink()

## sub-group ----
### age ----
sink('result/human/tolerability/sub-group/age.txt')
if (!inherits(try(metareg(pwma.tol, age), silent = T), "try-error")) {
  reg.tol.age <- metareg(pwma.tol, age)
  reg.tol.age
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### sex ----
sink('result/human/tolerability/sub-group/female.txt')
if (!inherits(try(metareg(pwma.tol, female_prop), silent = T), "try-error")) {
  reg.tol.female <- metareg(pwma.tol, female_prop)
  reg.tol.female
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### anhedonia baseline ----
sink('result/human/tolerability/sub-group/anhedonia_baseline.txt')
if (!inherits(try(metareg(pwma.tol, anhedonia_baseline), silent = T), "try-error")) {
  reg.tol.anhedonia <- metareg(pwma.tol, anhedonia_baseline)
  reg.tol.anhedonia
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### anxiety baseline ----
sink('result/human/tolerability/sub-group/anxiety_baseline.txt')
if (!inherits(try(metareg(pwma.tol, anxiety_baseline), silent = T), "try-error")) {
  reg.tol.anxiety <- metareg(pwma.tol, anxiety_baseline)
  reg.tol.anxiety
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### reward baseline ----
sink('result/human/tolerability/sub-group/reward_baseline.txt')
if (!inherits(try(metareg(pwma.tol, reward_baseline), silent = T), "try-error")) {
  reg.tol.reward <- metareg(pwma.tol, reward_baseline)
  reg.tol.reward
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

### tx duration ----
sink('result/human/tolerability/sub-group/tx_duration.txt')
if (!inherits(try(metareg(pwma.tol, tx_duration), silent = T), "try-error")) {
  reg.tol.txduration <- metareg(pwma.tol, tx_duration)
  reg.tol.txduration
} else {
  cat('Meta-regression not applicable.', sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

# intervention ----
sort(unique(df$treatment[df$studlab %in% c(pwma.acc$studlab,
                                      pwma.tol$studlab,
                                      pwma.anhedonia$studlab,
                                      pwma.anxiety$studlab)]))

# nausea ----
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

sink('result/human/nausea/descriptive.txt')
cat('# List of studies contributing to the meta-analysis:', sep = '\n')
cat(pwma.nausea$studlab, sep = '\n')
cat(paste0(''), sep = '\n')
cat('# List of treatments contributing to the meta-analysis:', sep = '\n')
cat(sort(unique(df.nausea$treatment[df.nausea$studlab %in% pwma.nausea$studlab])), sep = '\n')
cat(paste0(''), sep = '\n')
cat('# Additional details:', sep = '\n')
for (i in sort(unique(df.nausea$treatment[df.nausea$studlab %in% pwma.nausea$studlab]))) {
  cat(paste0(i, 
             ' (k = ', 
             length(unique(df.nausea$studlab[df.nausea$studlab %in% pwma.nausea$studlab & df.nausea$treatment == i])), 
             '; n = ', 
             sum(df.nausea$n[df.nausea$studlab %in% pwma.nausea$studlab & df.nausea$treatment == i], na.rm = T),
             '; r = ',
             sum(df.nausea$r[df.nausea$studlab %in% pwma.nausea$studlab & df.nausea$treatment == i], na.rm = T),
             ').'), 
      sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

sink('result/human/nausea/pwma.txt')
pwma.nausea
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

png('result/human/nausea/forest.png', width = 12.5, height = 10, units = 'in', res = 300)
forest(pwma.nausea, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Intervention', 
       label.c = 'Placebo', 
       label.left = 'Favours intervention',
       label.right = "Favours placebo",
       smlab = 'Nausea',
       col.square = pwma.nausea$data$colour
)
dev.off()

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

sink('result/human/nausea/rates.txt')
cat(paste0('Control event rate (placebo): ', round(nausea.rate.pla, 2)), sep = '\n')
cat(paste0('Experimental event rate (pro-dopaminergic interventions): ', round(nausea.eer, 2)), sep = '\n')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

## funnel ----
png(file = 'result/human/nausea/funnel.png', width = 7, height = 6, units = 'in', res = 300)
funnel(pwma.nausea, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c('green', 'yellow', 'pink'))
legend(4, 0, c('0.1 > p > 0.05', '0.05 > p > 0.01', '< 0.01'), fill = c('green', 'yellow', 'pink'), bty = 'n')
dev.off()

sink('result/human/nausea/funnel linreg.txt')
metabias(pwma.nausea, method.bias = 'linreg')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date()), sep = '\n')
sink()

# headache ----
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

sink('result/human/headache/descriptive.txt')
cat('# List of studies contributing to the meta-analysis:', sep = '\n')
cat(pwma.headache$studlab, sep = '\n')
cat(paste0(''), sep = '\n')
cat('# List of treatments contributing to the meta-analysis:', sep = '\n')
cat(sort(unique(df.headache$treatment[df.headache$studlab %in% pwma.headache$studlab])), sep = '\n')
cat(paste0(''), sep = '\n')
cat('# Additional details:', sep = '\n')
for (i in sort(unique(df.headache$treatment[df.headache$studlab %in% pwma.headache$studlab]))) {
  cat(paste0(i, 
             ' (k = ', 
             length(unique(df.headache$studlab[df.headache$studlab %in% pwma.headache$studlab & df.headache$treatment == i])), 
             '; n = ', 
             sum(df.headache$n[df.headache$studlab %in% pwma.headache$studlab & df.headache$treatment == i], na.rm = T),
             '; r = ',
             sum(df.headache$r[df.headache$studlab %in% pwma.headache$studlab & df.headache$treatment == i], na.rm = T),
             ').'), 
      sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

sink('result/human/headache/pwma.txt')
pwma.headache
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

png('result/human/headache/forest.png', width = 12.5, height = 11.5, units = 'in', res = 300)
forest(pwma.headache, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Intervention', 
       label.c = 'Placebo', 
       label.left = 'Favours intervention',
       label.right = "Favours placebo",
       smlab = 'headache',
       col.square = pwma.headache$data$colour
)
dev.off()

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

sink('result/human/headache/rates.txt')
cat(paste0('Control event rate (placebo): ', round(headache.rate.pla, 2)), sep = '\n')
cat(paste0('Experimental event rate (pro-dopaminergic interventions): ', round(headache.eer, 2)), sep = '\n')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

## funnel ----
png(file = 'result/human/headache/funnel.png', width = 7, height = 6, units = 'in', res = 300)
funnel(pwma.headache, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c('green', 'yellow', 'pink'))
legend(4, 0, c('0.1 > p > 0.05', '0.05 > p > 0.01', '< 0.01'), fill = c('green', 'yellow', 'pink'), bty = 'n')
dev.off()

sink('result/human/headache/funnel linreg.txt')
metabias(pwma.headache, method.bias = 'linreg')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date()), sep = '\n')
sink()

# insomnia ----
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

sink('result/human/insomnia/descriptive.txt')
cat('# List of studies contributing to the meta-analysis:', sep = '\n')
cat(pwma.insomnia$studlab, sep = '\n')
cat(paste0(''), sep = '\n')
cat('# List of treatments contributing to the meta-analysis:', sep = '\n')
cat(sort(unique(df.insomnia$treatment[df.insomnia$studlab %in% pwma.insomnia$studlab])), sep = '\n')
cat(paste0(''), sep = '\n')
cat('# Additional details:', sep = '\n')
for (i in sort(unique(df.insomnia$treatment[df.insomnia$studlab %in% pwma.insomnia$studlab]))) {
  cat(paste0(i, 
             ' (k = ', 
             length(unique(df.insomnia$studlab[df.insomnia$studlab %in% pwma.insomnia$studlab & df.insomnia$treatment == i])), 
             '; n = ', 
             sum(df.insomnia$n[df.insomnia$studlab %in% pwma.insomnia$studlab & df.insomnia$treatment == i], na.rm = T),
             '; r = ',
             sum(df.insomnia$r[df.insomnia$studlab %in% pwma.insomnia$studlab & df.insomnia$treatment == i], na.rm = T),
             ').'), 
      sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

sink('result/human/insomnia/pwma.txt')
pwma.insomnia
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

png('result/human/insomnia/forest.png', width = 12.5, height = 10, units = 'in', res = 300)
forest(pwma.insomnia, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Intervention', 
       label.c = 'Placebo', 
       label.left = 'Favours intervention',
       label.right = "Favours placebo",
       smlab = 'insomnia',
       col.square = pwma.insomnia$data$colour
)
dev.off()

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

sink('result/human/insomnia/rates.txt')
cat(paste0('Control event rate (placebo): ', round(insomnia.rate.pla, 2)), sep = '\n')
cat(paste0('Experimental event rate (pro-dopaminergic interventions): ', round(insomnia.eer, 2)), sep = '\n')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

## funnel ----
png(file = 'result/human/insomnia/funnel.png', width = 7, height = 6, units = 'in', res = 300)
funnel(pwma.insomnia, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c('green', 'yellow', 'pink'))
legend(4, 0, c('0.1 > p > 0.05', '0.05 > p > 0.01', '< 0.01'), fill = c('green', 'yellow', 'pink'), bty = 'n')
dev.off()

sink('result/human/insomnia/funnel linreg.txt')
metabias(pwma.insomnia, method.bias = 'linreg')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date()), sep = '\n')
sink()

# dry_mouth ----
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

sink('result/human/dry_mouth/descriptive.txt')
cat('# List of studies contributing to the meta-analysis:', sep = '\n')
cat(pwma.dry_mouth$studlab, sep = '\n')
cat(paste0(''), sep = '\n')
cat('# List of treatments contributing to the meta-analysis:', sep = '\n')
cat(sort(unique(df.dry_mouth$treatment[df.dry_mouth$studlab %in% pwma.dry_mouth$studlab])), sep = '\n')
cat(paste0(''), sep = '\n')
cat('# Additional details:', sep = '\n')
for (i in sort(unique(df.dry_mouth$treatment[df.dry_mouth$studlab %in% pwma.dry_mouth$studlab]))) {
  cat(paste0(i, 
             ' (k = ', 
             length(unique(df.dry_mouth$studlab[df.dry_mouth$studlab %in% pwma.dry_mouth$studlab & df.dry_mouth$treatment == i])), 
             '; n = ', 
             sum(df.dry_mouth$n[df.dry_mouth$studlab %in% pwma.dry_mouth$studlab & df.dry_mouth$treatment == i], na.rm = T),
             '; r = ',
             sum(df.dry_mouth$r[df.dry_mouth$studlab %in% pwma.dry_mouth$studlab & df.dry_mouth$treatment == i], na.rm = T),
             ').'), 
      sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

sink('result/human/dry_mouth/pwma.txt')
pwma.dry_mouth
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

png('result/human/dry_mouth/forest.png', width = 12.5, height = 10, units = 'in', res = 300)
forest(pwma.dry_mouth, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Intervention', 
       label.c = 'Placebo', 
       label.left = 'Favours intervention',
       label.right = "Favours placebo",
       smlab = 'dry_mouth',
       col.square = pwma.dry_mouth$data$colour
)
dev.off()

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

sink('result/human/dry_mouth/rates.txt')
cat(paste0('Control event rate (placebo): ', round(dry_mouth.rate.pla, 2)), sep = '\n')
cat(paste0('Experimental event rate (pro-dopaminergic interventions): ', round(dry_mouth.eer, 2)), sep = '\n')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

## funnel ----
png(file = 'result/human/dry_mouth/funnel.png', width = 7, height = 6, units = 'in', res = 300)
funnel(pwma.dry_mouth, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c('green', 'yellow', 'pink'))
legend(4, 0, c('0.1 > p > 0.05', '0.05 > p > 0.01', '< 0.01'), fill = c('green', 'yellow', 'pink'), bty = 'n')
dev.off()

sink('result/human/dry_mouth/funnel linreg.txt')
metabias(pwma.dry_mouth, method.bias = 'linreg')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date()), sep = '\n')
sink()

# constipation ----
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

sink('result/human/constipation/descriptive.txt')
cat('# List of studies contributing to the meta-analysis:', sep = '\n')
cat(pwma.constipation$studlab, sep = '\n')
cat(paste0(''), sep = '\n')
cat('# List of treatments contributing to the meta-analysis:', sep = '\n')
cat(sort(unique(df.constipation$treatment[df.constipation$studlab %in% pwma.constipation$studlab])), sep = '\n')
cat(paste0(''), sep = '\n')
cat('# Additional details:', sep = '\n')
for (i in sort(unique(df.constipation$treatment[df.constipation$studlab %in% pwma.constipation$studlab]))) {
  cat(paste0(i, 
             ' (k = ', 
             length(unique(df.constipation$studlab[df.constipation$studlab %in% pwma.constipation$studlab & df.constipation$treatment == i])), 
             '; n = ', 
             sum(df.constipation$n[df.constipation$studlab %in% pwma.constipation$studlab & df.constipation$treatment == i], na.rm = T),
             '; r = ',
             sum(df.constipation$r[df.constipation$studlab %in% pwma.constipation$studlab & df.constipation$treatment == i], na.rm = T),
             ').'), 
      sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

sink('result/human/constipation/pwma.txt')
pwma.constipation
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

png('result/human/constipation/forest.png', width = 12.5, height = 9.5, units = 'in', res = 300)
forest(pwma.constipation, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Intervention', 
       label.c = 'Placebo', 
       label.left = 'Favours intervention',
       label.right = "Favours placebo",
       smlab = 'constipation',
       col.square = pwma.constipation$data$colour
)
dev.off()

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

sink('result/human/constipation/rates.txt')
cat(paste0('Control event rate (placebo): ', round(constipation.rate.pla, 2)), sep = '\n')
cat(paste0('Experimental event rate (pro-dopaminergic interventions): ', round(constipation.eer, 2)), sep = '\n')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

## funnel ----
png(file = 'result/human/constipation/funnel.png', width = 7, height = 6, units = 'in', res = 300)
funnel(pwma.constipation, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c('green', 'yellow', 'pink'))
legend(4, 0, c('0.1 > p > 0.05', '0.05 > p > 0.01', '< 0.01'), fill = c('green', 'yellow', 'pink'), bty = 'n')
dev.off()

sink('result/human/constipation/funnel linreg.txt')
metabias(pwma.constipation, method.bias = 'linreg')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date()), sep = '\n')
sink()

# dizziness ----
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

sink('result/human/dizziness/descriptive.txt')
cat('# List of studies contributing to the meta-analysis:', sep = '\n')
cat(pwma.dizziness$studlab, sep = '\n')
cat(paste0(''), sep = '\n')
cat('# List of treatments contributing to the meta-analysis:', sep = '\n')
cat(sort(unique(df.dizziness$treatment[df.dizziness$studlab %in% pwma.dizziness$studlab])), sep = '\n')
cat(paste0(''), sep = '\n')
cat('# Additional details:', sep = '\n')
for (i in sort(unique(df.dizziness$treatment[df.dizziness$studlab %in% pwma.dizziness$studlab]))) {
  cat(paste0(i, 
             ' (k = ', 
             length(unique(df.dizziness$studlab[df.dizziness$studlab %in% pwma.dizziness$studlab & df.dizziness$treatment == i])), 
             '; n = ', 
             sum(df.dizziness$n[df.dizziness$studlab %in% pwma.dizziness$studlab & df.dizziness$treatment == i], na.rm = T),
             '; r = ',
             sum(df.dizziness$r[df.dizziness$studlab %in% pwma.dizziness$studlab & df.dizziness$treatment == i], na.rm = T),
             ').'), 
      sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

sink('result/human/dizziness/pwma.txt')
pwma.dizziness
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

png('result/human/dizziness/forest.png', width = 12.5, height = 10, units = 'in', res = 300)
forest(pwma.dizziness, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Intervention', 
       label.c = 'Placebo', 
       label.left = 'Favours intervention',
       label.right = "Favours placebo",
       smlab = 'dizziness',
       col.square = pwma.dizziness$data$colour
)
dev.off()

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

sink('result/human/dizziness/rates.txt')
cat(paste0('Control event rate (placebo): ', round(dizziness.rate.pla, 2)), sep = '\n')
cat(paste0('Experimental event rate (pro-dopaminergic interventions): ', round(dizziness.eer, 2)), sep = '\n')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

## funnel ----
png(file = 'result/human/dizziness/funnel.png', width = 7, height = 6, units = 'in', res = 300)
funnel(pwma.dizziness, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c('green', 'yellow', 'pink'))
legend(4, 0, c('0.1 > p > 0.05', '0.05 > p > 0.01', '< 0.01'), fill = c('green', 'yellow', 'pink'), bty = 'n')
dev.off()

sink('result/human/dizziness/funnel linreg.txt')
metabias(pwma.dizziness, method.bias = 'linreg')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date()), sep = '\n')
sink()

:# vomiting ----
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

sink('result/human/vomiting/descriptive.txt')
cat('# List of studies contributing to the meta-analysis:', sep = '\n')
cat(pwma.vomiting$studlab, sep = '\n')
cat(paste0(''), sep = '\n')
cat('# List of treatments contributing to the meta-analysis:', sep = '\n')
cat(sort(unique(df.vomiting$treatment[df.vomiting$studlab %in% pwma.vomiting$studlab])), sep = '\n')
cat(paste0(''), sep = '\n')
cat('# Additional details:', sep = '\n')
for (i in sort(unique(df.vomiting$treatment[df.vomiting$studlab %in% pwma.vomiting$studlab]))) {
  cat(paste0(i, 
             ' (k = ', 
             length(unique(df.vomiting$studlab[df.vomiting$studlab %in% pwma.vomiting$studlab & df.vomiting$treatment == i])), 
             '; n = ', 
             sum(df.vomiting$n[df.vomiting$studlab %in% pwma.vomiting$studlab & df.vomiting$treatment == i], na.rm = T),
             '; r = ',
             sum(df.vomiting$r[df.vomiting$studlab %in% pwma.vomiting$studlab & df.vomiting$treatment == i], na.rm = T),
             ').'), 
      sep = '\n')
}
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

sink('result/human/vomiting/pwma.txt')
pwma.vomiting
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

png('result/human/vomiting/forest.png', width = 12.5, height = 5, units = 'in', res = 300)
forest(pwma.vomiting, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Intervention', 
       label.c = 'Placebo', 
       label.left = 'Favours intervention',
       label.right = "Favours placebo",
       smlab = 'vomiting',
       col.square = pwma.vomiting$data$colour
)
dev.off()

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

sink('result/human/vomiting/rates.txt')
cat(paste0('Control event rate (placebo): ', round(vomiting.rate.pla, 2)), sep = '\n')
cat(paste0('Experimental event rate (pro-dopaminergic interventions): ', round(vomiting.eer, 2)), sep = '\n')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date(), ' by user ', Sys.info()[['user']], ' (script v', version, ').'), sep = '\n')
sink()

## funnel ----
png(file = 'result/human/vomiting/funnel.png', width = 7, height = 6, units = 'in', res = 300)
funnel(pwma.vomiting, pch = 16, contour = c(0.9, 0.95, 0.99), col.contour = c('green', 'yellow', 'pink'))
legend(4, 0, c('0.1 > p > 0.05', '0.05 > p > 0.01', '< 0.01'), fill = c('green', 'yellow', 'pink'), bty = 'n')
dev.off()

sink('result/human/vomiting/funnel linreg.txt')
metabias(pwma.vomiting, method.bias = 'linreg')
cat(paste0(''), sep = '\n')
cat(paste0('File created on ', Sys.Date()), sep = '\n')
sink()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# NMA, all ----
library(netmeta)
desirable <- c('treat', 'TE', 'LCI', 'UCI', 'seTE')

## vs placebo ----
results.all <- list()

for (i in 1:10) {
  df <- read.csv(paste0('data/human/data_inhouse_vivli_aggregate/data_aggregated_', i, '.csv'))
  df.pw <- netmeta::pairwise(studlab = STUDYID,
                             treat = TREATMENT_GROUP,
                             n = patient_n,
                             mean = endpoint_mean,
                             sd = endpoint_std,
                             data = df,
                             sm = 'SMD')
  net.df <- netmeta(df.pw, 
                    ref = 'placebo')
  results <- data.frame(matrix(ncol = length(desirable),
                               nrow = net.df$n))
  colnames(results) <- desirable
  rownames(results) <- net.df$trts
  results[, 'treat'] <- net.df$trts
  results[, 'TE'] <- net.df$TE.random[, 'placebo']
  results[, 'LCI'] <- net.df$lower.random[, 'placebo']
  results[, 'UCI'] <- net.df$upper.random[, 'placebo']
  results[, 'seTE'] <- net.df$seTE.random[, 'placebo']
  results.all[[i]] <- results
  rm(df)
  rm(df.pw)
  rm(net.df)
  rm(results)
}

r <- do.call(rbind, results.all)
r <- aggregate(r, list(r$treat), mean)
r <- r[,-which(colnames(r)=='treat')]

write.csv(r, 'result/human/non-dopaminergic/IPD_AD_NMA.csv')

# forest plot
s <- r[order(r$seTE),]

png('result/human/non-dopaminergic/IPD_AD_NMA.png', width = 7, height = 7, units = 'in', res = 300)
ggforestplot::forestplot(
  df = s,
  name = Group.1,
  estimate = TE,
  se = seTE,
  xlab = 'SMD < 1: antidepressant better than placebo',
  title = "Anhedonia symptoms: all studies"
) +
  ggplot2::xlim(-1, 1)
dev.off()

rm(r)
rm(s)

## vs citalopram ----
results.all <- list()

for (i in 1:10) {
  df <- read.csv(paste0('data/human/data_inhouse_vivli_aggregate/data_aggregated_', i, '.csv'))
  df.pw <- netmeta::pairwise(studlab = STUDYID,
                             treat = TREATMENT_GROUP,
                             n = patient_n,
                             mean = endpoint_mean,
                             sd = endpoint_std,
                             data = df,
                             sm = 'SMD')
  net.df <- netmeta(df.pw, 
                    ref = 'citalopram')
  results <- data.frame(matrix(ncol = length(desirable),
                               nrow = net.df$n))
  colnames(results) <- desirable
  rownames(results) <- net.df$trts
  results[, 'treat'] <- net.df$trts
  results[, 'TE'] <- net.df$TE.random[, 'citalopram']
  results[, 'LCI'] <- net.df$lower.random[, 'citalopram']
  results[, 'UCI'] <- net.df$upper.random[, 'citalopram']
  results[, 'seTE'] <- net.df$seTE.random[, 'citalopram']
  results.all[[i]] <- results
  rm(df)
  rm(df.pw)
  rm(net.df)
  rm(results)
}

r <- do.call(rbind, results.all)
r <- aggregate(r, list(r$treat), mean)
r <- r[,-which(colnames(r)=='treat')]

write.csv(r, 'result/human/non-dopaminergic/IPD_AD_NMA_citalopram.csv')

# forest plot
s <- r[order(r$seTE),]

png('result/human/non-dopaminergic/IPD_AD_NMA_citalopram.png', width = 7, height = 7, units = 'in', res = 300)
ggforestplot::forestplot(
  df = s,
  name = Group.1,
  estimate = TE,
  se = seTE,
  xlab = 'SMD < 1: antidepressant better than citalopram',
  title = "Anhedonia symptoms: all studies"
) +
  ggplot2::xlim(-1, 1)
dev.off()

rm(r)
rm(s)

# NMA, only pla ----
library(netmeta)
desirable <- c('treat', 'TE', 'LCI', 'UCI', 'seTE')

## vs placebo ----
results.all <- list()

for (i in 1:10) {
  df <- read.csv(paste0('data/human/data_inhouse_vivli_aggregate/data_aggregated_', i, '.csv'))
  studies.keep <- df$STUDYID[df$TREATMENT_GROUP=='placebo']
  df <- df[df$STUDYID %in% studies.keep, ]
  df.pw <- netmeta::pairwise(studlab = STUDYID,
                             treat = TREATMENT_GROUP,
                             n = patient_n,
                             mean = endpoint_mean,
                             sd = endpoint_std,
                             data = df,
                             sm = 'SMD')
  net.df <- netmeta(df.pw, 
                    ref = 'placebo')
  results <- data.frame(matrix(ncol = length(desirable),
                               nrow = net.df$n))
  colnames(results) <- desirable
  rownames(results) <- net.df$trts
  results[, 'treat'] <- net.df$trts
  results[, 'TE'] <- net.df$TE.random[, 'placebo']
  results[, 'LCI'] <- net.df$lower.random[, 'placebo']
  results[, 'UCI'] <- net.df$upper.random[, 'placebo']
  results[, 'seTE'] <- net.df$seTE.random[, 'placebo']
  results.all[[i]] <- results
  rm(df)
  rm(df.pw)
  rm(net.df)
  rm(results)
}

r <- do.call(rbind, results.all)
r <- aggregate(r, list(r$treat), mean)
r <- r[,-which(colnames(r)=='treat')]

write.csv(r, 'result/human/non-dopaminergic/IPD_AD_NMA_pla.csv')

# forest plot
s <- r[order(r$seTE),]

png('result/human/non-dopaminergic/IPD_AD_NMA_pla.png', width = 7, height = 7, units = 'in', res = 300)
ggforestplot::forestplot(
  df = s,
  name = Group.1,
  estimate = TE,
  se = seTE,
  xlab = 'SMD < 1: antidepressant better than placebo',
  title = "Anhedonia symptoms: PBO controlled studies"
) +
  ggplot2::xlim(-1, 1)
dev.off()

rm(r)
rm(s)

## vs citalopram ----
results.all <- list()

for (i in 1:10) {
  df <- read.csv(paste0('data/human/data_inhouse_vivli_aggregate/data_aggregated_', i, '.csv'))
  studies.keep <- df$STUDYID[df$TREATMENT_GROUP=='placebo']
  df <- df[df$STUDYID %in% studies.keep, ]
  df.pw <- netmeta::pairwise(studlab = STUDYID,
                             treat = TREATMENT_GROUP,
                             n = patient_n,
                             mean = endpoint_mean,
                             sd = endpoint_std,
                             data = df,
                             sm = 'SMD')
  net.df <- netmeta(df.pw, 
                    ref = 'citalopram')
  results <- data.frame(matrix(ncol = length(desirable),
                               nrow = net.df$n))
  colnames(results) <- desirable
  rownames(results) <- net.df$trts
  results[, 'treat'] <- net.df$trts
  results[, 'TE'] <- net.df$TE.random[, 'citalopram']
  results[, 'LCI'] <- net.df$lower.random[, 'citalopram']
  results[, 'UCI'] <- net.df$upper.random[, 'citalopram']
  results[, 'seTE'] <- net.df$seTE.random[, 'citalopram']
  results.all[[i]] <- results
  rm(df)
  rm(df.pw)
  rm(net.df)
  rm(results)
}

r <- do.call(rbind, results.all)
r <- aggregate(r, list(r$treat), mean)
r <- r[,-which(colnames(r)=='treat')]

write.csv(r, 'result/human/non-dopaminergic/IPD_AD_NMA_pla_citalopram.csv')

# forest plot
s <- r[order(r$seTE),]

png('result/human/non-dopaminergic/IPD_AD_NMA_pla_citalopram.png', width = 7, height = 7, units = 'in', res = 300)
ggforestplot::forestplot(
  df = s,
  name = Group.1,
  estimate = TE,
  se = seTE,
  xlab = 'SMD < 1: antidepressant better than citalopram',
  title = "Anhedonia symptoms: PBO controlled studies"
) +
  ggplot2::xlim(-1, 1)
dev.off()

rm(r)
rm(s)

# NMA, only H2H ----
library(netmeta)
desirable <- c('treat', 'TE', 'LCI', 'UCI', 'seTE')
results.all <- list()

for (i in 1:10) {
  df <- read.csv(paste0('data/human/data_inhouse_vivli_aggregate/data_aggregated_', i, '.csv'))
  df <- df[!df$TREATMENT_GROUP=='placebo', ]
  colnames(df) <- c('studlab', 'treatment', 'n', 'baseline_mean', 'baseline_sd', 'mean', 'sd')
  df <- df[, -c(4, 5)]
  df <- remove.onearm(df)
  df.pw <- netmeta::pairwise(studlab = studlab,
                             treat = treatment,
                             n = n,
                             mean = mean,
                             sd = sd,
                             data = df,
                             sm = 'SMD')
  connect.c1 <- netconnection(df.pw)
  net.df <- netmeta(df.pw, 
                    ref = 'citalopram',
                    subset = connect.c1$subnet==1)
  results <- data.frame(matrix(ncol = length(desirable),
                               nrow = net.df$n))
  colnames(results) <- desirable
  rownames(results) <- net.df$trts
  results[, 'treat'] <- net.df$trts
  results[, 'TE'] <- net.df$TE.random[, 'citalopram']
  results[, 'LCI'] <- net.df$lower.random[, 'citalopram']
  results[, 'UCI'] <- net.df$upper.random[, 'citalopram']
  results[, 'seTE'] <- net.df$seTE.random[, 'citalopram']
  results.all[[i]] <- results
  rm(df)
  rm(df.pw)
  rm(net.df)
  rm(results)
}

r <- do.call(rbind, results.all)
r <- aggregate(r, list(r$treat), mean)
r <- r[,-which(colnames(r)=='treat')]

write.csv(r, 'result/human/non-dopaminergic/IPD_AD_NMA_no_pla.csv')

# forest plot
s <- r[order(r$seTE),]

png('result/human/non-dopaminergic/IPD_AD_NMA_no_pla.png', width = 7, height = 7, units = 'in', res = 300)
ggforestplot::forestplot(
  df = s,
  name = Group.1,
  estimate = TE,
  se = seTE,
  xlab = 'SMD < 1: antidepressant better than citalopram',
  title = "Anhedonia symptoms: H2H studies"
) +
  ggplot2::xlim(-1, 1)
dev.off()

rm(r)
rm(s)

# NMA, pla + active ----
library(netmeta)
library(dplyr)
desirable <- c('treat', 'TE', 'LCI', 'UCI', 'seTE')

## vs placebo ----
results.all <- list()

for (i in 1:10) {
  df <- read.csv(paste0('data/human/data_inhouse_vivli_aggregate/data_aggregated_', i, '.csv'))
  studies.keep <- df$STUDYID[df$TREATMENT_GROUP=='placebo']
  df <- df[df$STUDYID %in% studies.keep, ]
  df <-  df %>% group_by(STUDYID) %>% filter(n()>=3)
  colnames(df) <- c('studlab', 'treatment', 'n', 'baseline_mean', 'baseline_sd', 'mean', 'sd')
  df <- df[, -c(4, 5)]
  df <- remove.onearm(df)
  df.pw <- netmeta::pairwise(studlab = studlab,
                             treat = treatment,
                             n = n,
                             mean = mean,
                             sd = sd,
                             data = df,
                             sm = 'SMD')
  connect.c1 <- netconnection(df.pw)
  net.df <- netmeta(df.pw, 
                    ref = 'placebo',
                    subset = connect.c1$subnet==1)
  results <- data.frame(matrix(ncol = length(desirable),
                               nrow = net.df$n))
  colnames(results) <- desirable
  rownames(results) <- net.df$trts
  results[, 'treat'] <- net.df$trts
  results[, 'TE'] <- net.df$TE.random[, 'placebo']
  results[, 'LCI'] <- net.df$lower.random[, 'placebo']
  results[, 'UCI'] <- net.df$upper.random[, 'placebo']
  results[, 'seTE'] <- net.df$seTE.random[, 'placebo']
  results.all[[i]] <- results
  rm(df)
  rm(df.pw)
  rm(net.df)
  rm(results)
}

r <- do.call(rbind, results.all)
r <- aggregate(r, list(r$treat), mean)
r <- r[,-which(colnames(r)=='treat')]

write.csv(r, 'result/human/non-dopaminergic/IPD_AD_NMA_pla_active.csv')

# forest plot
s <- r[order(r$seTE),]

png('result/human/non-dopaminergic/IPD_AD_NMA_pla_active.png', width = 7, height = 7, units = 'in', res = 300)
ggforestplot::forestplot(
  df = s,
  name = Group.1,
  estimate = TE,
  se = seTE,
  xlab = 'SMD < 1: antidepressant better than placebo',
  title = "Anhedonia symptoms: placebo and active comparator"
) +
  ggplot2::xlim(-1, 1)
dev.off()

rm(r)
rm(s)

## vs citalopram ----
results.all <- list()

for (i in 1:10) {
  df <- read.csv(paste0('data/human/data_inhouse_vivli_aggregate/data_aggregated_', i, '.csv'))
  studies.keep <- df$STUDYID[df$TREATMENT_GROUP=='placebo']
  df <- df[df$STUDYID %in% studies.keep, ]
  df <-  df %>% group_by(STUDYID) %>% filter(n()>=3)
  colnames(df) <- c('studlab', 'treatment', 'n', 'baseline_mean', 'baseline_sd', 'mean', 'sd')
  df <- df[, -c(4, 5)]
  df <- remove.onearm(df)
  df.pw <- netmeta::pairwise(studlab = studlab,
                             treat = treatment,
                             n = n,
                             mean = mean,
                             sd = sd,
                             data = df,
                             sm = 'SMD')
  connect.c1 <- netconnection(df.pw)
  net.df <- netmeta(df.pw, 
                    ref = 'citalopram',
                    subset = connect.c1$subnet==1)
  results <- data.frame(matrix(ncol = length(desirable),
                               nrow = net.df$n))
  colnames(results) <- desirable
  rownames(results) <- net.df$trts
  results[, 'treat'] <- net.df$trts
  results[, 'TE'] <- net.df$TE.random[, 'citalopram']
  results[, 'LCI'] <- net.df$lower.random[, 'citalopram']
  results[, 'UCI'] <- net.df$upper.random[, 'citalopram']
  results[, 'seTE'] <- net.df$seTE.random[, 'citalopram']
  results.all[[i]] <- results
  rm(df)
  rm(df.pw)
  rm(net.df)
  rm(results)
}

r <- do.call(rbind, results.all)
r <- aggregate(r, list(r$treat), mean)
r <- r[,-which(colnames(r)=='treat')]

write.csv(r, 'result/human/non-dopaminergic/IPD_AD_NMA_pla_active_citalopram.csv')

# forest plot
s <- r[order(r$seTE),]

png('result/human/non-dopaminergic/IPD_AD_NMA_pla_active_citalopram.png', width = 7, height = 7, units = 'in', res = 300)
ggforestplot::forestplot(
  df = s,
  name = Group.1,
  estimate = TE,
  se = seTE,
  xlab = 'SMD < 1: antidepressant better than citalopram',
  title = "Anhedonia symptoms: placebo and active comparator"
) +
  ggplot2::xlim(-1, 1)
dev.off()

rm(r)
rm(s)

# PWMA, bup vs pla ----
library(netmeta)
desirable <- c('treat', 'TE', 'LCI', 'UCI', 'seTE')
results.all <- list()

for (i in 1:10) {
  df <- read.csv(paste0('data/human/data_inhouse_vivli_aggregate/data_aggregated_', i, '.csv'))
  studies.keep <- df$STUDYID[df$TREATMENT_GROUP=='bupropion']
  df <- df[df$STUDYID %in% studies.keep, ]
  df <- df[df$TREATMENT_GROUP %in% c('bupropion', 'placebo'), ]
  df.pw <- netmeta::pairwise(studlab = STUDYID,
                             treat = TREATMENT_GROUP,
                             n = patient_n,
                             mean = endpoint_mean,
                             sd = endpoint_std,
                             data = df,
                             sm = 'SMD')
  pwma.df <- metacont(n.e = n1, 
                      mean.e = mean1, 
                      sd.e = sd1, 
                      n.c = n2, 
                      mean.c = mean2, 
                      sd.c = sd2, 
                      studlab = studlab, 
                      data = df.pw, 
                      sm = 'SMD', 
                      method.smd = 'Hedges', 
                      comb.fixed = F, 
                      comb.random = T, 
                      title = 'Anhedonia',
                      prediction = T, 
                      hakn = T, 
                      method.tau = 'REML')
  results <- data.frame(matrix(ncol = length(desirable),
                               nrow = 1))
  colnames(results) <- desirable
  results[, 'treat'] <- 'bupropion'
  results[, 'TE'] <- pwma.df$TE.random
  results[, 'LCI'] <- pwma.df$lower.random
  results[, 'UCI'] <- pwma.df$upper.random
  results[, 'seTE'] <- pwma.df$seTE.random
  results.all[[i]] <- results
  rm(df)
  rm(df.pw)
  rm(pwma.df)
  rm(results)
}

r <- do.call(rbind, results.all)
r <- aggregate(r, list(r$treat), mean)
r <- r[,-which(colnames(r)=='treat')]

write.csv(r, 'result/human/non-dopaminergic/IPD_AD_PWMA.csv')

# forest plot
s <- r[order(r$seTE),]

png('result/human/non-dopaminergic/IPD_AD_PWMA.png', width = 7, height = 5, units = 'in', res = 300)
ggforestplot::forestplot(
  df = s,
  name = Group.1,
  estimate = TE,
  se = seTE,
  xlab = 'SMD < 1: bupropion better than placebo',
  title = "Anhedonia symptoms: pairwise meta-analysis"
) +
  ggplot2::xlim(-1, 1)
dev.off()

rm(r)
rm(s)

# LSR1, bup vs pla ----
df <- readRDS('rds/human/df.rds')
studies.keep <- df$studlab[df$treatment=='bupropion']
df <- df[df$studlab %in% studies.keep, ]
df <- df[df$treatment %in% c('bupropion', 'placebo'), ]

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
pwma.anhedonia

r <- data.frame(matrix(ncol = length(desirable),
                       nrow = 1))
colnames(r) <- desirable
r[, 'treat'] <- 'bupropion'
r[, 'TE'] <- pwma.anhedonia$TE.random
r[, 'LCI'] <- pwma.anhedonia$lower.random
r[, 'UCI'] <- pwma.anhedonia$upper.random
r[, 'seTE'] <- pwma.anhedonia$seTE.random
write.csv(r, 'result/human/non-dopaminergic/GALENOS_AD_PWMA.csv')

png('result/human/non-dopaminergic/GALENOS_AD_PWMA.png', width = 12.5, height = 5, units = 'in', res = 300)
forest(pwma.anhedonia, 
       sortvar = seTE, 
       print.I2.ci = T, 
       label.e = 'Bupropion', 
       label.c = 'Placebo', 
       label.left = 'Favours bupropion',
       label.right = "Favours placebo",
       smlab = 'Anhedonia symptoms severity'
)
dev.off()
rm(r)

# LSR1+IPD - PWMA, bup vs pla ----
library(netmeta)
desirable <- c('treat', 'TE', 'LCI', 'UCI', 'seTE')
results.all <- list()

# prepare LSR1 df
df <- readRDS('rds/human/df.rds')
studies.keep <- df$studlab[df$treatment=='bupropion']
df <- df[df$studlab %in% studies.keep, ]
df <- df[df$treatment %in% c('bupropion', 'placebo'), ]
df.lsr1 <- df[, c('studlab', 
                  'treatment', 
                  'anhedonia_followup_n', 
                  'anhedonia_followup_mean', 
                  'anhedonia_followup_sd',
                  'anhedonia_followup_direction')]
df.lsr1 <- df.lsr1[!is.na(df.lsr1$anhedonia_followup_mean), ]
df.lsr1$anhedonia_followup_mean[df.lsr1$anhedonia_followup_direction=='positive'] <- df.lsr1$anhedonia_followup_mean[df.lsr1$anhedonia_followup_direction=='positive']*-1
df.lsr1 <- df.lsr1[, -which(colnames(df.lsr1)=='anhedonia_followup_direction')]
colnames(df.lsr1) <- c('studlab',
                  'treatment',
                  'n',
                  'mean',
                  'sd')
df.lsr1$arm <- ifelse(df.lsr1$treatment=='placebo', 'placebo', 'active')
df.lsr1 <- df.lsr1[!(df.lsr1$studlab=='Hewett (2009) - 87997883'), ]
df.lsr1 <- df.lsr1[!(df.lsr1$studlab=='Hewett (2010a) - 87997755'), ]
df.lsr1 <- df.lsr1[!(df.lsr1$studlab=='Koshino (2013) - 87997374'), ]
df.lsr1 <- remove.onearm(df.lsr1)
df.lsr1 <- collapse.arms.continuous.short(df.lsr1)
df.lsr1 <- df.lsr1[, -which(colnames(df.lsr1)=='arm')]
rm(df)

for (i in 1:10) {
  df <- read.csv(paste0('data/human/data_inhouse_vivli_aggregate/data_aggregated_', i, '.csv'))
  studies.keep <- df$STUDYID[df$TREATMENT_GROUP=='bupropion']
  df <- df[df$STUDYID %in% studies.keep, ]
  df <- df[df$TREATMENT_GROUP %in% c('bupropion', 'placebo'), ]
  df <- df[, -c(4, 5)]
  colnames(df) <- c('studlab',
                    'treatment',
                    'n',
                    'mean',
                    'sd')
  df <- rbind(df, df.lsr1)
  df.pw <- netmeta::pairwise(studlab = studlab,
                             treat = treatment,
                             n = n,
                             mean = mean,
                             sd = sd,
                             data = df,
                             sm = 'SMD')
  pwma.df <- metacont(n.e = n1, 
                      mean.e = mean1, 
                      sd.e = sd1, 
                      n.c = n2, 
                      mean.c = mean2, 
                      sd.c = sd2, 
                      studlab = studlab, 
                      data = df.pw, 
                      sm = 'SMD', 
                      method.smd = 'Hedges', 
                      comb.fixed = F, 
                      comb.random = T, 
                      title = 'Anhedonia',
                      prediction = T, 
                      hakn = T, 
                      method.tau = 'REML')
  results <- data.frame(matrix(ncol = length(desirable),
                               nrow = 1))
  colnames(results) <- desirable
  results[, 'treat'] <- 'bupropion'
  results[, 'TE'] <- pwma.df$TE.random
  results[, 'LCI'] <- pwma.df$lower.random
  results[, 'UCI'] <- pwma.df$upper.random
  results[, 'seTE'] <- pwma.df$seTE.random
  results.all[[i]] <- results
  rm(df)
  rm(df.pw)
  rm(pwma.df)
  rm(results)
}

r <- do.call(rbind, results.all)
r <- aggregate(r, list(r$treat), mean)
r <- r[,-which(colnames(r)=='treat')]

write.csv(r, 'result/human/non-dopaminergic/IPD_LSR1_PWMA.csv')

rm(r)






