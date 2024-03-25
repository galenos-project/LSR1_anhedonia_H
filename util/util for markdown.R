# functions ----
## remove one-arm studies
remove.onearm <- function(data, warn = T) {
  # Remove 1-arm studies
  sel <- as.logical(sapply(data[,'studlab'],
                           function(study) {sum(data[,'studlab'] == study, na.rm = T) > 1
                           }))
  
  if (warn && !all(sel)) {
    warning(paste('Removed', sum(!sel), 'one-arm studies:',
                  paste(data[!sel,'studlab'], collapse=', ')))
  }
  data[sel, , drop = F]
}
## collapse arms - binary
collapse.arms.binary <- function(data) {
  data3.2 = data[, c(which(colnames(data)=='studlab'), 
                     which(colnames(data)=='treatment'),
                     which(colnames(data)=='arm'))]
  data3.31 = data[, c(which(colnames(data)=='n'),
                      which(colnames(data)=='r'))]
  data3.32 = data[, c(which(colnames(data)=='age_mean'),
                      which(colnames(data)=='female_prop'),
                      which(colnames(data)=='anhedonia_baseline_mean'), 
                      which(colnames(data)=='anxiety_baseline_mean'), 
                      which(colnames(data)=='reward_baseline_mean'), 
                      which(colnames(data)=='tx_duration'))]
  data3.41 = aggregate(x = data3.31, by = list(data3.2$studlab, data3.2$treatment, data3.2$arm), FUN = sum)
  data3.42 = aggregate(x = data3.32, by = list(data3.2$studlab, data3.2$treatment), FUN = mean)
  data3.4 = merge(x = data3.41, y = data3.42, by = c('Group.1', 'Group.2'))
  colnames(data3.4) = c('studlab', 
                        'treatment', 
                        'arm', 
                        'n', 
                        'r', 
                        'age_mean', 
                        'female_prop', 
                        'anhedonia_baseline_mean', 
                        'anxiety_baseline_mean', 
                        'reward_baseline_mean', 
                        'tx_duration')
  data3.4 <- data3.4[with(data3.4, order(studlab, arm)), ]
  return(data3.4)
  #return.list = list('data_collapsed' = data3.4)
  #return(return.list)
}

## collapse arms - continuous
collapse.arms.continuous <- function(data) {
  data3.2 = data[, c(which(colnames(data)=='studlab'), 
                     which(colnames(data)=='treatment'),
                     which(colnames(data)=='arm'))]
  data3.31 = data[, which(colnames(data)=='n')]
  data3.32 = data[, c(which(colnames(data)=='mean'), 
                      which(colnames(data)=='sd'), 
                      which(colnames(data)=='age_mean'),
                      which(colnames(data)=='female_prop'),
                      which(colnames(data)=='anhedonia_baseline_mean'), 
                      which(colnames(data)=='anxiety_baseline_mean'), 
                      which(colnames(data)=='reward_baseline_mean'), 
                      which(colnames(data)=='tx_duration'))]
  data3.41 = aggregate(x = data3.31, by = list(data3.2$studlab, data3.2$treatment, data3.2$arm), FUN = sum)
  data3.42 = aggregate(x = data3.32, by = list(data3.2$studlab, data3.2$treatment), FUN = mean)
  data3.4 = merge(x = data3.41, y = data3.42, by = c('Group.1', 'Group.2'))
  colnames(data3.4) = c('studlab', 
                        'treatment', 
                        'arm', 
                        'n', 
                        'mean', 
                        'sd', 
                        'age_mean', 
                        'female_prop', 
                        'anhedonia_baseline_mean', 
                        'anxiety_baseline_mean', 
                        'reward_baseline_mean', 
                        'tx_duration')
  data3.4 <- data3.4[with(data3.4, order(studlab, arm)), ]
  return(data3.4)
  #return.list = list('data_collapsed' = data3.4)
  #return(return.list)
}

## collapse arms - continuous
collapse.arms.continuous.short <- function(data) {
  data3.2 = data[, c(which(colnames(data)=='studlab'), 
                     which(colnames(data)=='treatment'),
                     which(colnames(data)=='arm'))]
  data3.31 = data[, which(colnames(data)=='n')]
  data3.32 = data[, c(which(colnames(data)=='mean'), 
                      which(colnames(data)=='sd'))]
  data3.41 = aggregate(x = data3.31, by = list(data3.2$studlab, data3.2$treatment, data3.2$arm), FUN = sum)
  data3.42 = aggregate(x = data3.32, by = list(data3.2$studlab, data3.2$treatment), FUN = mean)
  data3.4 = merge(x = data3.41, y = data3.42, by = c('Group.1', 'Group.2'))
  colnames(data3.4) = c('studlab', 
                        'treatment',
                        'arm', 
                        'n', 
                        'mean', 
                        'sd')
  data3.4 <- data3.4[with(data3.4, order(studlab, arm)), ]
  return(data3.4)
  #return.list = list('data_collapsed' = data3.4)
  #return(return.list)
}

# drugs ----
IMAO = c('isocarboxazid', 'phenelzine', 'tranylcypromine', 'selegiline')
RIMA = c('brofaromine', 'minaprine', 'moclobemide')
DRI = c('bupropion', 'dextromethorphan-bupropion', 'amitifadine', 'gsk372475', 'methylphenidate')
agonist = c('memantine', 'ropinirole', 'pramipexole')
vescicular = ('rx-10100')

# other ----
version = 2






