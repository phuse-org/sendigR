###################################################################################
# Script name   : Q9POSFreqCounts.R
# Date Created  : 13-Feb-2020
# Documentation : N/A
# Programmer    : Daniel P. Russo
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Generate Frequency Counts for with in the Q9 database:
#               
# Description   : <detailed description of the algorithm(s) implemented in the script>
#
# Input         : <list of input data - i.e. SEND domains and other potiential input 
#                 files and/or data frames etc.>
#
# Output        : <list of output data - files, data frames or other kind of generated 
#                  output>
#
# Parameters    : <if relevant - list of user specified input parameters to be applied
#                 - e.g. for a function created by the script 
#
# Usage notes   : <how to use the script - e.g. the preconditions to be fulfilled 
#                 before script execution>
#
###################################################################################

source("importSENDDomains.R")
library(dplyr)

# Taken from Bill's email
# Q1.	how frequently you see each of the distinct values of TA.EPOCH?  
#          (this will enable us to determine if we can map these to Screening, Dosing, and Recovery)

freqCountsEpoch <- function() {
  
  # grab all the values from the TA domain 
  # this function makes available a global 
  # variable named after the query domain
  # e.g., TA 
  importSENDDomains(c("TA"))
  
  
  # currently dealing with duplicated studies from
  # but this should be a quick and dirty way to take 
  # care of it 
  unique_TA <- unique(TA)
  
  # get all epochs 
  freq_counts <- table(unique_TA$EPOCH)
  
  write.csv(freq_counts, 'data/epoch_freq_counts.csv')
  
}

# Q1.	2.	how frequently you see each of the distinct values of DSDECODE? 
# (This will give us a data bases for trying to find an alternative approach to determining study phase if this approach fails.)


freqCountsDECode <- function() {
  
  # grab all the values from the DS domain 
  # this function makes available a global 
  # variable named after the query domain
  # e.g., TA 
  importSENDDomains(c("DS"))
  
  
  # currently dealing with duplicated studies from
  # but this should be a quick and dirty way to take 
  # care of it 
  unique_DS <- unique(DS)
  
  # get all epochs 
  freq_counts <- table(unique_DS$DSDECOD)
  
  write.csv(freq_counts, 'data/DECode_freq_counts.csv')
  
}


matchesEpoch <- function(epoch, search_terms) {
  # Takes a query term, epoch will return true
  # if any term in 'search_terms' is a substring
  # of they query 
  
  for (s_term in search_terms) {
    isMatch <- grepl(s_term, epoch, fixed=TRUE)
    
    if (isMatch == TRUE) {
      return(TRUE)
    }

  }
  return(FALSE)  
}


inferEpoch <- function(epoch) {
  # will standardize and epoch by trying to get 
  # what the non controlled terminology means
  
  # some possible screening terms 
  screening_terms <- c('pre', 'screen', 'acclimat', 'observ', 'non')
  
  # some possible dosing terms 
  dosing_terms <- c('dos', 'treat', 'trt')
  
  # some possible recovery terms 
  recovery_terms <- c('recov')
  
  epoch_lower <- tolower(epoch)
  
  if (matchesEpoch(epoch_lower, screening_terms)) {
    return('Screening')
  }
  
  if (matchesEpoch(epoch_lower, dosing_terms)) {
    return('Dosing')
  }
  
  if (matchesEpoch(epoch_lower, recovery_terms)) {
    return('Recovery')
  }
  
  return('Ambiguous')
  
}  
  
mapEpochsToPhase <- function() {
  
  
  # grab all the values from the TA domain 
  # this function makes available a global 
  # variable named after the query domain
  # e.g., TA 
  importSENDDomains(c("TA"))
  
  
  # currently dealing with duplicated studies from
  # but this should be a quick and dirty way to take 
  # care of it 
  unique_TA <- unique(TA)
  
  epochs <- unique_TA$EPOCH
  
  # create a list for storing unique elements
  epoch_list <- list(Screening = c(),
                     Dosing = c(),
                     Recovery = c(),
                     Unmaped = c())
  

  # some possible screening terms 
  screening_terms <- c('pre', 'screen', 'acclimat', 'observ', 'non')
  
  # some possible dosing terms 
  dosing_terms <- c('dos', 'treat', 'trt')
  
  # some possible recovery terms 
  recovery_terms <- c('recov')
  
  for (epoch in epochs) {

    epoch_lower <- tolower(epoch)
    
    # match the epoch to the correct "bin"
    
    if (matchesEpoch(epoch_lower, screening_terms)) {
      epoch_list[['Screening']] <- c(epoch_list[['Screening']], epoch)
      next
    }
    
    if (matchesEpoch(epoch_lower, dosing_terms)) {
      epoch_list[['Dosing']] <- c(epoch_list[['Dosing']], epoch)
      next
    }
    
    if (matchesEpoch(epoch_lower, recovery_terms)) {
      epoch_list[['Recovery']] <- c(epoch_list[['Recovery']], epoch)
      next
    }
    
    epoch_list[['Unmapped']] <- c(epoch_list[['Unmapped']], epoch)
    
  }
  
  
  for (phase in c("Screening", "Dosing", "Recovery", "Unmapped")) {
    
    tally = table(epoch_list[phase])
    write.csv(tally, sprintf('data/%s_term_freq_counts.csv', phase))
  }
  
}


findingInPhase <- function(df, time) { 
    
  time <- as.Date(time)
  
  end <- as.Date(df['SEENDTC'])
  
  if (time <= end) {
    return(TRUE)
  } else {
    return(FALSE)
  }
    
    
}



testTrialDesign <- function() {  
  rm(list=ls()) 
  # right now only testing for MI 
  finding_domains <- c('MI')
  
  one_domain <- sample(finding_domains, 1)
  
  domains <- c('MI', 'SE', 'DM', 'TA')
  
  importSENDDomains(domains)
  
  random_finding <- MI[sample(nrow(MI), 1),]
  
  SE <- distinct(SE)
  
  setkey(random_finding,USUBJID)
  setkey(SE, USUBJID)
  SE <- random_finding[SE, nomatch=0]
  
  time_of_finding <- as.Date(random_finding$MIDTC)
  
  days_since_phase_end <- as.Date(SE$SEENDTC) - time_of_finding
  
  # get the minimum positive value from the last phase
  
  closest_time <- min(days_since_phase_end[days_since_phase_end >= 0])
  
  # use a mask get the code of the closest phase
  
  etcd <- SE[(days_since_phase_end == closest_time), ]$ETCD
  
  setkey(random_finding, STUDYID)
  setkey(TA, STUDYID)
  TA <- distinct(random_finding[TA, nomatch=0])
  
  studyid <- TA$STUDYID[1]
  
  setkey(random_finding, USUBJID)
  setkey(DM, USUBJID)
  DM <- distinct(random_finding[DM, nomatch=0])
  
  armcd <- DM$ARMCD
  
  usubjid <- random_finding$USUBJID
  epoch <- TA[((TA$ARMCD == armcd) & (TA$ETCD == etcd))]$EPOCH
  
  
  infered_epoch <- inferEpoch(epoch)
  
  
  return(c(studyid, usubjid, epoch, infered_epoch))
    
}  

  
df <- data.frame()

for (i in 1:10) {
  result <- testTrialDesign()
  df <- rbind(df, result)
  print(result)
}

write.csv(df, 'data/test_results.csv')
