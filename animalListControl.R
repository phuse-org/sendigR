###################################################################################
# Script name   : animalListControl.R
# Date Created  : 23-Jan-2020
# Documentation : <if relevant, reference to specification document>
# Programmer    : Bo Larsen - extended by Daniel P. Russo
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Extract a list of control animals for a given list of studies
#
# Description   : A function GetControlAnimals is defined:
#                 Returns a data table with a list of control animals for a given
#                 list of studies (se Output for the structure of output tab).
#                 The set of animals contains all animals from DM where the SETCD 
#                 is associated with a TX parameter 'TCNTRL'. Negative control 
#                 animals are further defined by 
#                   1) containing a word from a set of words, like placebo, to 
#                      automatically distinguish it as a negative control 
#                 or 
#                   2) contains words from two sets, defined in the beginning of 
#                      the script as currentModifiers and currentCntrlNouns
#
# Input         : A data table with a character column named STUDYID (input parameter)
#                 These SEND domains: TX, DM (there are imported from the pooled
#                 SEND store if they don't exist as data tables in the workspace)
#
# Output        : A data table with two character columns:
#                   STUDYID
#                   USUBJID
#
# Parameters    : Contains one input parameter:
#                   studyList - a data table with a character column named STUDYID
#
#
###################################################################################

library(data.table)

GetControlAnimals<-function(studyList=NULL) {
  
  # Definition of the search words for negative control terms
  standAlonesWords <- c('placebo', 'untreated', 'sham')
  currentModifiers <- c('negative', 'saline', 'peg', 'vehicle', 'citrate', 'dextrose', 'water', 'air')
  currentCntrlNouns <- c('item', 'control', 'article')
  
  ###################################################################################
  # Check whether the input string setNames contains a value indictaion it's a negative 
  # control group
  ###################################################################################
  HasNegativeControlTerms<-function(setName, controlMods=currentModifiers, controlNouns=currentCntrlNouns) {
    
    # trim white space 
    setName <- trimws(setName)
    
    # prepare strings in regrex format 
    standAlongString <- paste(standAlonesWords, collapse = '|')
    controlModsString <- paste(controlMods, collapse = '|')
    controlNounsString <- paste(currentCntrlNouns, collapse = '|')
    
    # if the string matches any of  the words
    # in the currentModisiers vector exactly
    # it should be treated as a negative control
    if (tolower(setName) %in% currentModifiers) {
      return(TRUE)
    }
    
    # the logic in the code below first  
    # checks if the string contains any word
    # like 'placebo' to delineate it 
    # as a negative control group 
    # the second if block check if it contains
    # words from both the 'currentCntrlNouns' and 
    # 'currentModifiers'
    
    if (grepl(standAlongString, setName, ignore.case=TRUE)) {
      return(TRUE)
    }  else if ((grepl(controlModsString, setName, ignore.case=TRUE)) & (grepl(controlNounsString, setName, ignore.case=TRUE))){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  ###################################################################################
  
  # import missing domains
  if (!exists('TX')) {
    importSENDDomains(c('TX'), studyList)
  }
  if (!exists('DM')) {
    importSENDDomains(c('DM'), studyList)
  }

  # Define sort order of included domains to ensure data can be merged
  setkeyv(TX,c('STUDYID', 'SETCD'))
  setkeyv(DM,c('STUDYID', 'SETCD', 'USUBJID'))

  if (is.null(studyList) | isTRUE(is.na(studyList)) | isTRUE(studyList=='')) {
    # No list of studies has been specified - get the control sets for all studies
    txCtrlSet<-TX[TXPARMCD == 'TCNTRL', .(STUDYID, SETCD, TXVAL)]
  } 
  else {
    # A list of studies has been specified - get the control sets for these studies
    setkey(studyList, STUDYID)
    txCtrlSet<-subset(merge(TX, studyList), TXPARMCD == 'TCNTRL',c('STUDYID', 'SETCD', 'TXVAL'))
  }
  setkeyv(txCtrlSet,c('STUDYID', 'SETCD'))
  
  # remove set names that don't meet
  # our match pattern 
  txCtrlSet <- txCtrlSet[sapply(txCtrlSet$TXVAL, HasNegativeControlTerms), .(STUDYID, SETCD)]
  
  # match the animals in DM to those in DM and 
  # return only the economics 
  dmCtrlSet<- subset(merge(DM[, .(STUDYID, USUBJID, SETCD)], txCtrlSet),TRUE,c('STUDYID', 'USUBJID', 'SETCD'))
  setkeyv(dmCtrlSet, c('STUDYID', 'USUBJID'))
   
  return(unique(dmCtrlSet[, .(STUDYID, USUBJID)]))

}
