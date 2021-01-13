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
#                 If the input parameter inclUncertain flag is enabled, uncertain animals
#                 are included in the output set.
#                 These uncertain situations are identified and reported (in column UNCERTAIN_MSG):
#                   - TX parameter 'TCNTRL' is missing
#                   - TXVAL for TX parameter 'TCNTRL' cannot be identified as Negative or Positive control
#                   
# Input         : - A data table specified in the input parameter studylList:
#                   It contains the list of studies extract control animals for
#                   - must contain these character variables:
#                       STUDYID
#                     other variables may be included
#                 - The TX and DM domains - are imported from the pooled SEND data 
#                   store if they don't exist in workspace.
#
# Output        : A data table with the character columns:
#                   STUDYID
#                   USUBJID
#                   TCNTRL (value of TXVAL for TX parameter TCNTRL)
#                   UNCERTAIN_MSG - if input parameter inclUncertain flag is enabled
#                 plus any additional columns which may be included in the input data studylList
#
# Parameters    : studyList:      Mandatory, data table (see Input)
#                 inclUncertain:  Optional, boolean.
#                                   Include uncertain rows or not
#
###################################################################################

library(data.table)

GetControlAnimals<-function(studyList=NULL, inclUncertain=FALSE) {
  
  # Definition of the search words for negative control terms
  negStandAlonesWords <- c('placebo', 'untreated', 'sham')
  negModifiers <- c('negative', 'saline', 'peg', 'vehicle', 'citrate', 'dextrose', 'water', 'air')
  
  # Definition of the search words for negative control terms
  posModifiers <- c('positive','reference')
  
  # Definition of the search words for common for negative/positive control terms
  allCntrlNouns <- c('item', 'control', 'article')
  
  ###################################################################################
  # Check whether the input string setNames contains a value indicating it's a  
  # negative or positive control group.
  # Default input parameter values set to identify negative or positive control groups
  ###################################################################################
  HasControlTerms<-function(setName, standAlonesWords=negStandAlonesWords, 
                                     controlMods=negModifiers, 
                                     controlNouns=allCntrlNouns) {
    # trim white space and convert to lowercase
    setName <- trimws(tolower(setName))
    
    # prepare strings in regrex format 
    if (! is.null(standAlonesWords)) {
      standAlongString <- paste(standAlonesWords, collapse = '|')
    }
    controlModsString <- paste(controlMods, collapse = '|')
    controlNounsString <- paste(controlNouns, collapse = '|')
    
    # if the string matches any of  the words
    # in the controlMods vector exactly
    # it should be treated as a relevant control
    if (setName %in% controlMods) {
      return(TRUE)
    }
    
    # the logic in the code below first  
    # checks if the string contains any standalone words
    # to delineate it  as a relevant control group 
    # the second if block check if it contains
    # words from both the 'controlNouns' and 
    # 'controlMods'
    
    if (! is.null(standAlonesWords) & grepl(standAlongString, setName)) {
      return(TRUE)
    } 
    if ((grepl(controlModsString, setName)) & (grepl(controlNounsString, setName))) {
      return(TRUE)
    } 
    return(FALSE)
  }
  ###################################################################################
  
  # Verify input parameter
  if (!is.data.table(studyList)) {
    stop('Input parameter studyList must have assigned a data table ')
  } 
  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter inclUncertain must be either TRUE or FALSE")
  }
  
  # import missing domains
  if (!exists('TS')) {
    importSENDDomains(c('TS'), studyList)
  }
  if (!exists('TX')) {
    importSENDDomains(c('TX'), studyList)
  }
  if (!exists('DM')) {
    importSENDDomains(c('DM'), studyList)
  }

  # Get the control sets for all studies
  txCtrlSet <- TX[TXPARMCD == 'TCNTRL', .(STUDYID, SETCD, TCNTRL=TXVAL)]
  if (inclUncertain) {
    # Include uncertain studies/animals - add studies with no TXPARM 'TCNTRL' to list of control sets
    txCtrlSet <- rbindlist(list(txCtrlSet, 
                                fsetdiff(unique(TS[,.(STUDYID)]), 
                                         txCtrlSet[,.(STUDYID)])[!is.na(STUDYID),.(STUDYID, SETCD = as.character(NA), TCNTRL = as.character(NA))]))
  }
  
  studyListIncl<-FALSE
  if ( is.data.table(studyList)) {
    # A list of studies has been specified - limit the list the control sets for these studies
    txCtrlSet<-merge(txCtrlSet, studyList[,.(STUDYID)], by='STUDYID')
    studyListIncl<-TRUE
  }

  # Get set of control groups identified as negative
  foundCtrlSet <- txCtrlSet[FALSE] 
  if (txCtrlSet[,.N] > 0)
    foundCtrlSet <- txCtrlSet[sapply(txCtrlSet$TCNTRL, HasControlTerms)]
    
  
  if (inclUncertain) {
    ## Include uncertain studies/animals 
    
    # Get the control groups not identified as negative
    notNegCtrlSet <- fsetdiff(txCtrlSet, foundCtrlSet)
    # Get the uncertain control groups  - i.e. control set not identified as negative or positive
    uncertainCtrlSet <- notNegCtrlSet[FALSE]
    if (notNegCtrlSet[,.N] > 0)
      uncertainCtrlSet<-fsetdiff(notNegCtrlSet, 
                                 # Set of control groups identified as positive
                                 notNegCtrlSet[sapply(notNegCtrlSet$TCNTRL, HasControlTerms,NA,posModifiers)])
    
    # Set the UNCERTAIN_MSG for the control groups identified as uncertain
    uncertainCtrlSet[, `:=` (UNCERTAIN_MSG = ifelse(is.na(TCNTRL),
                                                    'GetControlAnimals: TX parameter TCNTRL is missing',
                                                    'GetControlAnimals: Cannot decide type of control group from TXVAL in TX parameter TCNTRL'))]
    # Combine set of negative and uncertain control groups
    foundCtrlSet[,`:=` (UNCERTAIN_MSG = as.character(NA))]
    foundCtrlSet<-rbindlist(list(foundCtrlSet, uncertainCtrlSet))
    
    # Get the list of animals belong to the identified control groups - incl. uncertain control groups which doesn't match any rows in DM
    dmCtrlSet <- merge(DM[, .(STUDYID, USUBJID, SETCD)], foundCtrlSet, by=c('STUDYID', 'SETCD'), all.y=TRUE)[,.(STUDYID, USUBJID, TCNTRL, UNCERTAIN_MSG)]
    
    # Merge with the studyList to include all study level attributes and accumulate eventual UNCERTAIN_MSG texts 
    dmCtrlSet <- merge(dmCtrlSet, studyList, by='STUDYID')
    if ('UNCERTAIN_MSG.y' %in% names(dmCtrlSet)) {
      dmCtrlSet<-dmCtrlSet[,`:=` (UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y), 
                                                       paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep='|'),
                                                       Coalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)))][, `:=` (UNCERTAIN_MSG.x=NULL,UNCERTAIN_MSG.y=NULL)]
    }
  }
  else {
    # Get the list of animals belong to the identified control groups
    dmCtrlSet <- merge(DM[, .(STUDYID, USUBJID, SETCD)], foundCtrlSet, by=c('STUDYID', 'SETCD'), all.y=TRUE)[,.(STUDYID, USUBJID, TCNTRL)]
    # Merge with the studyList to include all study level attributes 
    dmCtrlSet <- merge(dmCtrlSet, studyList, by='STUDYID')
  }

  return(dmCtrlSet)

}
