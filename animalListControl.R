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
  # Default input param,eter values set to indentify negative control groups
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

  # Define sort order of included domains to ensure data can be merged
  #setkeyv(TX,c('STUDYID', 'SETCD'))
  #setkeyv(DM,c('STUDYID', 'SETCD', 'USUBJID'))

  # Get the control sets for all studies
  txCtrlSet <- TX[TXPARMCD == 'TCNTRL', .(STUDYID, SETCD, TCNTRL=TXVAL)]
  if (inclUncertain) {
    # Include uncertain studies/animals - add studies with no TXPARM 'TCNTRL' to list of control sets
    txCtrlSet <- rbindlist(list(txCtrlSet, 
                                fsetdiff(unique(TS[,.(STUDYID)]), 
                                         txCtrlSet[,.(STUDYID)])[,.(STUDYID, SETCD = NA, TCNTRL = NA)]))
  }
  
  studyListIncl<-FALSE
  if ( ! (is.null(studyList) | isTRUE(is.na(studyList)) | isTRUE(studyList==''))) {
    # A list of studies has been specified - limit the list the control sets for these studies
    txCtrlSet<-merge(txCtrlSet, studyList[,.(STUDYID)], by='STUDYID')
    studyListIncl<-TRUE
  }
  #setkeyv(txCtrlSet,c('STUDYID', 'SETCD'))
  
  # Get set of control groups identified as negative 
  foundCtrlSet <- txCtrlSet[sapply(txCtrlSet$TCNTRL, HasControlTerms)]
  
  if (inclUncertain) {
    ## Include uncertain studies/animals 
    
    # Get the control groups not identified as negative
    notNegCtrlSet <- fsetdiff(txCtrlSet, foundCtrlSet)
    # Get the uncertain control groups  - i.e. control set not identified as negative or positive
    uncertainCtrlSet<-fsetdiff(notNegCtrlSet, 
                               # Set of control groups identified as positive
                               notNegCtrlSet[sapply(notNegCtrlSet$TCNTRL, HasControlTerms,NA,posModifiers)])
    
    # Set the UNCERTAIN_MSG for the control groups identified as uncertain
    # (is.null(SETCD) | isTRUE(is.na(SETCD)) | isTRUE(SETCD=='')
    uncertainCtrlSet[, `:=` (UNCERTAIN_MSG = ifelse(is.na(TCNTRL),
                                                    'GetControlAnimals: TX parameter TCNTRL is missing',
                                                    'GetControlAnimals: Cannot decide type of control group from TXVAL in TX parameter TCNTRL'))]
    # Combine set of negative and uncertain control groups
    foundCtrlSet[,`:=` (UNCERTAIN_MSG = NA)]
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
