###################################################################################
# Script name   : animalListControl.R
# Date Created  : 23-Jan-2020
# Documentation : <if relevant, reference to specification document>
# Programmer    : Bo Larsen
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
#                 is associated with a TX parameter 'TCNTRL'.
#                 Animals from this set is excluded if they have been treated according 
#                 to EX (EXDOSE > 0 or EXDOSTXT is not empty)
#
# Input         : A data table with a character column named STUDYID (input parameter)
#                 These SEND domains: TX, DM, EX (there are imported from the pooled
#                 SEND store if they don't exist as data tables in the workspace)
#
# Output        : A data table with two character columns:
#                   STUDYID
#                   USUBJID
#
# Parameters    : Contains one input parameter:
#                   studyList - a data table with a character column named STUDYID
#
# Usage notes   : Source the script.
#                 Execute the function GetControlAnimals to get a complete list of 
#                 control animals for a list of studies
#
# !!! MISSING: Filtering of EX based on POOLID !!!
#
###################################################################################

library(data.table)

GetControlAnimals<-function(studyList=NULL) {
  # import missing domains
  if (!exists("TX")) {
    importSENDDomains(c("TX"), studyList)
  }
  if (!exists("DM")) {
    importSENDDomains(c("DM"), studyList)
  }
  if (!exists("EX")) {
    importSENDDomains(c("EX"), studyList)
  }
  # Define sort order of included domains to ensure data can be merged
  setkeyv(TX,c('STUDYID', 'SETCD'))
  setkeyv(DM,c('STUDYID', 'SETCD', 'USUBJID'))
  setkeyv(EX, c('STUDYID', 'USUBJID'))

  if (is.null(studyList)) {
    # No list of studies has been specified - get the control sets for all studies
    txCtrlSet<-TX[TXPARMCD == "TCNTRL", .(STUDYID, SETCD)]
  } 
  else {
    # A list of studies has been specified - get the control sets for these studies
    setkey(studyList, STUDYID)
    txCtrlSet<-subset(merge(TX, studyList), TXPARMCD == "TCNTRL",c('STUDYID', 'SETCD'))
  }
  setkeyv(txCtrlSet,c('STUDYID', 'SETCD'))

  # Extract all the animals from DM which are included in the control sets
  dmCtrlSet<- subset(merge(DM[, .(STUDYID, USUBJID, SETCD)], txCtrlSet),TRUE,c('STUDYID', 'USUBJID'))
  setkeyv(dmCtrlSet, c('STUDYID', 'USUBJID'))

  # Extract all animals from EX which are included in the control sets
  exCtrlSet<-merge(EX,dmCtrlSet, all=FALSE)
  
  # Extract and return the set of DM control animals excluding animals with records in EX
  # where EXDOSE > 0 or EXDOSTXT is non-empty 
  fsetdiff(fsetdiff(dmCtrlSet
                   ,subset(exCtrlSet[,.(MAXDOSE=max(EXDOSE)), by=list(STUDYID,USUBJID)], MAXDOSE > 0 ,c('STUDYID', 'USUBJID') ))
                   ,exCtrlSet[EXDOSTXT != "", .(STUDYID, USUBJID)])
}

