###################################################################################
# Script name   : subjDataExtract.R
# Date Created  : 20-Feb-2020
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Extract all rows and columns from a specified finding domain for 
#                 a specified list of animals
#
# Description   : The function ExtractSubjData extracts and returns all rows and 
#                 columns from the specified domain (input parameter domain) for the 
#                 set of animals included in the given input table (input parameter
#                 animalList)  
#
# Input         : - The domain specified in input parameter and  POOLDEF - they are 
#                   imported from the pooled SEND data store if they don't exist in 
#                   workspace.
#                 - The domain specified in input parameter domain (imported from the 
#                   pooled SEND data store if it doesn't exist in workspace).
#                 - Data table with list of animals to extract data for (input parameter
#                   animalList) - must contain at least these variables:
#                     - STUDYID       - character
#                     - USUBJID       - character
#
# Output        : The function ExtractSubjData returns a data table containing the set 
#                 of rows from the input domain for the animals contained in the input list.
#                 Both subject level (i.e. rows where USUBJID is not empty) and pool level 
#                 (i.e rows POOLID is not empty) data are included in the extracted set.
#                 The structure (included variables) is equal to the input domain. 
#
# Parameters    :   domain:     Mandatory, character
#                               The name of the domain to extract rows from.
#                   animalList: Mandatory, data table
#                               The table with the animals to extract data for
#                               
###################################################################################

library(data.table)

ExtractSubjData<-function(domain=NULL, animalList=NULL) {
  if (is.null(domain) | isTRUE(is.na(domain)) | isTRUE(domain=='')) {
    stop('Input parameter domain must have assigned a domain name ')
  }  
  
  if (!is.data.table(animalList)) {
    stop('Input parameter animalList must have assigned a data table ')
  } 
  
  if (!exists('POOLDEF')) {
    importSENDDomains('POOLDEF', unique(animalList[,.(STUDYID)]))
  }
  domain<-toupper(trimws(domain))
  if (!exists(domain)) {
    importSENDDomains(domain, unique(animalList[,.(STUDYID)]))
  }
  domainDT<-get(domain)
  
  # Extract subject level data for the input list of animals
  SubjData<-merge(domainDT[!(is.null(USUBJID) | USUBJID == '')], 
                  animalList[,.(STUDYID, USUBJID)],
                  by=c('STUDYID', 'USUBJID'))
  
  if ('POOLID' %in% names(domainDT)) {
    # Extract pool level data 
    #  - input list of animals is joined to POOLDEF to get the related POOLID values
    SubjData<-rbindlist(list(SubjData, 
                             merge(domainDT[!(is.null(POOLID) | POOLID == '')], 
                                   merge(animalList[,.(STUDYID, USUBJID)], 
                                         POOLDEF, 
                                         by=c('STUDYID', 'USUBJID'))[,.(STUDYID, POOLID)],
                                   by=c('STUDYID', 'POOLID'))), 
                        use.names=TRUE, fill=TRUE)
  } 
  
  # Merge the extracted findings with the input set of animals to keep
  # any additional columns from the input table - return rows
  return(merge(SubjData, animalList, by=c('STUDYID', 'USUBJID')))
}

