###################################################################################
# Script name   : filterAnimalsSex.R
# Date Created  : 20-Mar-2020
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : The function filterAnimalsSex extracts set of animals of the 
#                 specified sex from an input data sets with a set of animals.
#                 
# Description   : The function filterAnimalsSex returns all variables for rows 
#                 from the input table where the sex is equal to the specified sex.
#                 - If the input table contains a SEX column - the set of rows 
#                   matching the specified sex is extracted based on the included SEX 
#                   variable.
#                 - Else the the input table is joined with DM, and the set of rows
#                   matching the specified sex is extracted based on the DM.SEX variable.
#                 The comparison of the sex values are done case insensitive.
#                 
#                 If the input parameter inclUncertain flag is enabled, uncertain animals
#                 are included in the output set.
#                 This uncertain situations are identified and reported (in column UNCERTAIN_MSG):
#                  -  The DM.SEX value is empty or invalid (not CT value - CDISC codelist SEX)
#                 Non-empty UNCERTAIN_MSG values are merged with non-empty UNCERTAIN_MSG values 
#                 which may exist in the input set of animals (animalList).
#                 
# Input         : - A data table containing the input set of animals (input parameter 
#                   'animalList') - the minimum set of variables in the table are:
#                     - STUDYID       - character
#                     - USUBJID       - character
#                 - Domain (imported from the pooled SEND data store if they don't 
#                   exist in workspace): 
#                     - DM  
#                 - CDISC CT code list SEX imported from a CDISC CT file.
#
# Output        : A data table with the character columns:
#                   STUDYID
#                   USUBJID
#                   SEX
#                   UNCERTAIN_MSG - if input parameter inclUncertain flag is enabled
#                 plus any additional columns which may be included in the input data animalList
#
# Parameters    : The function filterAnimalsSex must be called with input parameters: 
#                 - animalList: Mandatory, data table.
#                               The data table to extract data from
#                 - sexFilter:  Mandatory, character.
#                               The value of SEX for extract data for 
#                 - inclUncertain: Optional, boolean.
#                                  Include uncertain rows or not
#
###################################################################################

library(data.table)

filterAnimalsSex<-function(animalList=NULL, sexFilter=NULL, inclUncertain=FALSE) {
  
  # Verify input parameter
  if (!is.data.table(animalList)) {
    stop('Input parameter animalList must have assigned a data table ')
  } 
  if (is.null(sexFilter) | isTRUE(is.na(sexFilter)) | isTRUE(sexFilter=='')) {
    stop('Input parameter sexFilter must have assigned a non-empty value')
  } 
  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter inclUncertain must be either TRUE or FALSE")
  }
  
  if (!exists('DM')) {
    importSENDDomains(c('DM'), unique(animalList[,.(STUDYID)]))
  }
    
  # Merge the input list of animals with DM to add the SEX variable
  animalListSEX<-merge(animalList[,.(STUDYID, USUBJID)], DM[,.(STUDYID, USUBJID, SEX)], by=c("STUDYID", "USUBJID"), all.x=TRUE)

  if (inclUncertain) {
    # Include uncertain rows
    
    # Get values of codelist SEX from CDISC CT
    ctSEX<-getCTCodListValues("SEX")
    
    # Extract the rows matching the specified sex plus the uncertain rows
    foundAnimals<-animalListSEX[toupper(trimws(SEX)) %in% toupper(trimws(sexFilter)) | ! toupper(trimws(SEX)) %in% ctSEX | is.na(USUBJID),
                                .(STUDYID, USUBJID, SEX, UNCERTAIN_MSG=ifelse(!is.na(USUBJID) & ! toupper(trimws(SEX)) %in% ctSEX ,
                                                                              'filterAnimalsSex: DM.SEX does not contain a valid CT value',
                                                                               NA))]
  }
  else {
    # Do not include uncertain rows
    # - extract  the rows matching the specified sex 
    foundAnimals<-animalListSEX[toupper(trimws(SEX)) %in% toupper(trimws(sexFilter))]
  }
    
  # Merge the list of extracted animals with the input set of animals to keep
  # any additional columns from the input table 
  foundAnimals<-merge(foundAnimals, animalList, by=c('STUDYID', 'USUBJID'))
  if ("UNCERTAIN_MSG.y" %in% names(foundAnimals)) {
    # An UNCERTAIN_MSG column is included in both input and found list of animals
    #  - merge the UNCERTAIN_MSG from each of the merged tables into one column
    #  - non-empty messages are separated by '|'
    #  - exclude the original UNCERTAIN_MSG columns after the merge  
    foundAnimals<-foundAnimals[,`:=` (UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y), 
                                                           paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep='|'),
                                                           Coalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)))][, `:=` (UNCERTAIN_MSG.x=NULL,UNCERTAIN_MSG.y=NULL)]
  }
  # Return list of found animals  
  return(foundAnimals)
}