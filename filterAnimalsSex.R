###################################################################################
# Script name   : filterAnimalsSex.R
# Date Created  : 20-Mar-2020
# Documentation : <if relevant, reference to specification document>
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
# Input         : - A data table containing the input set of animals (input parameter 
#                   'animalList') - the minimum set of variables in the table are:
#                     - STUDYID       - character
#                     - USUBJID       - character
#                 - Domain (imported from the pooled SEND data store if they don't 
#                   exist in workspace): 
#                     - DM  
#
# Output        : A data table containing the rows matching the specified input values
#                 of SEX in the input data table.
#                 The data table contains the same variables as the input data table.
#
# Parameters    : The function filterAnimalsSex must be called with input parameters: 
#                 - animalList: Mandatory, data table
#                               The data table to extract data from
#                 - sexFilter:  Mandatory, character
#                               The value of SEX for extract data for
#
# Usage notes   : Example - extract mail animals from list of all control animals:
#                   filterAnimalsSex(controlAnimals, "M")
# MISSING: 
#   - Handling of pooled data
###################################################################################

library(data.table)

filterAnimalsSex<-function(animalList=NULL, sexFilter=NULL, inclUncertain=FALSE) {
  
  
  ##################################################################################
  # Hard code CT list of SEX - should be read from a CT input file
  ctSEX=c("F","M","U","UNDIFFERENTIATED")
  ##################################################################################

  # Verify input parameter
  if (is.null(animalList) | isTRUE(is.na(animalList)) | isTRUE(animalList=='')) {
    stop('Input parameter animalList must have assigned a data table ')
  } 
  if (is.null(sexFilter) | isTRUE(is.na(sexFilter)) | isTRUE(sexFilter=='')) {
    stop('Input parameter sexFilter must have assigned a data table ')
  } 
  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter inclUncertain must be either TRUE or FALSE")
  }
  
  if (!exists('DM')) {
    importSENDDomains(c('DM'), animalStudies)
  }
    
  # Merge the input list of animals with DM to add the SEX variable
  animalListSEX<-merge(animalList[,.(STUDYID, USUBJID)], DM[,.(STUDYID, USUBJID, SEX)], by=c("STUDYID", "USUBJID"), all.x=TRUE)

  if (inclUncertain) {
    # Include uncertain rows
    #  - extract the rows matching the specified sex plus the uncertain rows
    foundAnimals<-animalListSEX[toupper(trimws(SEX)) %in% toupper(trimws(sexFilter)) | ! toupper(trimws(SEX)) %in% ctSEX | is.na(USUBJID),
                                .(STUDYID, USUBJID, SEX, UNCERTAIN_MSG=ifelse(!is.na(USUBJID) & ! toupper(trimws(SEX)) %in% ctSEX ,
                                                                              'filterAnimalsSex: DM.SEX does not contain a valid CT value',
                                                                               NA))]
  }
  else {
    # Do not include uncertain rows
    # - extract  the rows matching the specified sex 
    animalListSEX[toupper(trimws(SEX)) %in% toupper(trimws(sexFilter))]
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