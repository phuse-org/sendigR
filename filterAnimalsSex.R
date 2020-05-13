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

filterAnimalsSex<-function(animalList, sexFilter) {
  if (any(names(animalList) == "SEX")) {
    # SEX column is included in input table
    #  -extract and return the rows matching the specified sex
    reuturn(animalList[toupper(SEX)==toupper(sexFilter)])
  } else {
    # No SEX column is included in input table
    # - extract DM rows for animals in input table
    #  - merge the two tables 
    #  - extract the rows matching the specified sex
    #  - return rows with all the variables included in input table (i.e. SEX is deleted)
    return(merge(setkeyv(ExtractSubjData("DM",animalList)[,.(STUDYID,USUBJID,SEX)], c("STUDYID", "USUBJID")),
                 setkeyv(animalList, c("STUDYID", "USUBJID")))[toupper(SEX)==toupper(sexFilter)][,SEX:=NULL])
  }
  
}