###################################################################################
# Script name   : subjDataExtract.R
# Date Created  : 20-Feb-2020
# Documentation : <if relevant, reference to specification document>
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
# Input         : - The domain specified in input parameter domain (imported from the 
#                   pooled SEND data store if it doesn't exist in workspace).
#                 - Data table with list of animals to extract data for (input parameter
#                   animalList) - must contain these variables:
#                     - STUDYID       - character
#                     - USUBJID       - character
#
# Output        : The function ExtractSubjData returns a data table containing the set 
#                 of rows from the input domain for the animals contained in the input list.
#                 The structure (included variables) is equal to the input domain. 
#
# Parameters    : The function ExtractSubjData conatins to input parameters:
#                   domain:     Mandatory, character
#                               The name of the domain to extract rows from.
#                   animalList: Manadatory, data table
#                               The table with the animals to extract data for
#
# Usage notes   : <how to use the script - e.g. the preconditions to be fulfilled 
#                 before script execution>
#
# MISSING:
#   - A posibilty to limit the set of variables to be extracted
#   - complete valiadtion of the input parameters
#   - Handling of pooled data
###################################################################################

ExtractSubjData<-function(domain, animalList) {
  if (is.null(animalList) | is.null(domain)) {
    print("ERROR: A domain name and a data table with list of animals must be specified")
  } 
  else {
    domain<-toupper(domain)
    if (!exists(domain)) {
      importSENDDomains(domain, unique(animalList[,.(STUDYID)]))
    }
    merge(setkeyv(get(domain),c('STUDYID', 'USUBJID')), setkeyv(animalList,c('STUDYID', 'USUBJID')))
  }
}

