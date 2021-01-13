###################################################################################
# Script name   : miscFunctions.R
# Date Created  : 18-Jun-2020
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Miscellaneous functions 
#
# Description   : Contains small functions - see header for each function for details
#
###################################################################################

###################################################################################
# Function name : getCTCodListValues
#
# Purpose       : Get values for CDISC Terminology code list 
#
# Description   : Extracts and return a list (vector) of code values for the CDISC 
#                 code list specified in call of function
#
# Input         : Data table with CDISC Terminology importred from CT file 
#
# Output        : Character vector with all code values for the code list specified
#                 in input parameter pCodeList
#
# Parameters    : pCodeList - name of code list to get values for
#
###################################################################################

library(data.table)

getCTCodListValues<-function(pCodeList=NULL) {
  if (is.null(pCodeList) | isTRUE(is.na(pCodeList)) | isTRUE(pCodeList=='')) {
    stop('Input parameter codeList must have assigned a code list name')
  } 
  
  # Check if the requested code list exists
  if (!toupper(pCodeList) %in% CDISCctCodeLists$CodeList) {
    stop('The specified code list does not exist in the CDISC terminolgy file')
  }
  
  # Extract and return a character vector with all value for the requested code list
  return(merge(CDISCctCodeLists[CodeList==toupper(pCodeList), .(CodelistCode)],
               CDISCctCodeValues[!is.na(CodelistCode)])$CDISCSubmissionValue);
}