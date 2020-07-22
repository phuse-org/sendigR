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
# Input         : CDISC Terminology file in Excel (xls) format - located in folder
#                 defined in global variable metadataRoot
#
# Output        : Character vector with all code values for the code list specified
#                 in input parameter pCodeList
#
# Parameters    : pCodeList - name of code list to get values for
#
###################################################################################

library(readxl)
library(data.table)

getCTCodListValues<-function(pCodeList=NULL) {
  if (is.null(pCodeList) | isTRUE(is.na(pCodeList)) | isTRUE(pCodeList=='')) {
    stop('Input parameter codeList must have assigned a code list name')
  } 
  
  if (!exists('CDISCctCodeLists')) {
    # The CDISC CT data have not yet being imported - we do it now
    # 
    if (!exists('metadataRoot')) {
      stop('A variable named metadataRoot must be defined an point to location for CDISC CT file(s) in XLS format')
    }
    
    if (!dir.exists(metadataRoot)){
      stop(paste('The folder defined in variable metadataRoot does not exist: ', metadataRoot, sep=''))
    }
    
    # get a list of CT files - name pattern is 'SEND<sep>Terminology<sep><date>.xls', <sep> may be ' ' or '_'
    ctFiles<-list.files(metadataRoot, pattern="SEND[_ ]Terminology.*.\\.xls", ignore.case = TRUE, full.names = TRUE)
    if (length(ctFiles) == 0) {
      stop(paste('No CDISC CT files exists in XLS format in folder ', metadataRoot, sep=''))
    }
        
    # Extract the newest file bases on file naming convention using ISO 8601 date format in <date> part
    ctFile<-max(ctFiles)
    
    # Import content from worksheet named SEND<sep>Terminology<something> - include relevant columns and all rows
    ctSheets<-excel_sheets(ctFile)
    ctAll<-as.data.table(read_xls(ctFile, sheet=ctSheets[grepl('send[_ ]terminology', 
                                                               tolower(ctSheets) )]))[,c("Code", "Codelist Code", "CDISC Submission Value")]
    setnames(ctAll, c("Codelist Code","CDISC Submission Value"),c("CodelistCode","CDISCSubmissionValue"))
    
    # Extract all CDISC CT code list names
    assign('CDISCctCodeLists',ctAll[is.na(CodelistCode), .(CodelistCode=Code, CodeList=CDISCSubmissionValue)], envir=.GlobalEnv)
    
    
    # Extract all CDISC CT code list values
    assign('CDISCctCodeValues',ctAll[!is.na(CodelistCode), .(CodelistCode,CDISCSubmissionValue)], envir=.GlobalEnv)
  }
  
  # Check if the requested code list exists
  if (!toupper(pCodeList) %in% CDISCctCodeLists$CodeList) {
    stop('The specified code list does not exist in the CDISC terminolgy file')
  }
  
  # Extract and return a character vector with all value for the requested code list
  return(merge(CDISCctCodeLists[CodeList==toupper(pCodeList), .(CodelistCode)], CDISCctCodeValues[!is.na(CodelistCode)])$CDISCSubmissionValue);
}