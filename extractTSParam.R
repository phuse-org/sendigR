###################################################################################
# Script name   : extractTSParam.R
# Date Created  : 01-Apr-2020
# Documentation : n/a
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Extract the value(s) of one or more TS parameters for a given
#                 list of studies
#
# Description   : Two functions are defined:
# 
#                 - extractTSParam:
#                   Extracts and returns as set of rows from domain TS 
#                   - rows fulfilling these criterias are included
#                     - STUDYID is with the input set of studyid value 
#                       (data table given in input parameter 'studies').
#                     - Values of TSPARMCD matches the given input parameter 
#                       'tsparmcd' or values of TSPARM matches the given input 
#                       parameter 'tsparmcd'.
#                       The filtering of TSPARMCD/TSPARM is done as regular expression
#                       searches if the input parameter includes one or more '*' (wildcard) 
#                       - and '*' is executesd as '.*', i.e. '0 or more occurences of any character'.
#                       The filtering is done case insensitive.
#                   One rows are returned per found TS row.
#                   
#                 - extractTSParam2:
#                   Extracts and returns as set of rows from domain TS. 
#                   The function extractTSParam is called to extract a set of TS rows
#                   fulfilling the set of given input parameters.
#                   The extracted rows are grouped in this way and returned:
#                     - One row is returned per distinct combination of 
#                       STUDYID, TSPARMCD, TSPARM
#                     - For each combination of STUDYID, TSPARMCD, TSPARM with multiple
#                       TS rows, the TSVAL value for all rows are concatenated into one 
#                       TSVAL column in this format:
#                         <TSGRPID>: <TSVAL> [ - <TSGRPID>: <TSVAL>]
#                       where the part in [] is zero or more occurneces of TSGRPIP/TSVAL 
#                       values per STUDYID, TSPARMCD, TSPARM. 
#
# Input         : Both functions: a data table containing the input set of studyids (input parameter 
#                   'studies') - the minimun set of variables in the table are:
#                     - STUDYID       - character
#                 - Domain (imported from the pooled SEND data store if they don't 
#                   exist in workspace): 
#                     - TS  
#
# Output        : Function extractTSParam:
#                   A data table with the extracted TS rows with these character 
#                   columns - one row per studyid/TS parameter/TSVAL:
#                     - STUDYID
#                     - TSGRPID
#                     - TSPARMCD
#                     - TSPARM
#                     - TSVAL
#                 Function extractTSParams:
#                   A data table with the extracted TS rows with these character 
#                   columns - one row per studyid/TS parameter:
#                     - STUDYID
#                     - TSPARMCD
#                     - TSPARM
#                     - TSVAL
#                     where TSVAL contains all TSVAL values per STUDYID/TS parameter 
#                     combination.
#                    
# Parameters    : Both functions are defined with these input parameters:
#                   studies   - mandatory, data table
#                               Contains the list of studies to extract TS parameters for
#                   tsparmcd  - optional, character *)
#                               A TSPARMCD text to search for
#                   tsparm    - optional, character *)
#                               A TSPARM text to search for
#                     *)  Either tsparmcd or tsparm must be specified with a 
#                         non-empty value.
#                         '*' is used as optional wildcard in the spefified value of 
#                         tsparmcd/tsparm.
#
# Usage notes   : Examples
#                   - Extract values of Treatment Vehicle - using part of TSPARM text:
#                       extractTSParam(studiesAll, tsparm='*vehicle*')
#                   - Extract values of Treatment Vehicle - using exact TSPARMCD value:
#                       extractTSParam(studiesAll, 'TRTV')
#                   - Extract values of Treatment - combine multiple values into 
#                     one row:
#                       extractTSParam2(studiesAll, 'TRT')
#                   - Extract values of all treatment related parameters - both as 
#                     one row per value and combining multiple values into one row:
#                       extractTSParam(studiesAll, 'TRT*')
#                       extractTSParam2(studiesAll, 'TRT*')
#                     
###################################################################################

extractTSParam<-function(studies=NULL, tsparmcd=NULL, tsparm=NULL) {
  # Evaluate input parameters
  if (is.null(studies) | isTRUE(is.na(studies)) | isTRUE(studies=="")) {
    stop("A table with list if studyids must be specified")
  }
  if (((is.null(tsparmcd) | isTRUE(is.na(tsparmcd)) | isTRUE(tsparmcd=="")) & (is.null(tsparm) | isTRUE(is.na(tsparm)) | isTRUE(tsparm==""))) |
      (!(is.null(tsparmcd) | isTRUE(is.na(tsparmcd)) | isTRUE(tsparmcd=="")) & !(is.null(tsparm) | isTRUE(is.na(tsparm)) | isTRUE(tsparm=="")))) {
    # Either none or bot tsparm* parameters has been specified
    stop("A value must be specified for either TSPARMCD or TSPARM")
  } 
  if (!exists("TS")) {
    # import TS if it's not already exists
    importSENDDomains(c("TS"))
  }
  
  # Extract all TS rows for the given list of studies
  studyTS<-merge(setkeyv(TS[,.(STUDYID, TSGRPID, TSPARMCD, TSPARM, TSVAL)],c("STUDYID")), 
                 setkeyv(studies[,.(STUDYID)], c("STUDYID")))
  
  #  Construct filter condtion based on the tsparmcd/tsparm input para
  if (!(is.null(tsparmcd) | isTRUE(is.na(tsparmcd)) | isTRUE(tsparmcd==""))) {
    # Filtering on TSPARMCD
    if (grepl('\\*', tsparmcd)) {
      # To be evaluated as regex  - included wildcard characters '*' are changed to '.*' 
      filter<-paste("grepl('", gsub('\\*', '.*', tsparmcd), "',TSPARMCD,ignore.case=TRUE)", sep="");
    }
    else {
      filter<-"toupper(TSPARMCD) == toupper(tsparmcd)"
    }
  }
  else {
    # Filtering on TSPARM
    if (grepl('\\*', tsparm)) {
      # To be evaluated as regex  - included wildcard characters '*' are changed to '.*'
      filter<-paste("grepl('", gsub('\\*', '.*', tsparm), "',TSPARM,ignore.case=TRUE)", sep="");
    }
    else {
      filter<-"toupper(TSPARM) == toupper(tsparm)"
    }
  }
  # Build and exeute statement to extract an return the set of TS rows matching 
  # the given input filter
  stmt=paste("studyTS[", filter, ",][order(STUDYID,TSGRPID,TSPARMCD)]", sep="")
  eval(parse(text=stmt))
}

extractTSParam2<-function(studies=NULL, tsparmcd=NULL, tsparm=NULL) {
  # Extract adn return the requested TS paramaters
  extractTSParam(studies, tsparmcd, tsparm)[,
    # If value of TSGRPID is not empty - concatenate the TSGRPID to TSVAL in format '<TSGRPID>: <TSVAL>' - exclude TSGRPID column
    `:=` (TSVAL=paste(ifelse(is.na(TSGRPID)|TSGRPID=='','',paste(TSGRPID,': ',sep='')),TSVAL,sep=""), TSGRPID=NULL)][,
    # Combine concatenaded TSVAL in comma separated string for each distinct STUDYID/TS parameter combination 
    # - i.e. one row per STUDYID/TS parameter with all TSVAL values                                                                                                             
    lapply(.SD, paste0, collapse=" - "),by = list(STUDYID,TSPARMCD,TSPARM)] 
}
