###################################################################################
# Script name   : studyListStudyDesign.R
# Date Created  : 04-Feb-2020
# Documentation : Specification - R script - Q0.5 Study Design.docx
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Extract a list of SEND study ids which fulfills a specified 
#                 study design.
#
# Description   : Contains function GetStudyListSDESIGN:
#                 Returns a data table with the list of study ids from TS where  
#                 the value of TSVAL for the TSPARMCD 'SDESIGN' is equal to a given 
#                 study design.
#                 The comparison of study design values are done case insensitive.
#                 If specified (imput parameter 'inclUncertain') - studyid values
#                 included in TS 
#                   a) without any row for TSPARMCD='SDESIGN' or
#                   b) TSVAL doesn't contain a value included in the  CDISC CT list 
#                      'DESIGN' for TSPARMCD='SDESIGN'
#                 are extracted and returned too - including af message tellinge 
#                 whether reason is a) or b).
#
# Input         : The TS domain - is imported from the pooled SEND data store if 
#                 it doesn't exist in workspace
#
# Output        : The function GetStudyListSDESIGN returns a data table with one column:
#                     STUDYID           (character)
#                     SDESIGN           (character) 
#                     UNCERTAIN_MSG     (character)
#                       Only included when parameter inclUncertain=TRUE 
#                       Contains indication of whether STSTDTC is missing of has wrong 
#                       format - is NA for rows where STSTDTC is valid.
#                     Additional columns contained in the studyList table (if specified) 
#
# Parameters    : The function GetStudyListSDESIGN are defined with one input 
#                 parameter:
#                   studyDesign:  mandatory. The study design to use as criterion for 
#                                 filtering of the study id values.
#                   Exclusively:  mandatory, default value "Y". Specifies if the returned
#                                 list of studyid values includes only studies with 
#                                 one study design specied in TS ("Y") or may include
#                                 studies with multiple study designs specifed ("N"). 
#                   studyList:
#                             Optional, a data table with a list of studies to 
#                             limit the output to be within this set of studies
#                   inclUncertain:
#                             Optional, boolean (TRUE or FALSE), default: FALSE 
#                             Indicates whether study ids STSTDTC is missing or wrong 
#                             shall be included or not in the output data table
#
# Usage notes   : Source the script.
#                 Execute the function GetStudyListSDESIGN as many times as needed to extract 
#                 list(s) of studies for the wanted study design.
#                 Examples:
#                   GetStudyListSDESIGN("parallel")
#                   GetStudyListSDESIGN("parallel", Exclusively="n")
#                   GetStudyListSDESIGN("DOSE ESCALATION")
# 
#######################################################################################################################################################################

library(data.table)

GetStudyListSDESIGN<-function(studyDesign=NULL, Exclusively=TRUE, studyList=NULL, inclUncertain=FALSE) {
  
  ##################################################################################
  # Hard code CT list of study design - should be read from a CT input file
  ctDESIGN=c("CROSSOVER","DOSE ESCALATION","FACTORIAL","LATIN SQUARE","PARALLEL")
  ##################################################################################
  
  if ((is.null(studyDesign) | isTRUE(is.na(studyDesign)) | isTRUE(studyDesign==""))) {
    stop("A study design must be specified")
  } 
  
  if (!(toupper(Exclusively) %in% c(TRUE,FALSE))) {
    stop("Parameter Exclusively must be either (T)RUE or (F)ALSE")
  }
  
  if (!exists("TS")) {
    # import TS if it's not already exists
    importSENDDomains(c("TS"))
  }
  
  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter Exclusively must be either TRUE or FALSE")
  }
  
  studyListIncl<-FALSE
  if (!(is.null(studyList) | isTRUE(is.na(studyList)) | isTRUE(studyList==''))) {
    # An initial list of studies is included
    studyListIncl<-TRUE
  }
  
  # Extract all TS rows for parameter SDESIGN rename TSVAL to SDESIGN
  tsSDESIGN<-unique(TS[TSPARMCD == 'SDESIGN', .(STUDYID, SDESIGN = toupper(trimws(TSVAL)))])
  if (inclUncertain) {
    #Include all studies with no SDESIGN parameter included
    tsSDESIGN<-rbindlist(list(tsSDESIGN, fsetdiff(unique(TS[,.(STUDYID)]), tsSDESIGN[,.(STUDYID)])[,.(STUDYID, SDESIGN = NA)]))
  }
  if (studyListIncl) {
    # Limit to the set of studies given as input
    tsSDESIGN<-merge(tsSDESIGN, studyList[,.(STUDYID)], by='STUDYID')
  }
  
  # Add variable with count of distinct study designs specified per study
  tsSDESIGN[, `:=` (NUM_SDESIGN = .N), by = STUDYID]
  
  # Construct the statement to apply the specified design
  designFilter<-'toupper(SDESIGN) %in% toupper(trimws(studyDesign))'
  if (Exclusively) {
    designFilter<-paste(designFilter, ' & NUM_SDESIGN==1', sep='')
  }
  
  if (inclUncertain) {
    ## Include uncertain studies
    
    # Include condition for rows with empty or wrong value of SDESIGN
    designFilter<-paste(paste("(", designFilter), ") | ! (toupper(SDESIGN) %in% ctDESIGN)")
    
    # Build the statement to execute - include column indication of missing or wrong value
    stmt=paste(paste("tsSDESIGN[", designFilter, sep=""),
               ",.(STUDYID, SDESIGN, UNCERTAIN_MSG=ifelse( ! (toupper(SDESIGN) %in% ctDESIGN),
                                                          ifelse(is.na(SDESIGN), 
                                                                 'GetStudyListSDESIGN: TS parameter SDESIGN is missing', 
                                                                 'GetStudyListSDESIGN: TS parameter SDESIGN does not contain a valid CT value'),
                                                          NA))]", 
               sep="")
    # Execute statement to extract studies fulfilling the condition(s) plus uncertain studies
    foundStudies<-eval(parse(text=stmt))
    
    if (studyListIncl) {
      # Merge the list of extracted studies with the input set of studies to keep
      # any additional columns from the input table 
      foundStudies<-merge(foundStudies, studyList, by='STUDYID')
      if ("UNCERTAIN_MSG.y" %in% names(foundStudies)) {
        # The studyList table contains alread an UNCERTAIN_MSG column
        #  - merge the UNCERTAIN_MSG from each of the merged tables into one column
        #  - non-empty messages are separated by '|'
        #  - exclude the original studyList.UNCERTAIN_MSG after the merge  
        foundStudies<-foundStudies[,`:=` (UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y), 
                                                               paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep='|'),
                                                               Coalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)))][, !names(foundStudies) %in% c('UNCERTAIN_MSG.x','UNCERTAIN_MSG.y'), with=FALSE]
      }
    }
    # Return the list of extracted studies
    return(foundStudies)
  }
  else  {
    ## Do not include uncertain studies
    
    # Build the statement to extract studies fulfilling the condition(s) and execute
    foundStudies<-eval(parse(text=paste(paste("tsSDESIGN[", 
                                              designFilter, 
                                              sep=""),
                                        ",.(STUDYID, SDESIGN)]", 
                                        sep="")))
    
    if (studyListIncl) {
      # Return the list of extracted studies merged with the input set of studies to keep
      # any additional columns from the input table 
      return(merge(foundStudies, studyList, by='STUDYID'))
    }
    else {
      # Return the list of extracted studies
      return(foundStudies)
    }
  }
}




