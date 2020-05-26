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
# Description   : A function GetStudyListSDESIGN is defined:
#                   Returns a data table with the list of study ids from TS 
#                   where the value of TSVAL for the TSPARMCD 'SDESIGN'  
#                   is equal to a given study design.
#                   The comparison of study design values are done case insensitive.
#
# Input         : The TS domain - is imported from the pooled SEND data store if 
#                 it doesn't exist in workspace
#
# Output        : The function GetStudyListSDESIGN returns a data table with one column:
#                     STUDYID             (character)
#
# Parameters    : The function GetStudyListSDESIGN are defined with one input 
#                 parameter:
#                   studyDesign:  mandatory. The study design to use as criterion for 
#                                 filtering of the study id values.
#                   Exclusively:  mandatory, default value "Y". Specifies if the returned
#                                 list of studyid values includes only studies with 
#                                 one study design specied in TS ("Y") or may include
#                                 studies with multiple study designs specifed ("N"). 
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
#library(DescTools)

GetStudyListSDESIGN<-function(studyDesign=NULL, Exclusively=TRUE, UncertainTabName=NULL) {
  
  ##################################################################################
  # Hard code CT list of study design - should be read from a CT input file
  ctDESIGN=c("CROSSOVER","DOSE ESCALATION","FACTORIAL","LATIN SQUARE","PARALLEL")
  ##################################################################################
  
  if (is.null(studyDesign)) {
    stop("A study design must be specified")
  } 
  
  if (!(toupper(Exclusively) %in% c(TRUE,FALSE))) {
    stop("Parameter Exclusively must be either (T)RUE or (F)ALSE")
  }
  
  if (!exists("TS")) {
    # import TS if it's not already exists
    importSENDDomains(c("TS"))
  }
  
  # Extract all TS rows for parameter SDESIGN rename TSVAL to SDESIGN
  tsSDESIGN<-unique(TS[TSPARMCD == 'SDESIGN', .(STUDYID, SDESIGN = toupper(trimws(TSVAL)))])
  
  # Add variable with count of distinct study designs specified per study
  tsSDESIGN[, `:=` (NUM_SDESIGN = .N), by = STUDYID]
  
  if (!is.null(UncertainTabName)) {
    # Find studies with no SDESIGN parameter in TS
    # Find studies with invalid value in SDESIGN parameter in TS
    assign(UncertainTabName,
           rbindlist(list(tsSDESIGNmiss=fsetdiff(unique(TS[,.(STUDYID)]),tsSDESIGN[,.(STUDYID)])[,.(STUDYID, MSG="Missing TS parameter SDESIGN")],
                                tsSDESIGNinvalid=tsSDESIGN[! (toupper(SDESIGN) %in% ctDESIGN), .(STUDYID, MSG="No valid CT value for TS parameter SDESIGN")])), 
           envir=parent.env(environment(NULL))) 
  }
  
  # extract and return list of studies of specified design
  if (Exclusively) {
    # extract from list of studies with only one study design specified in TS
    return(tsSDESIGN[ toupper(SDESIGN) %in% toupper(trimws(studyDesign)) & NUM_SDESIGN==1, .(STUDYID)])
  }
  else {
    # extract from complete list of studies
    return(unique(tsSDESIGN[toupper(SDESIGN) %in% toupper(trimws(studyDesign)), .(STUDYID)]))
  }
}




