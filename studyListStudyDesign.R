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
library(DescTools)

GetStudyListSDESIGN<-function(studyDesign=NULL, Exclusively="Y") {
  ########################################################################
  # Generate a global data table with all studies and study designs
  # included in the TS domain. 
  # Calculate and add a column with the number of distinct study design 
  # values for each study.
  prepareTS_SDESIGN<-function() {
    if (!exists("TS")) {
      # import TS if it's not already exists
      importSENDDomains(c("TS"))
    }
    # Extract all TS rows for parameter ROUTE with non-empty TSVAL, rename TSVAL to ROUTE
    assign("TSAllSDESIGN", unique(TS[TSPARMCD == 'SDESIGN' & TSVAL != "", .(STUDYID, SDESIGN = TSVAL)]), envir=.GlobalEnv)
    
    # Add variable with count of distinct study designs specified per study
    TSAllSDESIGN[, `:=` (NUM_SDESIGN = .N), by = STUDYID]
  }
  ### End of prepareTS_ROUTE ##############################################
  
  if (is.null(studyDesign)) {
    print("ERROR: A study design must be specified")
  } 
  else if (!(toupper(Exclusively) %in% c("Y","N"))) {
    print("ERROR: Parameter Exclusively must be either 'Y' or 'N'")
  }
  else {
    if (!exists("TSAllSDESIGN")) {
      # Initial extraction  of TS route data - executed once.
      prepareTS_SDESIGN()
    }
    
    # extract and return list of studies of specified design
    if (toupper(Exclusively) == "Y") {
      # extract from list of studies with only one study design specified in TS
      TSAllSDESIGN[ toupper(SDESIGN) == toupper(StrTrim(studyDesign)) & NUM_SDESIGN==1, .(STUDYID)]
    }
    else {
      # extract from complete list of studies
      TSAllSDESIGN[toupper(SDESIGN) == toupper(StrTrim(studyDesign)), .(STUDYID)]
    }
  }
}




