###################################################################################
# Script name   : addFindingsAnimalAge.R
# Date Created  : 19-Mar-2020
# Documentation : Specification - R script -Animal Age at TOM.docx
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Add a variable with the age of animal at the time of finding to
#                 a set of rows extratced from a findings domain.
#
# Description   : The function addFindingsAnimalAge calculates for each row in a 
#                 set of findings the age og the animal at the time of the finding
#                 using this algorithm:
#                 
#                 1. Compute Animal_age_on_Reference_Start_Date in days
#                     a. If BRTHDTC is populated compute DM.RFSTDTC – DM.BRTHDTC + 1
#                     b. Else If AGE is populated convert from units specified 
#                        in AGEU to days.
#                     c. Else If AGETXT is populated convert the mid-point of the 
#                        range from units specified in AGEU to days.
#                     These AGEU units are handled with the dexcribed conversion 
#                     from value to number of days:
#                     - DAYS 
#                     - WEEKS   : value * 7
#                     - MONTHS  : value * 365/12
#                     - YEARS   : value * 365
#                 2. Determine the Study_days_between_study_start_and_findings  
#                     a. if --DY is populated 
#                           i. If DY>0 then use DY-1
#                           ii. Else use DY
#                     b. Else If –DTC is populated compute DM.RFSTDTC – (--DTC)  in days
#                 3. Animal age at time of finding is 
#                    [Animal_age_on_Reference_Start_Date] + [Study_days_between_study_start_and_findings ]. 
#                    Report in days into additional variable AGE in the findings data set.
#                    
#                 If an age can not be calculated du to missing values in relevant 
#                 variables in either DM or the finings data, the reason is documented
#                 in an additional variable AGE_MESSAGE in the findings data set.
#                 This variable is only included in the output data table in the 
#                 input parameter 'inclUncertain' is specified as TRUE.
#
# Input         : - A data table containing the input set of findings (input parameter 
#                   'findings') - the minimun set of variables in the table are:
#                     - STUDYID       - character
#                     - USUBJID       - character
#                     - <domain>SEQ   - integer
#                     - <domain>DTC   - character
#                     - <domain>DY    - character
#                       where <domain> is the value of input parameter 'domain'
#                 - Domain (imported from the pooled SEND data store if they don't 
#                   exist in workspace): 
#                     - DM  
#
# Output        : A data table containing all the rows and variables in the input 
#                 table additional additional variable(s):
#                     - AGE         - integer
#                       The calculated age in days of the anmimal at the time of 
#                       the finding. The value is NA if an AGE couldn't be 
#                       calculated due to missing values in relevant variables.
#                     - AGE_MESSAGE - character
#                       Only included if the input parameter 'inclUncertain' is 
#                       specified as TRUE.
#                       If the AGE couldn't be calculated - it contains an explanation 
#                       of the reason - else it's empty (NA)
#
# Parameters    : The function addFindingsAnimalAge is defined with input parameters:
#                   domain:   Mandatory, character
#                             The name of the actual findings domain.
#                   findings: Manadatory, data table
#                             The table with the findings to calculate age for
#                   inclUncertain:
#                             Optional, boolean (TRUE or FALSE), default: FALSE
#                             Indicates whether the output table shall contain
#                             a variable or not with the an explantion for rows 
#                             where the age cannot be calculated.
#
# Usage notes   : Source the script.
#                 Execute the function addFindingsAnimalAge as many times as needed to  
#                 calculate the age of animal in sets of findings rows.
#                 Examples:
#                   allMI<-addFindingsAnimalAge('MI', allMI)
#                   LBgluc<-addFindingsAnimalAge('LB', LBgluc, inclUncertainMsg=TRUE)
#                 Please be aware that the input data set must be a data table.
#
# MISSING:
#   - Differentiate between rodents/non-rodents (if relevant)
#   - Handling of pooled data
###################################################################################

library(data.table)
library(parsedate)
library(stringr)


addFindingsAnimalAge<-function(domain, findings, inclUncertainMsg=FALSE) {

  ###################################################################################################
  # calculate the age for an animal at the referenece start date in DM to days 
  # - returns either the calculated age or a test with reason why the age couldn't be calculated. 
  ###################################################################################################
  calcDMAgeDays<-function(RFSTDTC,BRTHDTC,AGETXT,AGE,AGEU) {
    if (!(RFSTDTC == "" | is.na(RFSTDTC) | BRTHDTC == "" | is.na(BRTHDTC))) { 
      # BRTHDTC is populated
      return(as.numeric(parse_iso_8601(RFSTDTC)-parse_iso_8601(BRTHDTC)))
    } else if (!((AGEU == "" | is.na(AGEU)) | ((AGE == "" | is.na(AGE)) & (AGETXT == "" | is.na(AGETXT))))) {
      ageCalc<-NA
      if (!(AGE == "" | is.na(AGE))) {
        # AGE is populated
        ageCalc<-AGE
      } else if (grepl("^\\d+-\\d+$", AGETXT)) {
        # AGETXT is populated - use the mid value for calculation
        ageCalc<-(as.numeric(word(AGETXT,1,sep = "-"))+as.numeric(word(AGETXT,2,sep = "-")))/2
      }
      # Convert age to number of days:
      if (AGEU=='DAYS') {
        return(as.character(round(ageCalc)))
      } else if (AGEU=='WEEKS') {
        return(as.character(round(ageCalc*7)))
      } else if (AGEU=='MONTHS') {
        return(as.character(round(ageCalc*365/12)))
      } else if (AGEU=='YEARS') {
        return(as.character(round(ageCalc*365)))
      } else {
        # Not supported AGEU - cannot calculate
        return("Not supported or missing AGEU value has been populated in DM")
      }
    } else {
      # Not enough variables has been populated to do calculations
      return("Neither RFSTDTC/BRTHDTC nor AGE/AGETXT/AGEU has been fully populated in DM")
    }
  }
  ###################################################################################################
  
  if (is.null(findings) | is.null(domain) ) {
    print("ERROR: A domain name and a data table with findings to add age column to must be specified")
  } 
  else {
    # Get relevant DM rows and calculate age at RFSTDTC
    dm<-ExtractSubjData("DM",unique(findings[,.(STUDYID,USUBJID)]))[,.(STUDYID, USUBJID, RFSTDTC,BRTHDTC,AGETXT,AGE=as.numeric(AGE),AGEU)]
    dm[,AGEDAYStxt := mapply(calcDMAgeDays, RFSTDTC,BRTHDTC,AGETXT,AGE,AGEU)][,`:=` (AGEDAYS=suppressWarnings(as.numeric(AGEDAYStxt)), 
                                                                                     AGEDAYStxt=ifelse(!grepl("^[0-9]+$",AGEDAYStxt), AGEDAYStxt,NA))]
    dm <- unique(dm)
    # Merge relevant findings columns with dm for age calculation
    dm_find<-merge(dm[,.(STUDYID, USUBJID, RFSTDTC, AGEDAYS, AGEDAYStxt)], findings[, .(STUDYID, USUBJID, seq=get(paste(toupper(domain),'SEQ', sep='')), dy=get(paste(toupper(domain),'DY', sep='')),dtc=get(paste(toupper(domain),'DTC', sep='')))])
    # Calculate the age of each animal at time of finding
    dm_find[,`:=` (AGE = ifelse(!(dy == "" | is.na(dy)),
                           #  --DY is populated
                           AGEDAYS + ifelse(dy>0,dy-1,dy),
                           ifelse(!(dtc == "" | is.na(dtc)),
                                  #  --DTC is populated
                                  AGEDAYS + as.numeric(parse_iso_8601(dtc) - parse_iso_8601(RFSTDTC)),
                                  # Neither --DY nor --DTC is populated
                                  NA)),
                   AGE_MESSAGE=ifelse((dy=="" | is.na(dy)) & (dtc=="" | is.na(dtc)),ifelse(is.na(AGEDAYStxt),'Neither --DY nor --DTC has been populated', AGEDAYStxt), AGEDAYStxt))]
    
    # Remove columns not to be merged into the final data rows
    dm_find[, `:=` (dy=NULL, dtc=NULL, RFSTDTC=NULL, AGEDAYS=NULL, AGEDAYStxt=NULL)]
    if (!inclUncertainMsg) {
      dm_find[,AGE_MESSAGE := NULL]
    }
    
    # Merge and return the list of finding IDs plus age with the input list of findings to include all variables
    merge(setkeyv(findings,c("STUDYID", "USUBJID", paste(toupper(domain),'SEQ', sep=''))), 
          # Rename the SEQ variable to the real --SEQ variable name
          setkeyv(setnames(dm_find, "seq",paste(toupper(domain),'SEQ', sep='')),c("STUDYID", "USUBJID",paste(toupper(domain),'SEQ', sep=''))),
          all.x=TRUE)
  }
}
