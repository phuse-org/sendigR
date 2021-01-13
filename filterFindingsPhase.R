###################################################################################
# Script name   : filterFindingsPhase.R
# Date Created  : 25-Feb-2020
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date       Description
# --------------------- ------------------------------------------------------------
# <init/dd-Mon-yyyy>    <description>
# 
# -------------------------------------------------------------------------------
# Purpose       : Extract a set of findings for a specified study phase
#
# Description   : The function FilterFindingsPhase extracts and return a set of 
#                 findings from a input set of findings which is located within a
#                 specified study phaseFilter.
#                 The logic for the extraction is based on the subject elements and 
#                 the trial design domains - for each finding row:
#                 - The related subject element is found in SE as the row where the
#                   value of <domain>DTC is within the intervala SESTDTC-SEENDTC
#                 - The actual EPOCH is found in TA in the row matching the found 
#                   element (via the ETCD value)
#                 - The actual study phase is derived from the EPOCH value matching 
#                   at set of text patterns
#                 If the inclUncertain parameter is enabled, unceratin findings rows
#                 are include in the output set including an explanation of the 
#                 reason for the uncertainty.
#                 A row is registered as uncertain if:
#                 - One of the date/time values SESTDTC, SEENDTC or <domain>DTC is
#                   empty or contains an invalid ISO 8601 value
#                 - The value of <domain>DTC is included in more one SESTDTC/SEENDTC
#                   interval
#                 - The EPOCH value does not match any of the patterns identifying
#                   the set of possible study phases. 
#
# Input         : - A data table containing the input set of findings (input parameter 
#                   findings) - the minimum set of variables in the table are:
#                     - STUDYID       - character
#                     - USUBJID       - character
#                     - <domain>SEQ   - integer
#                     - <domain>DTC   - character
#                       where <domain> is the value of input parameter domain
#                 - Domains (imported from the pooled SEND data store if they don't 
#                   exist in workspace): 
#                     - DM 
#                     - SE 
#                     - TA 
#
# Output        : A data table containing the rows for the specified study phase
#                 (input parameter phaseFilter).
#                 If the input parameter inclUncertain is specified as TRUE,
#                 a set of rows where a study phase couldn't be identified is also
#                 included.
#                 The data table contains the same variables as the input data table
#                 and these additional variables:
#                     - PHASE   - character
#                       The identified study phase. The value is 'Uncertain' if a phase
#                       couldn't be identified 
#                     - UNCERTAIN_MSG - character
#                       Only included if the input parameter 'inclUncertain' is 
#                       specified as TRUE.
#                 plus any additional columns which may be included in the input data findings
#
# Parameters    : The function FilterFindingsPhase is defined with four input 
#                 parameters:
#                   domain:   Mandatory, character
#                             The name of the actual findings domain.
#                   findings: Mandatory, data table
#                             The table with the findings to filter
#                   phaseFilter:    Mandatory, character
#                             The study phaseFilter to filter for.
#                             It can be a single string, a vector or a list of multiple strings. 
#                             Valid values of phase included in the filter (case insensitive):
#                               Screening, Treatment, Recovery
#                   inclUncertain:
#                             Optional, boolean (TRUE or FALSE)
#                             Indicates whether findings where the study phaseFilter cannot 
#                             be identified by some reason shall be included or not 
#                             in the output data table
#                             
###################################################################################


library(data.table)
library(parsedate)

FilterFindingsPhase<-function(domain=NULL, findings=NULL, phaseFilter=NULL, inclUncertain=FALSE) {
  
  ################################################################################
  
  # Derive the phaseFilter for a given epoch text - using regular expressions
  getPhase<-function(epoch) {
    if (is.na(epoch) ) {
      'Uncertain'
    } else if (grepl("pre.*(tr(ea)?t|dos|test|study|exposure)|acclimat|screen|baseline|allocat|random",
                     epoch, 
                     ignore.case = TRUE)) {
      'Screening' 
    } else if (grepl("recovery|post.*(tr(ea)?t|dos|test|study|exposure)", 
                     epoch,
                     ignore.case = TRUE)) {
      'Recovery' 
    } else if (  grepl("tr(ea)?t|dos|test|exposure",
                       epoch, 
                       ignore.case = TRUE)
                 & !grepl("off|non|free|holiday",
                          epoch, 
                          ignore.case = TRUE)) {
      'Treatment' 
    } else {
      'Uncertain'
    }
  }
  
  #####################################################################################
  
  # Evaluate input parameters
  if (is.null(domain) | isTRUE(is.na(domain)) | isTRUE(domain=='')) {
    stop('Input parameter domain must have assigned a domain name ')
  }  
  if (!is.data.table(findings)) {
    stop('Input parameter findings must have assigned a data table ')
  }
  if (is.null(phaseFilter) | isTRUE(is.na(phaseFilter)) | isTRUE(phaseFilter=='')) {
    stop("Parameter phaseFilter must have assigned a non-empty value")
  }
  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter inclUncertain must be either TRUE or FALSE")
  }
 
  if (!exists("DM")) {
    importSENDDomains(c("DM"), unique(findings[,.(STUDYID)]))
  }
  if (!exists("SE")) {
    importSENDDomains(c("SE"), unique(findings[,.(STUDYID)]))
  }
  if (!exists("TA")) {
    importSENDDomains(c("TA"), unique(findings[,.(STUDYID)]))
  }
  
  # The specific --seq and --dtc variable names for the domain
  seq<-paste(toupper(domain), "SEQ", sep="")
  dtc<-paste(toupper(domain), "DTC", sep="")
  
  # Join DM with TA to get a list off all possible epochs per animal
                          # Extract DM rows for animals in the input list of findings
  dmEpoch<-merge(setkeyv(ExtractSubjData("DM",unique(findings[,.(STUDYID,USUBJID)]))[,.(STUDYID,USUBJID,ARMCD)],
                         c('STUDYID', 'ARMCD')),
                 setkeyv(TA[,.(STUDYID,ARMCD,ETCD,EPOCH=tolower(EPOCH))],c('STUDYID', 'ARMCD')), 
                 allow.cartesian=TRUE)[,ARMCD := NULL]
  
  # Join findings keys with Subject Elements to get all possible element(s) per subject
  findElementAll<-merge(setkeyv(setnames(subset(findings, TRUE, c("STUDYID","USUBJID", seq, dtc)),c(seq,dtc),c("SEQ","DTC")),
                                c('STUDYID', 'USUBJID')), 
                                # Extract SE rows for animals in the input list of findings
                        setkeyv(ExtractSubjData("SE",unique(findings[,.(STUDYID,USUBJID)]))[,.(STUDYID,USUBJID,ETCD,SESTDTC,SEENDTC)],
                                c('STUDYID', 'USUBJID')),
                        allow.cartesian=TRUE)
  
  # Extract rows where the finding --DTC is within the data interval in one or two 
  # elements start/end dates
  # There may be more than one element associated with a finding if the --DTC is within the interval of the end date in 
  # one element and start date in another element(i.e. a time part is missing in one or more of the --DTC, SESTDTC, SEENDTC).
  # All ISO date/time variables are converted to numerical datetime before the comparison 
  # If a --DTC or SESTDTC value misses the timepart 
  # - it's automatically calculated as 00:00:00 
  # If a SEENDTC values misses the timepart - it's calculated as 23:59:59
  if (!inclUncertain) {
    # Include only rows where all involved ISO date values are non-empty and valid 
    findElement<-findElementAll[!is.na(parse_iso_8601(DTC))     &
                                !is.na(parse_iso_8601(SESTDTC)) &
                                !is.na(parse_iso_8601(SEENDTC)) &
                                data.table::between(parse_iso_8601(DTC),
                                                    parse_iso_8601(SESTDTC),
                                                    parse_iso_8601(ifelse(grepl("T",SEENDTC),SEENDTC,paste(SEENDTC,"T23:59:59",sep=""))))]
    # Exclude rows where more then one element fits the finding date
    # - Remove the DTC column (MIDTC is included later on)
    findElement<-findElement[,NUM_ELEMENT := .N, by = .(STUDYID,USUBJID,SEQ)][NUM_ELEMENT == 1][, `:=` (DTC = NULL, NUM_ELEMENT=NULL) ]
  } 
  else {
    # Include additional rows where one or more involved ISO date values are empty or invalid 
    # - add an message for these uncertain rows                                       
    findElement<-findElementAll[is.na(parse_iso_8601(DTC))     |
                                is.na(parse_iso_8601(SESTDTC)) |
                                is.na(parse_iso_8601(SEENDTC)) |
                                data.table::between(parse_iso_8601(DTC),
                                                    parse_iso_8601(SESTDTC),
                                                    parse_iso_8601(ifelse(grepl("T",SEENDTC),SEENDTC,paste(SEENDTC,"T23:59:59",sep=""))))][,
                                INVAL_ISO_DT_MSG := ifelse(is.na(parse_iso_8601(DTC))     |
                                                           is.na(parse_iso_8601(SESTDTC)) |
                                                           is.na(parse_iso_8601(SEENDTC)),
                                                           paste0('Empty or invalid ISO8601 date/time value in: ', dtc, ', SESTDTC or SEENDTC'),
                                                           as.character(NA))]

    # Add number of distinct elements per finding to identify if more then one element fits the finding date
    # - Remove the DTC column (MIDTC is included later on)
    findElement[,NUM_ELEMENT := .N, by = .(STUDYID,USUBJID,SEQ)][,DTC := NULL]
  }
  
  # Merge epochs per animal with findings keys/elements to get the epoch(s) per finding
  # Keep each element whether an epoch is found or not (left join)
  # - Derive the study phase based on the epoch
  findEpoch<-merge(setkeyv(findElement, c('STUDYID', 'USUBJID', 'ETCD')),
                   setkeyv(dmEpoch,c('STUDYID', 'USUBJID', 'ETCD')),
                   all.x = TRUE)[, 
             PHASE := mapply(getPhase,EPOCH )]
  
  if (!inclUncertain) {
    # Remove rows with 'Uncertain' PHASE value
    findEpoch<-findEpoch[PHASE!='Uncertain']
    # Apply the given PHASE filter
    findIDs<-findEpoch[toupper(PHASE) %in% toupper(phaseFilter)]
  }
  else {
    # If any PHASE values are 'Uncertain', include an explanation 
    findEpoch[PHASE == 'Uncertain', MISS_PHASE_MSG := 'Could not decide PHASE based on EPOCH value']
    # For findings with multiple elements - collect all phases per finding in a message string
    findEpoch[NUM_ELEMENT>1,
              `:=` (MULTI_ELEM_MSG = paste0('Phase could not be decided due to date overlap - possibly: ', 
                                     paste(PHASE, collapse=','),
                                     ' (EPOCH: ', paste(EPOCH, collapse=','),')',
                                     ' (ETCD: ', paste(ETCD, collapse=','),')', 
                                     ' (SESTDTC/SEENDTC: ', paste(paste(SESTDTC,SEENDTC,sep='/'), collapse=','),')'), 
                    ETCD = 'UNCERTAIN',
                    SESTDTC = 'UNCERTAIN',
                    SEENDTC = 'UNCERTAIN',
                    EPOCH = 'UNCERTAIN',
                    PHASE = 'Uncertain'), 
              by = .(STUDYID,USUBJID,SEQ)] 
    # Remove duplicates generated by multiple found elements 
    # - collects all uncertainty messages in one column
    findEpoch<-unique(findEpoch)[,UNCERTAIN_MSG := ifelse(!is.na(INVAL_ISO_DT_MSG) | !is.na(MISS_PHASE_MSG) | !is.na(MULTI_ELEM_MSG),
                                                           gsub(";$", "", paste0('FilterFindingsPhase: ',
                                                                                 paste(ifelse(is.na(INVAL_ISO_DT_MSG), "", INVAL_ISO_DT_MSG), 
                                                                                       ifelse(is.na(MISS_PHASE_MSG), "", MISS_PHASE_MSG), 
                                                                                       ifelse(is.na(MULTI_ELEM_MSG), "", MULTI_ELEM_MSG), sep = ";"))),
                                                           as.character(NA))][,
                                `:=` (INVAL_ISO_DT_MSG = NULL, 
                                      MISS_PHASE_MSG = NULL, 
                                      MULTI_ELEM_MSG = NULL, 
                                      NUM_ELEMENT = NULL)]
    # Apply the given PHASE filter - include the uncertain rows
    findIDs<-findEpoch[toupper(PHASE) %in% toupper(phaseFilter) | !is.na(UNCERTAIN_MSG)]
  }

  
    # Merge the list of finding IDs with the input list of findings to include all variables
  foundFindings<-
    merge(setkeyv(findings,c("STUDYID", "USUBJID", seq)), 
                  # Rename the SEQ variable to the real --SEQ variable name
          setkeyv(setnames(findIDs, "SEQ",seq),c("STUDYID", "USUBJID",seq)))
  
  if ("UNCERTAIN_MSG.y" %in% names(foundFindings)) 
    # An UNCERTAIN_MSG column is included in both input and calculated set of findings
    #  - merge the UNCERTAIN_MSG from each of the merged tables into one column
    #  - non-empty messages are separated by '|'
    #  - exclude the original UNCERTAIN_MSG columns after the merge  
    foundFindings<-foundFindings[,`:=` (UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y), 
                                                             paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep='|'),
                                                             Coalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)))][, `:=` (UNCERTAIN_MSG.x=NULL,UNCERTAIN_MSG.y=NULL)]
  
  # return final data set 
  return(foundFindings)
}

