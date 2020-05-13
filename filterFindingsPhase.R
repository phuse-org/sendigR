###################################################################################
# Script name   : filterFindingsPhase.R
# Date Created  : 25-Feb-2020
# Documentation : <if relevant, reference to specification document>
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date       Description
# --------------------- ------------------------------------------------------------
# Bo Larsen/17-Apr-2020 Changed conversion of ISO date/times to use package parsedate 
#
# -------------------------------------------------------------------------------
# Purpose       : Extract a set of findings for a specified study phase
#
# Description   : The function FilterFindingsPhase extracts and return a set of 
#                 findings from a input set of findings which is located within a
#                 specified study phase.
#                 The logic for the extraction is based on the subject elements and 
#                 the trial design domains
#
# Input         : - A data table containing the input set of findings (input parameter 
#                   findings) - the minimun set of variables in the table are:
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
#                 (input paramter phase).
#                 If the input parameter inclUncertain is specified as TRUE,
#                 a set of rows where a study phae couldn't be identified is also
#                 included.
#                 The data table contains the same variables as the input data table
#                 and these additional variables:
#                     - PHASE   - character
#                       The identified study phase. The value is 'Uncertain' if a phase
#                       couldn't be identified 
#                     - MESSAGE - character
#                       If the value of PAHSE is 'Uncertain' - it contains an explanation 
#                       of why the function cannot identify the study phase - else it's empty
#
# Parameters    : The function FilterFindingsPhase is defined with four input 
#                 parameters:
#                   domain:   Mandatory, character
#                             The name of the actual findings domain.
#                   findings: Mandatory, data table
#                             The table with the findings to filter
#                   phase:    Mandatory, character
#                             The study phase to filter for
#                             Valid values (case insensitive):
#                               Screening, Treatment, Recovery
#                   inclUncertain:
#                             Optional, boolean (TRUE or FALSE), default: FALSE
#                             Indicates whether findings where the study phase cannot 
#                             be identified by some reason shall be included or not 
#                             in the output data table
#
# Usage notes   : <how to use the script - e.g. the preconditions to be fulfilled 
#                 before script execution>
#
# MISSING: 
#   - Import reg. epxressions to identify phases from input file instead of hard coded 
#   - Handle cases where multiple epochs are identified per elements
#   - Handling of pooled data
#   - complete validation of all input parameters
###################################################################################


library(data.table)
library(parsedate)

FilterFindingsPhase<-function(domain, findings, phase, inclUncertain=FALSE) {
  
  # Derive the phase for a given epoch text - using regular expressions
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
  
  if (is.null(findings) | is.null(domain) | is.null(phase)) {
    print("ERROR: A domain name, a data table with findings to filter and the study phase to filter for must be specified")
  } 
  else {
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
                   allow.cartesian=TRUE)
    
    # Join findings keys with Subject Elements to get actual element(s) where the findings --DTC is within the date range of the element(s).
    # There may be more than one element associated with a finding if the --DTC is within the interval of the end date in 
    # one element and start date in another element(i.e. a time part is missing in one or more of the --DTC, SESTDTC, SEENDTC).
    # All ISO date/time variables are converted to numerical datetime before the comparison
    findElement<-merge(setkeyv(setnames(subset(findings, TRUE, c("STUDYID","USUBJID", seq, dtc)),c(seq,dtc),c("SEQ","DTC")),
                               c('STUDYID', 'USUBJID')), 
                                # Extract SE rows for animals in the input list of findings
                       setkeyv(ExtractSubjData("SE",unique(findings[,.(STUDYID,USUBJID)]))[,.(STUDYID,USUBJID,ETCD,SESTDTC,SEENDTC)],
                               c('STUDYID', 'USUBJID')),
                                            # Extract rows where the finding --DTC is withing the data interval in one or two 
                                            # elements start/end dates 
                                                                # If a --DTC or SESTDTC value misses the timepart 
                                                                # - it's automatically calculated as 00:00:00
                       allow.cartesian=TRUE)[between(parse_iso_8601(DTC),
                                                     parse_iso_8601(SESTDTC),
                                                     # If a SEENDTC values misses the timepart - it's calculated as 23:59:59
                                                     parse_iso_8601(ifelse(grepl("T",SEENDTC),SEENDTC,paste(SEENDTC,"T23:59:59",sep=""))))]

    # Add number of distinct elements per finding to identify if more then one element fits the finding date
    findElement[,`:=` (NUM_ELEMENT = .N), by = .(STUDYID,USUBJID,SEQ)]

    # Merge epochs per animal with findings keys/elements to get the epoch(s) per finding
    # Keep each element whether an epoch is found or not (left join)
    findEpoch<-merge(setkeyv(findElement, c('STUDYID', 'USUBJID', 'ETCD')),
                     setkeyv(dmEpoch,c('STUDYID', 'USUBJID', 'ETCD')),
                     all.x = TRUE)[,.(STUDYID,USUBJID, SEQ, ETCD, NUM_ELEMENT, EPOCH)]  
    
    # - Add number of distinct epochs per finding/element to identify if more then one epoch contain the element
    # - Derive the study phase based on the epoch
    findEpoch[,`:=` (NUM_EPOCH = .N ), by = .(STUDYID,USUBJID,SEQ,ETCD)][, PHASE := mapply(getPhase,EPOCH )]
    
    
    # For findings with mutliple elements - collect all phases per finding in a message string
    multiElemIn<-setkeyv(findEpoch[NUM_ELEMENT>1],c('STUDYID', 'USUBJID','SEQ'))
    # Initiate the list of findings with multiple identified elements 
    multiElemOut<-data.table(STUDYID=character(), USUBJID=character(), SEQ=integer(), PHASE=character(), MESSAGE=character())
    if (nrow(multiElemIn) > 0) {
      prev_key<-data.table(STUDYID=character(), USUBJID=character(), SEQ=integer())
      for (i in 1:nrow(multiElemIn)) {
        row<-multiElemIn[i,.(STUDYID,USUBJID,SEQ,PHASE)]
        key<-row[,.(STUDYID,USUBJID,SEQ)]  
        if (!isTRUE(all.equal(prev_key,key))) {
          # First row or a new findings key
          if (nrow(prev_key)==1) {
            # a new findings key - add the findings row to the list 
            multiElemOut<- rbindlist(list(multiElemOut,prev_key[,.(STUDYID,USUBJID,SEQ, PHASE='Uncertain', MESSAGE=allPhases)]))
          }
          # Change to new key - initiate message text
          prev_key<-key
          allPhases<-paste("Phase could not be decided due to date overlap - possibly: ", row[,(PHASE)],sep="")
        } else {
            # next row for the current findings key - just add the PHASE to the message text 
            allPhases<-paste(allPhases,row[,(PHASE)],sep=", ")
        }
      }
      # Add the last findings row to the list
      multiElemOut<-rbindlist(list(multiElemOut,prev_key[,.(STUDYID,USUBJID,SEQ, PHASE='Uncertain', MESSAGE=allPhases)]))
    }
    
    
    # Merge all the input finding keys with the identified finding keys with one or more identified elements
    # Keep each input finding whether or not any element has been identified (left join)
                        # All input finding keys
    allFindEpoch<-merge(setkeyv(setnames(subset(findings, TRUE, c("STUDYID","USUBJID", seq)),seq,"SEQ"),
                                c("STUDYID", "USUBJID","SEQ")),
                        # All finding keys with at one or more identified elements
                        setkeyv(rbindlist(list(findEpoch[NUM_ELEMENT==1,.(STUDYID,USUBJID,SEQ, PHASE, MESSAGE=ifelse(PHASE=="Uncertain","Could not identify Phase from Epoch", ""))], 
                                               multiElemOut)),
                               c("STUDYID","USUBJID","SEQ")),
                                      # Set the value of PHASE and a related message where a single PHASE couldn't be identified
                       all.x = TRUE)[,`:=` (MESSAGE = ifelse(is.null(PHASE) | is.na(PHASE),"Could not find an element", MESSAGE),PHASE = ifelse(is.null(PHASE) | is.na(PHASE),"Uncertain", PHASE))]
    
    # Extract the finding IDs for the selected phase
    if (inclUncertain) {
      allFindIDs<-allFindEpoch[toupper(PHASE)==toupper(phase) | PHASE=='Uncertain']
    } else {
      allFindIDs<-allFindEpoch[toupper(PHASE)==toupper(phase)]
    }
    
    # Merge and return the list of finding IDs with the input list of findings to include all variables
    merge(setkeyv(findings,c("STUDYID", "USUBJID", seq)), 
                  # Rename the SEQ variable to the real --SEQ variable name
          setkeyv(setnames(allFindIDs, "SEQ",seq),c("STUDYID", "USUBJID",seq)))
  }
}

