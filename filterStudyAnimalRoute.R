###################################################################################
# Script name   : filterStudyAnimalRoute.R
# Date Created  : 16-Jan-2020
# Documentation : Specification - R script - ROUTE.docx
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Extract studies and animals which fulfills a specified  route of 
#                 administration from a pooled SEND data store.
#
# Description   : Function FilterAnimalListRoute:
# 
#                 Returns a data table with a set of animals extracted from the table of  
#                 animals given as input where the animals are treated via the route of  
#                 administration given as input (a list of multiple route values may be 
#                 given as input).
#                 Animals from the input set, which are included in studies where one of  
#                 of the input route values is registered in TS (and TS have only this route 
#                 value included) are included in the output set.
#                 Animals from the input set, belonging to studies 
#                   - with no or non-CT route value registered  or  
#                   - with more than one route value registered
#                 in TS are included in the output set if they exists in EX with the EXROUTE 
#                 matching one of the input route values. The filtering of EX rows is done
#                 both at subject and pool level rows.
#                 The comparison of route values is done case insensitive.
#                 
#                 If the input parameter inclUncertain flag is enabled, uncertain animals
#                 are included in the output set.
#                 These uncertain situations are identified and reported (in column UNCERTAIN_MSG):
#                  - TS parameter is missing or invalid (not CT value - CDISC code list ROUTE) 
#                    and EXROUTE is missing or invalid (not CT value - CDISC code list ROUTE)
#                  - Multiple TS parameter ROUTE values are registered for study but EXROUTE is missing 
#                    or invalid (not CT value)
#                  Non-empty UNCERTAIN_MSG values are merged with non-empty UNCERTAIN_MSG values 
#                  which may exist in the input set of animals (animalList).
#                 
# Input         : The EX and POOLDEF domains - are imported from the pooled SEND data 
#                 store if they don't exist in workspace.
#                 A data table specified in the input parameter animalList:
#                   It contains the list of animals to filter for specified route value(s)
#                   - must contain these character variables:
#                       STUDYID
#                       USUBJID
#                     other variables may be included
#                  
#                 CDISC CT code list ROUTE
#                 One of more values for ROUTE to include in the the filtering 
#
# Output        : A data table with the character columns:
#                   STUDYID
#                   USUBJID
#                   ROUTE
#                   UNCERTAIN_MSG - if input parameter inclUncertain flag is enabled
#                 plus any additional columns which may be included in the input data animalList
#
# Parameters    : animalList:     mandatory, data table (see Input)
#                 routeFilter:    mandatory, the route value(s) to use as criterion for 
#                                 filtering of the input data table.
#                                 It can be a single string, a vector or a list of 
#                                 multiple strings. 
#                 inclUncertain: Optional, Include uncertain rows or not
###################################################################################

library(data.table)

FilterAnimalListRoute<-function(animalList=NULL, routeFilter=NULL, inclUncertain=FALSE) {
  
  if (is.null(animalList) | isTRUE(is.na(animalList)) | isTRUE(animalList=='')) {
    stop('Input parameter animalList must have assigned a data table ')
  } 
  if (is.null(routeFilter) | isTRUE(is.na(routeFilter)) | isTRUE(routeFilter=='')) {
    stop('Input parameter routeFilter must have assigned a data table ')
  }
  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter inclUncertain must be either TRUE or FALSE")
  }
  
  # List of studyid values included in the input table of animals
  animalStudies<-unique(animalList[,.(STUDYID)])
  
  # import domain which may be missing in workspace
  if (!exists('TS')) {
    importSENDDomains(c('TS'))
  }
  if (!exists('EX')) {
    importSENDDomains(c('EX'), animalStudies)
  }
  if (!exists('POOLDEF')) {
    importSENDDomains(c('POOLDEF'), animalStudies)
  }
  
  
  # Extract all TS rows for parameter ROUTE, rename TSVAL to ROUTE 
  # - remove duplicates
  # - limit to the set of studies for 
  tsROUTEall<-merge(unique(TS[TSPARMCD == 'ROUTE', .(STUDYID,ROUTE=toupper(trimws(TSVAL)))]), animalStudies, by='STUDYID')
  # Add variable with count of number of distinct routes per study
  # - extract set or rows matching routeFilter
  tsROUTEall[, `:=` (NUM_ROUTE = .N), by = STUDYID]
  tsROUTE<-tsROUTEall[ROUTE %in% toupper(trimws(routeFilter))]
  
  # Get values of code list ROUTE from CDISC CT
  ctROUTE<-getCTCodListValues("ROUTE")
  
  # Get list of uncertain ROUTE data at TS level - combined by 
  #  - list of studies with no TS parameter ROUTE
  #  - list of studies with non-CT value of TS parameter ROUTE
  #  - include a study level uncertain message
  tsROUTEUncertain<-rbindlist(list(fsetdiff(animalStudies, 
                                            tsROUTEall[,.(STUDYID)])[,.(STUDYID, ROUTE=NA, UNCERTAIN_MSG='TS parameter ROUTE is missing')],
                                   tsROUTEall[!(toupper(ROUTE) %in% ctROUTE),
                                              .(STUDYID,ROUTE,UNCERTAIN_MSG='TS parameter ROUTE does not contain a valid CT value')]))
  
  # Extract list of animals for studies with only one ROUTE specified in TS 
  # - for these studies we can extract all control animals
  studyLvlAnimalsFound<-merge(tsROUTE[NUM_ROUTE==1, .(STUDYID, ROUTE)], 
                              animalList[,.(STUDYID, USUBJID)], by='STUDYID')[,.(STUDYID, USUBJID, ROUTE)]
  
  # Extract list of EX rows for the input animals belonging to studies with no valid route value registered  
  # or with more than one route value registered in TS:
  #  - extract set of animals belonging to either studies with more than one ROUTE specified in TS or uncertain studies
  #  - extract EX rows outer joined to POOLDEF to get USUBJID for pools included in EX
  #  - join the two lists to get the complete list of input animals with related ROUTE registered in EX - do an 
  #    outer join to also include animals with no rows in EX
  EXLvlAnimalsAll<-merge(merge(animalList[,.(STUDYID,USUBJID)],
                               unique(rbindlist(list(tsROUTE[NUM_ROUTE > 1, .(STUDYID)], tsROUTEUncertain[,.(STUDYID)]))),
                               by='STUDYID'),
                         unique(merge(unique(EX[,.(STUDYID, USUBJID, POOLID, ROUTE=toupper(trimws(EXROUTE)))]),
                                      POOLDEF[,.(STUDYID, POOLID, USUBJID)],
                                      by=c('STUDYID', 'POOLID'),all.x=TRUE)[,.(STUDYID, USUBJID=ifelse(USUBJID.x=='',USUBJID.y,USUBJID.x ), ROUTE)]),
                         by=c('STUDYID', 'USUBJID'), all.x=TRUE)
                        
  
  
  
  if (inclUncertain) {
    # Include uncertain rows

    # Extract set of animals matching the routeFilter in EX
    #   - include uncertain rows - i.e. animals with missing or non-CT ROUTE value with a related message
    EXLvlAnimalsFound<-EXLvlAnimalsAll[ROUTE %in% toupper(trimws(routeFilter)) | !(ROUTE %in% ctROUTE) | is.na(ROUTE),
                                       .(STUDYID, USUBJID, ROUTE, UNCERTAIN_MSG=ifelse(is.na(ROUTE),
                                                                                       'Missing rows in EX',
                                                                                       ifelse(!(ROUTE %in% ctROUTE),
                                                                                              'EXROUTE does not contain a valid CT value',
                                                                                              NA)))]
    # Merge the EX level set of animals with the list of uncertain studies
    #  - If a route has been identified at animal level, not uncertain message is included
    #  - else an uncertain message is included 
    #       - if uncertain messages exists both at study and animals level, these are merged separated by ' & '
    EXLvlAnimalsFound<-merge(EXLvlAnimalsFound, 
                             tsROUTEUncertain[,.(STUDYID, UNCERTAIN_MSG)], by='STUDYID', all.x=TRUE)[,.(STUDYID, USUBJID, ROUTE, 
                                                                                                        UNCERTAIN_MSG=ifelse(is.na(ROUTE) | (!is.na(ROUTE) & !is.na(UNCERTAIN_MSG.x)),
                                                                                                                             paste('FilterAnimalListRoute: ',ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y), 
                                                                                                                                   paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep=' & '),
                                                                                                                                   Coalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)), sep=''),
                                                                                                                             NA))]
    # Combine extracted sets of animals at studye and EX level
    foundAnimals<-rbindlist(list(EXLvlAnimalsFound,
                                 studyLvlAnimalsFound), use.names=TRUE, fill=TRUE)
  }
  else {
    # Do not include uncertain rows
    
    # Extract set of animals matching the routeFilter in EX
    #  - combine with set of animals extracted at studylevel
    foundAnimals<-rbindlist(list(EXLvlAnimalsAll[ROUTE %in% toupper(trimws(routeFilter))],
                                 studyLvlAnimalsFound))
   
  }

  # Merge the list of extracted animals with the input set of animals to keep
  # any additional columns from the input table 
  foundAnimals<-merge(foundAnimals, animalList, by=c('STUDYID', 'USUBJID'))
  if ("UNCERTAIN_MSG.y" %in% names(foundAnimals)) {
    # An UNCERTAIN_MSG column is included in both input and found list of animals
    #  - merge the UNCERTAIN_MSG from each of the merged tables into one column
    #  - non-empty messages are separated by '|'
    #  - exclude the original UNCERTAIN_MSG columns after the merge  
    foundAnimals<-foundAnimals[,`:=` (UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y), 
                                                           paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep='|'),
                                                           Coalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)))][, `:=` (UNCERTAIN_MSG.x=NULL,UNCERTAIN_MSG.y=NULL)]
  }
  # Return list of found animals  
  return(foundAnimals)
}
