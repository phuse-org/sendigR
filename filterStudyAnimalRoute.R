###################################################################################
# Script name   : filterStudyAnimalRoute.R
# Date Created  : 16-Jan-2020
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
#                  - TS parameter ROUTE is missing or invalid (not CT value - CDISC code list ROUTE) 
#                    and EXROUTE is missing or invalid (not CT value - CDISC code list ROUTE)
#                  - Multiple TS parameter ROUTE values are registered for study but EXROUTE is missing 
#                    or invalid (not CT value)
#                  Non-empty UNCERTAIN_MSG values are merged with non-empty UNCERTAIN_MSG values 
#                  which may exist in the input set of animals (animalList).
#                 
# Input         : - The TS, EX and POOLDEF domains - are imported from the pooled SEND data 
#                   store if they don't exist in workspace.
#                 - A data table specified in the input parameter animalList:
#                   It contains the list of animals to filter for specified route value(s)
#                   - must contain these character variables:
#                       STUDYID
#                       USUBJID
#                     other variables may be included
#                 - The CDISC CT code list ROUTE imported from a CDISC CT file.
#
# Output        : A data table with the character columns:
#                   STUDYID
#                   USUBJID
#                   ROUTE
#                   UNCERTAIN_MSG - if input parameter inclUncertain flag is enabled
#                 plus any additional columns which may be included in the input data animalList
#
# Parameters    : animalList:     Mandatory, data table (see Input)
#                 routeFilter:    Mandatory, character.
#                                   The route value(s) to use as criterion for filtering of the input data table.
#                                   It can be a single string, a vector or a list of multiple strings. 
#                 inclUncertain:  Optional, boolean.
#                                   Include uncertain rows or not
#                 exclusively:    Optional, boolean.
#                                   TRUE: Include animals only for studies with no other route than routeFilter
#                                   FALSE: Include animals for all studies with route matching routeFilter
#                 matchAll:       Optional, boolean.
#                                   TRUE: Include animals only for studies with route(s) matching all values 
#                                         in routeFilter  
#                                   FALSE: Include animals for all studies with route matching at least one value 
#                                          in routeFilter
###################################################################################

library(data.table)

FilterAnimalListRoute<-function(animalList=NULL, routeFilter=NULL, inclUncertain=FALSE, exclusively=FALSE, matchAll=FALSE) {
  
  ##################################################################################################################
  
  # Function to identify uncertain animals 
  identifyUncertainROUTE<-function(ROUTE, ROUTE_TS, ROUTE_EX,  ALL_ROUTE_TS, NUM_ROUTE_TS, NUM_ANIMALS) {
    msgArr<-c()
    if (is.na(ROUTE))
      msgArr<-c(msgArr, 'TS parameters ROUTE and EX rows are missing')
    else {
      if (! ROUTE %in% ctROUTE) {
        if (!is.na(ROUTE_EX) & ! ROUTE_EX %in% ctROUTE)
          msgArr<-c(msgArr, 'EXROUTE does not contain a valid CT value')
        else if (!is.na(ROUTE_TS) & ! ROUTE_TS %in% ctROUTE)
          msgArr<-c(msgArr, 'TS parameter ROUTE does not contain a valid CT value')
      }
      if (NUM_ROUTE_TS == 1 & length(unique(na.omit(c(ROUTE_TS, ROUTE_EX)))) > 1)
        msgArr<-c(msgArr, 'Mismatch in values of TS parameter ROUTE and EXROUTE')
      else
        if (NUM_ROUTE_TS > 1 & !ROUTE %in% ALL_ROUTE_TS) 
          msgArr<-c(msgArr, 'Mismatch in values of TS parameter ROUTE and EXROUTE')
    }
    msg<-paste(msgArr, collapse = ' & ')
    return(ifelse(msg=="", as.character(NA), paste('FilterAnimalListRoute: ', msg, sep='')))
  }
  
  ##################################################################################################################
  
  if (!is.data.table(animalList)) {
    stop('Input parameter animalList must have assigned a data table ')
  } 
  if (is.null(routeFilter) | isTRUE(is.na(routeFilter)) | isTRUE(routeFilter=='')) {
    stop('Input parameter routeFilter must have assigned a non-empty value')
  }
  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter inclUncertain must be either TRUE or FALSE")
  }
  if (!(exclusively %in% c(TRUE,FALSE))) {
    stop("Parameter exclusively must be either TRUE or FALSE")
  }
  if (!(matchAll %in% c(TRUE,FALSE))) {
    stop("Parameter matchAll must be either TRUE or FALSE")
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
  
  
  # Get values of code list ROUTE from CDISC CT
  ctROUTE<-getCTCodListValues("ROUTE")
  
  
  # Extract all TS rows for parameter ROUTE, rename TSVAL to ROUTE_TS 
  # - remove duplicates
  # - limit to the set of studies for input set of animals
  tsROUTEall<-merge(unique(TS[TSPARMCD == 'ROUTE', .(STUDYID,ROUTE_TS=toupper(trimws(TSVAL)))]), animalStudies, by='STUDYID')
  # Add studies with no TS parameter ROUTE from the set of studies for input set of animals
  tsROUTEall<-
    rbindlist(list(tsROUTEall,
                   fsetdiff(animalStudies, 
                            tsROUTEall[,.(STUDYID)])[!is.na(STUDYID),.(STUDYID, ROUTE_TS=as.character(NA))]),
              use.names=TRUE, fill=TRUE)
  
  # Add variables with 
  #  - count of number of distinct ROUTE per study
  #  - concatenation of all ROUTE per study (for studies with one ROUTE this is equal to ROUTE_TS)
  tsROUTEall[, `:=` (ALL_ROUTE = unique(ROUTE_TS), NUM_ROUTE_TS = .N), by = STUDYID]
  tsROUTEall[,`:=`(ALL_ROUTE_TS = c(.SD)), by = STUDYID, .SDcols='ROUTE_TS']
  
  # JOin the list of studies/routes with EX to get all animal level ROUTES 
  #  - join to POOLDEF, to get USUBJID for pooled data
  #  - the list will contain STUDYID/USUBJID duplicates for studies with multiple ROUTE registered in TS               
  allAnimals<-
    merge(tsROUTEall,
          merge(animalList[,.(STUDYID, USUBJID)], 
                unique(merge(unique(EX[,.(STUDYID, USUBJID, POOLID, ROUTE_EX=toupper(trimws(EXROUTE)))]),
                       POOLDEF[,.(STUDYID, POOLID, USUBJID)],
                       by=c('STUDYID', 'POOLID'),all.x=TRUE)[,.(STUDYID, USUBJID=ifelse(USUBJID.x=='',USUBJID.y,USUBJID.x ), ROUTE_EX)]), 
                by=c('STUDYID', 'USUBJID'), all.x = TRUE),
           by='STUDYID', allow.cartesian = TRUE)
  #  Add variables 
  #    - ROUTE with the first non-empty species value from EX or TS
  #    - count of unique USUBJID per study (there is expected to be one usubjid per studyid per TSPARMCD 'ROUTE' )
  allAnimals[,`:=` (ROUTE=fcoalesce(ROUTE_EX,ROUTE_TS))][, `:=` (NUM_ANIMALS = .N), by = .(STUDYID, USUBJID)]

  # Identify uncertain animals - add variable UNCERTAIN_MSG
  allAnimals[,`:=` (UNCERTAIN_MSG=mapply(identifyUncertainROUTE, ROUTE, ROUTE_TS, ROUTE_EX,  ALL_ROUTE_TS, NUM_ROUTE_TS, NUM_ANIMALS ))]
  
  # Extract unique set of animals and related ROUTE - exclude uncertain animals
  allAnimalsUniq<-unique(allAnimals[is.na(UNCERTAIN_MSG), .(STUDYID, USUBJID, ROUTE)])
  
  # Extract animals matching the routeFilter 
  foundAnimals<-unique(allAnimalsUniq[ROUTE %in% routeFilter, .(STUDYID, USUBJID, ROUTE)])
  
  if (exclusively) {
    # Exclude all animals belonging to studies which have other ROUTEs than the requested
    foundAnimals<-
      merge(foundAnimals,
            # Set of studies to keep:
            fsetdiff(unique(foundAnimals[,.(STUDYID)]),
                     # Set of studies (included in the found set of animals with matching ROUTE values) with possible 
                     # ROUTE values not included in the routeFilter:                       
                     unique(fsetdiff(merge(# Set of possible ROUTE values per study in the input set of animals:
                                           unique(allAnimalsUniq[,.(STUDYID, ROUTE)]),
                                           unique(foundAnimals[,.(STUDYID)]), by='STUDYID'),
                                     unique(foundAnimals[,.(STUDYID, ROUTE)]))[,.(STUDYID)])),
            by='STUDYID')
  }
  
  if (matchAll & length(routeFilter) > 1) {
    # Exclude animals belonging to studies which do not match all requested ROUTE values
    foundAnimals<-
      merge(foundAnimals,
            # Studies with equal number of distinct number of ROUTE values as included in the requested set of ROUTEs
            unique(foundAnimals[,.(STUDYID,ROUTE)])[,.(NUM_ROUTE = .N), by = STUDYID][NUM_ROUTE == length(routeFilter),.(STUDYID)],
            by='STUDYID')
  }
  
  if (inclUncertain)
    # Add the uncertain animals
    foundAnimals<-rbindlist(list(foundAnimals, 
                                 unique(allAnimals[!is.na(UNCERTAIN_MSG), .(STUDYID, USUBJID, ROUTE, UNCERTAIN_MSG)])),
                            use.names=TRUE, fill=TRUE)
  
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
