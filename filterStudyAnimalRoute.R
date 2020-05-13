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
# Purpose       : 1 - Extract a list of SEND study ids which fulfills a specified 
#                     route of administration from a pooled SEND data store.
#                 2 - Extract a list of animal ids for the list of study ids which 
#                     fulfils the specified route of administration for studies where 
#                     more than one route of administration is specified at study level.
#
# Description   : Two function are defined 
#                   - GetStudyListROUTE
#                   - FilterAnimalListRoute
#                 Each of these function sare described below
#
# Input         : See description for each function below
#
# Output        : See description for each function below
#
# Parameters    : See description for each function below
#
###################################################################################

library(data.table)

###################################################################################
# Function:     : GetStudyListROUTE
#
# Description   : Returns a data table with a list of studyid and route values
#                 extracted from TS (where TSPARMCD is 'ROUTE') where
#                 the TSVAL value is equal to one or more  given route input value(s).
#                 The comparison of route values is done case insensitive.
#                 There may be specified multiple values of route per study in TS.
#                 To indicate whether this is the case for the studies included in 
#                 the output table, an extra variables is added:
#                   - the number of distinct route values per STUDYID in TS (if greater 
#                     than 1, multiple routes are specified for that study in TS even though 
#                     only one route has been extracted for that study).
#                 If data table of studyids are given as input - the rows in the the 
#                 output table are limited to be withn this set of studyids
#
# Input         : The TS domain - is imported from the pooled SEND data store if 
#                 it doesn't exist in workspace
#
# Output        : The function GetStudyListROUTE returns a data table with these columns:
#                     STUDYID   (character)
#                     ROUTE     (character) (<-TS.TSVAL where TSPARMCD='ROUTE')
#                     NUM_ROUTE (character) - the number of routes defined per study in TS.
#
# Parameters    : routeFilter:  mandatory, the route value(s) to use as criterion for 
#                               filtering of the studies.
#                               It can be a single string, a vector or a list of 
#                               multiple strings.
#                 studyList:    optional, a data table with a list of studies to 
#                               limit the output to be within this set of studies
###################################################################################

GetStudyListROUTE<-function(routeFilter=NULL, studyList=NULL) {
  
  if (is.null(routeFilter) | isTRUE(is.na(routeFilter)) | isTRUE(routeFilter=='')) {
    stop('A routeFilter must be specified')
  } 
  
  if (!exists('TS')) {
      # import TS if it's not already exists
      importSENDDomains(c('TS'))
  }
  
  # Extract all TS rows for parameter ROUTE, rename TSVAL to ROUTE - remove duplicates
  TSAllROUTE<-unique(TS[TSPARMCD == 'ROUTE', .(STUDYID,ROUTE=toupper(TSVAL))])
  
  if (!(is.null(studyList) | isTRUE(is.na(studyList)) | isTRUE(studyList==''))) {
    # Limit to the set of studies given as input
    TSAllROUTE<-merge(TSAllROUTE,studyList, by='STUDYID') 
  } 
    
  # Extract all TS rows for parameter ROUTE, rename TSVAL to ROUTE - remove duplicates
  TSAllROUTE<-unique(TS[TSPARMCD == 'ROUTE', .(STUDYID,ROUTE=toupper(TSVAL))])
 
  # Add variable with count of number of distinct routes per study
  TSAllROUTE[, `:=` (NUM_ROUTE = .N), by = STUDYID]
 
  # extract filtered set of rows and return
  return(TSAllROUTE[ROUTE %in% toupper(trimws(routeFilter))])
}

###################################################################################
# Function      : FilterAnimalListRoute
#
# Description   : Returns a data table with a set of animals extratced from the table of  
#                 animals given as input where the animals are treated via the route of  
#                 administration given as input (a list of multiple route values may be 
#                 given as input).
#                 Animals from the input set, which are included in studies where one of  
#                 of the input route values is registered in TS (and TS have only this route 
#                 value included) are included in the output set.
#                 Animals from the input set, belonging to studies 
#                   - with no route value registered  or  
#                   - with more than one route value registered
#                 in TS are included in the output set if they exists in EX with the EXROUTE 
#                 mathcing one of the input route values. The filtering of EX rows is done
#                 both at subject and pool level rows.
#                 The comparison of route values is done case insensitive.
#
# Input         : The EX and POOLDEF domains - are imported from the pooled SEND data 
#                 store if they don't exist in workspace.
#                 A data table specified in the input parameter animalList:
#                   It contains the list of animals to filter for specified route value(s)
#                   - must contain these character variables:
#                       STUDYID
#                       USUBJID
#                     other variables may be included
#                 One of more values for ROUTE to include in the the filtering 
#
# Output        : A data table with the character columns:
#                   STUDYID
#                   USUBJID
#                   ROUTE
#                 plus any additional columns which may be included in the input data animalList
#
# Parameters    : animalList:   mandatory, data table (see Input)
#                 routeFilter:  mandatory, the route value(s) to use as criterion for 
#                               filtering of the input data table.
#                               It can be a single string, a vector or a list of 
#                               multiple strings. 
######################################### ##########################################
FilterAnimalListRoute<-function(animalList=NULL, routeFilter=NULL) {
  
  if (is.null(animalList) | isTRUE(is.na(animalList)) | isTRUE(animalList=='')) {
    stop('Input parameter animalList must have assigned a data table ')
  } 
  
  if (is.null(routeFilter) | isTRUE(is.na(routeFilter)) | isTRUE(routeFilter=='')) {
    stop('Input parameter routeFilter must have assigned a data table ')
  } 
  
  # List of studyid values included in the input table of animals
  animalStudies<-unique(animalList[,.(STUDYID)])
  
  if (!exists('EX')) {
    importSENDDomains(c('EX'), animalStudies)
  }
  if (!exists('POOLDEF')) {
    importSENDDomains(c('POOLDEF'), animalStudies)
  }
  
  # Get list of studies with ROUTE parameter value(s) included in the routeFIlter
  tsROUTE<-GetStudyListROUTE(routeFilter, animalStudies)
  
  # Extract list of animals for studies with only one ROUTE specified in TS
  studyLvlAnimals<-merge(tsROUTE[NUM_ROUTE==1, .(STUDYID, ROUTE)], 
                         animalList[,.(STUDYID, USUBJID)], by='STUDYID')[,.(STUDYID, USUBJID, ROUTE)]
  
  # Extract list of animals belonging to studies with no route value registered or with more 
  # than one route value registered in TS:
  #  - extract list of studies included in animalList which are not included in studyLvlAnimals
  #  - join with EX to get all rows for these studies - include only rows where EXROUTE fits the routeFIlter
  #  - outer join with POOLDEF to get USUBJID for pools included in EX
  #  - join with unique set of studyid/usubjid from animalList to extract the animals to include in the output table
  EXLvlAnimals<-merge(merge(unique(merge(EX[EXROUTE %in% routeFilter,.(STUDYID, USUBJID, POOLID, ROUTE=EXROUTE)],
                                   fsetdiff(animalStudies, studyLvlAnimals[,.(STUDYID)]),
                                   by='STUDYID')),
                            POOLDEF[,.(STUDYID, POOLID, USUBJID)],
                            by=c('STUDYID', 'POOLID'),all.x=TRUE
                           )[, USUBJID := mapply(ifelse,USUBJID.x=='',USUBJID.y,USUBJID.x )][,.(STUDYID, USUBJID, ROUTE)],
                      unique(animalList[,.(STUDYID,USUBJID)]), by=c('STUDYID', 'USUBJID'))
  
  # Return the list of extracted animals merged with the input set of animals to keep
  # any additional columns from the input table 
  return(merge(animalList,
               rbindlist(list(studyLvlAnimals, EXLvlAnimals),use.names=TRUE, fill=TRUE),
               by=c('STUDYID', 'USUBJID')))
}
