###################################################################################
# Script name   : importSENDDomains.R
# Date Created  : 23-Jan-2020
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Import a set of specified SEND domains from an SQLite database 
#
# Description   : Import each domain from the given list of domains from a
#                 database into a data table in the global environment.
#                 Each data table are named equal to the domain in uppercase.
#                 All variables are included for each domain.
#                 If a list of studyid values are inlcuded in the the studyList,
#                 parameter is included, a subset of rows for these studies are 
#                 imported from each domain - else all rows are imported from each 
#                 domain.
#                 The actual import of data from the database is done by a function 
#                 named doImportDomain which points at a function specific for the
#                 databases type used in the actual environment
#
# Input         : A database including SEND data for a pooled set of studies.
#
# Output        : A data table for each domain to be imported
#
# Parameters    : Contains one input parameter:
#                   domainList  - a list or vector containing the list of domains 
#                                 to be imported 
#                   studyList   - a data frame or table containing a list of studyid 
#                                 values to be included in the extraction of rows 
#                                 from the database.
#                                 This parameter is optional.
#
###################################################################################

library(data.table)

# Import all specified SEND domains
importSENDDomains<-function(domainList, studyList=NULL) {
  
  ############################################################
  # Import one domain - assign output into a global table 
  importDomain<-function(domain) {
    assign(toupper(domain),doImportDomain(domain, studyList), envir=.GlobalEnv)
    return()
  }
  ############################################################


  # Import each domain from the specified list
  lapply(domainList, importDomain)
  
}

