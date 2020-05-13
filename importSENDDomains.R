###################################################################################
# Script name   : importSENDDomains.R
# Date Created  : 23-Jan-2020
# Documentation : <if relevant, reference to specification document>
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
# Description   : Import each domain from the given list of domains from an SQLite
#                 database into a data table in the global environment.
#                 Each data table are named equal to the domain in uppercase.
#                 All variables are included for each domain.
#                 If a list of studyid values are inlcuded in the the studyList,
#                 parameter is included, a subset of rows for these studies are 
#                 imported from each domain - else all rows are imported from each 
#                 domain.
#
# Input         : A SQLite database including SEND data for a pooled set of studies
#                 A R script file sysParameters.R located in working directory is sourced
#                 sourced - that script defines the location and name of the SQLite 
#                 database (in variable dbFullName)
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
# Usage notes   : If necessary, modify sysParameters.R with location and name
#                 of the database to match the actual environment.
#                 Source the this script.
#                 Execute function importSENDDomains for each set of domains to 
#                 be imported.
#                 Examples 
#                 - import the trial design domains for all studies: 
#                     importSENDDomains(c("TX","TS", "TA", "TE"))
#                 - import clinical signs for list of studies:
#                     importSENDDomains(c("CL"), studyIdList)
#
#                 NB: If the actual pooled SEND store is not located in SQLite database 
#                 - substitue this version of the sccript with a new script accessing 
#                   the actual data store
#                 - ensure that function name, input parameter and output table format 
#                   are equal to the function defined in this script.
#
###################################################################################

library(RSQLite)
library(data.table)

# Import all specified SEND domains
importSENDDomains<-function(domainList, studyList=NULL) {
  
  ############################################################
  # Import one domain
  importDomain<-function(domain) {
    stmt<-paste("select t.* from ", domain, " t", sep="")
    if (!is.null(studyList)) {
      # Add a join to the specified set of studyids to limit the set of imported rows
      stmt<-paste(stmt, " join temp_studyList s on s.studyid = t.studyid", sep="")
    }
    assign(toupper(domain),data.table(dbGetQuery(db, stmt)), envir=.GlobalEnv)
  }
  ############################################################
  source("sysParameters.R")
  
  # connect to database
  db<-dbConnect(RSQLite::SQLite(), dbFullName)
  
  if (!is.null(studyList)) {
    # Save the content of studyList parameter as a temporary table in the database
    dbWriteTable(db, "temp_studyList", studyList, temporary=TRUE)
  }
  
  
  # Import each domain from the specifed list
  lapply(domainList, importDomain)
  
  dbDisconnect(db)
}

