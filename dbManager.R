
# sysParameters imports dbFullName, metaDataRoot
source("sysParameters.R")
library(readxl)



# Get list of all SEND 3.1 domains from CDISC SEND 3.1 library archive
allDomains <- dplyr::pull(read_excel(paste(metadataRoot,"sendig-3-1-excel.xls", sep="/"), sheet=3), 1)
# list of trial design domains
notGeneralDataDomains <- c("TS","TA","TE","TX","DM","POOLDEF","SUPPQUAL", "RELREC")
# list of data domains
suppDomains <- paste("SUPP", setdiff(allDomains, notGeneralDataDomains), sep="")



ConnectToSend <- function() {
  # just returns a SQlite connect to the database
  return(RSQLite::dbConnect(RSQLite::SQLite(), dbFullName))
}

GenericQuery <- function(query_string, query_params=NULL) {
  # a function to make any query to the SEND db 
  # just ensures that it gets the conn 
  # gets opened and closed
  # @query_string: the string to be used to query the database
  # @query_params: the string to be used to query the database
  
  conn <- ConnectToSend()
  if (is.null(query_params)){
    query_result <- RSQLite::dbGetQuery(conn, query_string)
  } else {
    query_result <- RSQLite::dbGetQuery(conn, query_string, query_params)
  }
  
  RSQLite::dbDisconnect(conn)
  return(query_result)
}

GenericExecute <- function(execute_string, execute_params=NULL, commit=FALSE) {
  # like the GenericQuery, just executes a string on the send db
  # @execute_string: the string to be used to query the database
  # @execute_params: the string to be used to query the database
  # @commit: will commit if true
  
  conn <- ConnectToSend()
  if (is.null(execute_params)){
    RSQLite::dbExecute(conn, execute_string)
  } else {
    RSQLite::dbExecute(conn, execute_string, query_params)
  }
  
  if (commit) {
    RSQLite::dbCommit(conn)
  }
  
  RSQLite::dbDisconnect(conn)
  
}


CreateIdx <- function(domainsVector=c('AN', allDomains), indexVector=c('STUDYID', 'USUBJID')) {
  # creates an index on studyid for everytable in the database
  # will create and index on every index in IndexList
  # for every domain in domainsList
  for (dm in domainsVector) {
    
    columns <-  GenericQuery(sprintf('PRAGMA table_info(%s);', dm))$name
    
    for (idx in indexVector) {
      
      if (any(idx %in% columns)) {
        
        exists_query <- sprintf("SELECT * FROM sqlite_master WHERE type='index' and name='index_%s_%s';", idx, dm)
        
        matches <- GenericQuery(exists_query)
        
        if (nrow(matches) == 0){
          query_execute <- sprintf("CREATE INDEX index_%s_%s ON %s (%s)", idx, dm, dm, idx)
          GenericExecute(query_execute)
        }
      }
    }
  }
}