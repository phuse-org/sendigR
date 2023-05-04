################################################################################
## Database functions specific for SQLite
##  - helper functions called by the published database functions.
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2020-12-04   Bo Larsen             Initial version
################################################################################

## Connect function
connectDB_sqlite<-function(dbPath) {
  RSQLite::dbConnect(RSQLite::SQLite(), dbPath)
}

## Disconnect function
disconnectDB_sqlite<-function(dbHandle) {
  RSQLite::dbDisconnect(dbHandle)
}


## Send generic statement
dbSendStatement_sqlite <- function(dbHandle, queryString, queryParams = NULL) {
  if (is.null(queryParams)){
    RSQLite::dbSendStatement(dbHandle, queryString)
  } else {
    RSQLite::dbSendStatement(dbHandle, queryString, queryParams)
  }
}

## Clear Result
dbClearResult_sqlite <- function(res) {
  RSQLite::dbClearResult(res)
}


## Begin Transaction
dbBegin_sqlite <- function(dbHandle) {
  RSQLite::dbBegin(dbHandle)
}


## Rollback Transaction
dbRollback_sqlite <- function(dbHandle) {
  RSQLite::dbRollback(dbHandle)
}


## Commit Transaction
dbCommit_sqlite <- function(dbHandle) {
  RSQLite::dbCommit(dbHandle)
}


## Write Table
dbWriteTable_sqlite <- function(dbHandle, name, value, append=TRUE) {
  RSQLite::dbWriteTable(dbHandle,
                        name = name,
                        value = value,
                        append = append)
}

## Execute a generic query
# Result data set always returned as data table
#  ## ADD
#  # POSIBILITY FOR MULTIPLE QUERY PARAMS
#  ## ADD POSIBILITY FOR MULTIPLE QUERY PARAMS
genericQuery_sqlite<-function(dbHandle, queryString, queryParams) {
  if (is.null(queryParams)){
    data.table::as.data.table(RSQLite::dbGetQuery(dbHandle, queryString))
  } else {
    # Input query parameters are converted to a unnamed list used as bind variable
    # regardless of the type of input
    data.table::as.data.table(RSQLite::dbGetQuery(dbHandle, queryString,
                                list(unname(unlist(list(list(queryParams)))))))
  }
}

################################################################################
##  Check if specified table exists in database
#  Returns boolean
dbExistsTable_sqlite <- function(dbHandle, table) {
  table %in% RSQLite::dbListTables(dbHandle)
}

################################################################################
##  Return list of columns in specified database table
dbListFields_sqlite <- function(dbHandle, table) {
  RSQLite::dbListFields(dbHandle, table)
}

# Return list of tables in the SQLite database
dbGetTables_sqlite <- function(dbHandle) {
  queryString <- "select name
                  from sqlite_master
                  where type ='table' and name not like 'sqlite_%'"
  res <- data.table::as.data.table(RSQLite::dbGetQuery(dbHandle, queryString))
  res$name;
}

# Return list of indexes in the SQLite database
dbGetIndexes_sqlite <- function(dbHandle) {
  queryString <- "select name from sqlite_master
                  where type = 'index'
                  and name like '%sendigr%'"
  res <- data.table::as.data.table(RSQLite::dbGetQuery(dbHandle, queryString))
  res$name;
}

