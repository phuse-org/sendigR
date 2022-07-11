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
