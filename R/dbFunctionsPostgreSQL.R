################################################################################
## Database functions specific for PostgreSQL
##  - helper functions called by the published database functions.
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2023-02-14   Cecily Abraham        Initial version
################################################################################


## Connect function
connectDB_postgresql <- function(dbPath, dbHost, dbUser, dbPwd, dbPort = 5432) {
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = dbPath,
    host = dbHost,
    user = dbUser,
    password = dbPwd,
    port = dbPort)
}


## Check if you can connect to PostgreSQL DB with name 'dbPath'
# Returns boolean
canConnectDB_postgresql <- function(dbPath, dbHost, dbUser, dbPwd, dbPort = 5432) {
  DBI::dbCanConnect(
    RPostgres::Postgres(),
    dbname = dbPath,
    host = dbHost,
    user = dbUser,
    password = dbPwd,
    port = dbPort)
}


## Disconnect function
disconnectDB_postgresql <- function(dbHandle) {
  RPostgres::dbDisconnect(dbHandle)
}


## Execute a generic query
# Result data set always returned as data table
#  ## ADD POSSIBILITY FOR MULTIPLE QUERY PARAMS
genericQuery_postgresql <- function(dbHandle, queryString, queryParams = NULL) {
  if (is.null(queryParams)){
    data.table::as.data.table(RPostgres::dbGetQuery(dbHandle, queryString))
  } else {
    formatQuery <- DBI::sqlInterpolate(dbHandle, queryString, paste(queryParams))
    print(formatQuery)
    data.table::as.data.table(RPostgres::dbGetQuery(dbHandle, formatQuery))
  }
}


################################################################################
##  Check if specified table exists in database
#  Returns boolean
dbExistsTable_postgresql <- function(dbHandle, table) {
  table %in% RPostgres::dbListTables(dbHandle)
}


################################################################################
##  Return list of columns in specified database table
dbListFields_postgresql <- function(dbHandle, table) {
  RPostgres::dbListFields(dbHandle, table)
}
