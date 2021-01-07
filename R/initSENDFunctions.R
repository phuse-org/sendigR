################################################################################
## The functions
##    initEnvironment
##    disconnectDB
##    genericQuery
##
## A set of internal helper functions are included too
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2020-12-04   Bo Larsen             Initial version
################################################################################


################################################################################
## Published functions
################################################################################

#' Initialize the environment.
#'
#' Defines relevant global variables for - type of SEND database - pointer to
#' database (filename, connect string....) - username/password to database (if
#' relevant) Import CDISC CT data from specified CT file in Excel format.
#'
#' Multiple types of databases are supported - for each of the supported db
#' types:
#' \itemize{
#'   \item A metadata record is included describing the relevant properties for
#' a type of database
#'   \item A function to connect to the specific database
#'   \item function to import data for a SEND domain from the specific database
#' }
#'
#' @param dbType Mandatory - type of database
#' @param dbPath Mandatory - the pointer to the database (path to file or db
#'   name)
#' @param dbUser Username - mandatory if login credentials are required for the
#'   specific db type
#' @param dbPwd Password - Mandatory if login credentials are required for the
#'   specific db type
#' @param dbSchema Optional - The table owner of the SEND table in the specific
#'   database
#' @param ctFile Mandatory - name (full path) of CDISC CT file in Excel xls
#'   format to be imported
#'
#' @return Token for the open database connection
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' initEnvironment(dbType='sqlite',
#'                dbPath='//servername/SendData/db/send.db',
#'                ctFile='//servername/SendData/metadata/SEND_Terminology_2019-12-27.xls')
#' initEnvironment(dbType='oracle',
#'                 dbPath='dbserver:1521/send_db',
#'                 dbUser='ME',
#'                 dbPwd='mypassword',
#'                 dbSchema = 'send',
#'                 ctFile='//servername/SendData/metadata/SEND_Terminology_2019-12-27.xls')
#' }
#'
#'
################################################################################
# Import some functions from used packages to used by all functions in the
# package:
#' @importFrom data.table .N
#' @importFrom data.table .SD
#' @importFrom data.table %like%
#' @importFrom data.table :=
#' @importFrom stats na.omit
#' @importFrom magrittr %>%
#'
initEnvironment<-function(dbType=NULL,
                          dbPath=NULL,
                          dbUser=NULL,
                          dbPwd=NULL,
                          dbSchema=NULL,
                          ctFile=NULL) {

  ## Evaluate database specification parameters

  # dbType
  errMsg<-paste0('Parameter dbType must be one of: ', paste(sapply(validDbTypes[,c('db_type')], paste0), collapse=','))
  if (is.null(dbType) | isTRUE(is.na(dbType)) | isTRUE(dbType==''))
    stop(errMsg)
  else if (nrow(dbProperties<-validDbTypes[db_type==tolower(dbType)]) == 0)
    stop(errMsg)
  dbType<-tolower(dbType)

  errMsg<-'Parameter %s must be a non-empty string'
  # dbPath
  if (is.null(dbPath) | isTRUE(is.na(dbPath)) | isTRUE(dbPath==''))
    stop(sprintf(errMsg, 'dbPath'))

  # dbUSer and dbPwd
  if (as.logical(dbProperties[,c('req_credentials')])) {
    if (is.null(dbUser) | isTRUE(is.na(dbUser)) | isTRUE(dbUser==''))
      stop(sprintf(errMsg, 'dbUser'))
    if (is.null(dbPwd) | isTRUE(is.na(dbPwd)) | isTRUE(dbPwd==''))
      stop(sprintf(errMsg,'dbPwd'))
  }

  # dbSchema - set to "" if no value are specified
  if (is.null(dbSchema) | isTRUE(is.na(dbSchema)) | isTRUE(dbSchema==''))
    dbSchema<-""

  ## Verifying specified CT file - and import
  if (is.null(ctFile) | isTRUE(is.na(ctFile)) | isTRUE(ctFile==''))
    stop(sprintf(errMsg,'ctFile'))
  else
    ctDataFile <- importCtFile(ctFile)

  ## Check for existence of package for the specified db type
  ##
  if (!requireNamespace(as.character(dbProperties[,c('package_name')]), quietly = TRUE))
      stop(sprintf('Package "%s" needed for access to a(n) %s database. Please install it.',
                   as.character(dbProperties[,c('package_name')]),
                   as.character(dbProperties[,c('db_type')])),
          call. = FALSE)

  ## Connect to the database
  #  - execute the function specific for the db type to create the connections
  if (as.logical(dbProperties[,c('req_credentials')]))
    dbInputParams<-paste0('(dbPath, dbUser, dbPwd)')
  else
    dbInputParams<-paste0('(dbPath)')
  dbHandle<-eval(parse(text=paste0('connectDB_',dbProperties[,c('db_type')],dbInputParams)))

  ## Verify existence of function specific for db type to execute a generic query
  genericQueryName <- paste0('genericQuery_', dbProperties[,c('db_type')])
  if (!exists(genericQueryName))
    stop(sprintf('A function named %s is missing', genericQueryName))

  ## Verify existence of function specific for db type to execute disconnect
  #  from database
  disconnectDBName <- paste0('disconnectDB_', dbProperties[,c('db_type')])
  if (!exists(disconnectDBName))
    stop(sprintf('A function named %s is missing', disconnectDBName))

  ## Verify existence of function specific for db type to verify existence of
  #  specific table in database
  dbExistsTableName <- paste0('dbExistsTable_', dbProperties[,c('db_type')])
  if (!exists(dbExistsTableName))
    stop(sprintf('A function named %s is missing', dbExistsTableName))

  ## Verify existence of function specific for db type to list columns in a
  #  specific table in database
  dbListFieldsName <- paste0('dbListFields_', dbProperties[,c('db_type')])
  if (!exists(dbListFieldsName))
    stop(sprintf('A function named %s is missing', dbListFieldsName))

  ## Return a db token to be included in all calls to database related functions.
  #  It includes:
  #   - The temporary RData file containing imported CDISC CT data
  #   - Handle to the open database connection
  #   - Database schema to select data from
  #   - Pointers to function specific for the database type:
  #     - execution of a generic SQL query
  #     - disconnect from database
  return (list(ctDataFile = ctDataFile,
               dbHandle = dbHandle,
               dbSchema = dbSchema,
               genericQuery = get(genericQueryName),
               disconnectDB = get(disconnectDBName),
               dbExistsTable = get(dbExistsTableName),
               dbListFieldsTable = get(dbListFieldsName)
  ))
}

#' Disconnect from the open database.
#'
#' Disconnect from database by calling the disconnect function specific for
#' the current database type.
#'
#' @param dbToken Mandatory - token for the open database connection
#'
#' @export
#'
#' @examples
#' \dontrun{
#' disconnectDB()
#' }
disconnectDB <- function(dbToken) {
  dbToken$disconnectDB(dbToken$dbHandle)
}

#' Execute database query and returns fetched rows.
#'
#' The function executes a SQL select statements in the database and returns
#' the fetched set of rows as a data table.
#' The underlying functions specific for the database type is called to do the
#' SQL execution.
#'
#' @param dbToken Mandatory - token for the open database connection
#' @param queryString Mandatory - the select statement to execute
#' @param queryParams Optional - a varibale with values for bind variable
#' referenced in the where clause of the select statemt
#'
#' @return Data table with the set of fetched rows
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' genericQuery(dbToken,
#'              'select studyid, tsseq, tsgrpid, tsparmcd, tsval from ts')
#' genericQuery(dbToken,
#'              'select studyid, tsval from ts where tsprmcd = 'SDESIGN' and studyid in (:1)',
#'              list('1234546','222333','444555'))
#' }
genericQuery <- function(dbToken, queryString, queryParams=NULL) {
  #  Prepare the given select statement by adding potential schema name
  #  to all tables and execute the genericQuery function specific for the
  #  actual db type
  #  ## ADD POSIBILITY FOR MULTIPLE QUERY PARAMS
  return(dbToken$genericQuery(dbToken$dbHandle,
                              selectStmtAddSchema(dbToken$dbSchema,
                                                  queryString),
                              queryParams))
}

################################################################################
## Helper functions and metadata
################################################################################

## The currently supported database types
validDbTypes <-
  data.table::data.table(db_type         = c('sqlite',  'oracle'),
                         req_credentials = c( FALSE,     TRUE),
                         package_name    = c('RSQLite', 'ROracle'))

################################################################################
##  Check if specified table exists in database
#  Returns boolean
dbExistsTable <- function(dbToken, table) {
  if (dbToken$dbSchema == '')
    return(dbToken$dbExistsTable(dbToken$dbHandle, table))
  else
    return(dbToken$dbExistsTable(dbToken$dbHandle, dbToken$dbSchema ,table))
}

################################################################################
##  Return list of columns in specified database table
dbListFields<- function(dbToken, table) {
  if (dbToken$dbSchema == '')
    return(dbToken$dbListFields(dbToken$dbHandle, table))
  else
    return(dbToken$dbListFields(dbToken$dbHandle, dbToken$dbSchema, table))
}

################################################################################
## Import specified CDISC CT file
# - save extracted code lists and value in temporary Rdata files
# - return the R data file name
importCtFile<-function(ctFile) {
  CodelistCode <- NULL

  # Check file is XLS and exists
  if (! tolower(xfun::file_ext(ctFile)) == 'xls')
    stop(paste0('The ctFile ' , ctFile, ' is not an XLS file'))
  if (!file.exists(ctFile)) {
    stop(paste0('The ctFile ' , ctFile, 'could not be found'))
  }

  # Import content from worksheet named SEND<sep>Terminology<something>
  # - include relevant columns and all rows
  ctSheets<-readxl::excel_sheets(ctFile)
  ctAll<-data.table::as.data.table(readxl::read_xls(ctFile,
                                                    sheet=ctSheets[grepl('send[_ ]terminology',
                                                                         tolower(ctSheets) )]))[,c("Code", "Codelist Code", "CDISC Submission Value")]
  data.table::setnames(ctAll, c("Codelist Code","CDISC Submission Value"),
                       c("CodelistCode","CDISCSubmissionValue"))


  # Extract all code list names
  CDISCctCodeLists = ctAll[is.na(CodelistCode), c('Code', 'CDISCSubmissionValue')]
  data.table::setnames(CDISCctCodeLists, c("Code","CDISCSubmissionValue"),
                                         c("CodelistCode","CodeList"))
  # Extract all code list values
  CDISCctCodeValues = ctAll[!is.na(CodelistCode), c('CodelistCode','CDISCSubmissionValue')]

  # Save the extracted CDISC CT code lists and values in a temporary RData file
  CDISCctFile = tempfile('CDISCct', fileext='.RData')
  save(CDISCctCodeLists, CDISCctCodeValues, file = CDISCctFile)

  # Return file name with saved data
  return(CDISCctFile)
}

################################################################################
## Extract values for specific code lists from extracted CDIUS CT code lists/values
getCTCodListValues<-function(dbToken, pCodeList=NULL) {
  if (is.null(pCodeList) | isTRUE(is.na(pCodeList)) | isTRUE(pCodeList=='')) {
    stop('Input parameter codeList must have assigned a code list name')
  }

  # load the CDISC ct data from temporary RData file
  load(dbToken$ctDataFile)

  # Check if the requested code list exists
  if (!toupper(pCodeList) %in% CDISCctCodeLists$CodeList) {
    stop('The specified code list does not exist in the CDISC terminolgy file')
  }

  # Extract and return a character vector with all value for the requested code list
  return(data.table::merge.data.table(CDISCctCodeLists[CodeList==toupper(pCodeList), c('CodelistCode')],
                                      CDISCctCodeValues[!is.na(CodelistCode)])$CDISCSubmissionValue);
}

################################################################################
## Take a SQL statement as input
#   return the statement modified in this way:
#   - substitute line shifts (\n) with space
#   - substitute multiple consecutive spaces with one space
#   - substitute '==' with '='
#   - if the global defined db schame names is not empty -
#     - add the schema plus '.' in front of all table names ,
#       i.e. names following a 'from ' or 'join ' except
#       subqueries (starting with '(')
selectStmtAddSchema <- function(dbSchema, stmt) {
  vSchema <- ifelse(dbSchema=='','',paste0(dbSchema,'.'))
  return(
    stmt %>%
      stringr::str_replace_all('\n', ' ') %>%
      stringr::str_replace_all(' +', ' ') %>%
      stringr::str_replace_all('=+', '=') %>%
      stringr::str_replace_all(stringr::regex('(from |join )([^(])', ignore_case = TRUE),
                               paste0('\\1', vSchema, '\\2'))
  )
}

################################################################################
# Prepare a set of final set rows to be returned from a data extraction
# function (i.e. the function calling this function).
#  - merge potential pairs of xxx_MSG columns.x/y originated from both
#       - the data table given as input to the calling extraction function
#       - generated by extraction function
#    respectively
#  - set the column order to be
#     1 - the columns from the data table given as input to the calling
#         extraction function excluding potential xxx_MSG columns.
#     2 - the columns added by the calling extraction function
#     3 - the potential xxx_MSG columns in alphabetically order
#
# Parameters:
#   dt - the data table to process
#   srcCols - the list of columns (vector) in the data table given as input to the calling
#      function. May be empty.
#   addCols - the list of columns (vector) added by the calling function
prepareFinalResults <- function(dt, srcCols, addCols) {
  if (exists('..colList'))
    rm('..colList')
  # Get list of message columns which may be included from from two merged
  # tables (i.e. both a _MSG.x and _MSG.y column included)
  mergedMsgColList <- names(dt)[names(dt) %like% 'MSG.x$']

  # Statement to merge content of a given message column which exists in both tables
  #  - merge the _MSG column from each of the merged tables into one column
  #  - non-empty messages are separated by '|'
  #  - exclude the original _MSG columns after the merge
  mergeMsgColStmt <- "dt[,`:=` (msgCol=ifelse(!is.na(msgCol.x) & !is.na(msgCol.y), paste(msgCol.y, msgCol.x, sep='|'), DescTools::Coalesce(msgCol.x, msgCol.y)))][, `:=` (msgCol.x=NULL,msgCol.y=NULL)]"

  # Execute merge for each of message column included from two tables
  # - remove the last '.x' from message column name
  for (msgCol in mergedMsgColList)
      eval(str2expression(gsub('msgCol', gsub('.x$','',msgCol), mergeMsgColStmt)))

  if (length(srcCols) == 1 & srcCols[1] == '')
    # Only columns added by calling function included
    colList <- addCols
  else
    # Add the columns specified in addCols to the list of columns specified in
    # srcCols - excluding any potential MSG columns
    colList <- append(srcCols[!srcCols %like% 'MSG$'], addCols)

  # Get list of potential message columns
  # - sort names alphabetically
  msgColList <- sort(names(dt)[names(dt) %like% 'MSG$'])
  if (length(msgColList) > 0)
    # Add message column list
    colList <- append(colList, msgColList)

  # Include specified columns in correct order and return
  return(eval(str2expression("data.table::setcolorder(dt[,..colList], colList)")))
}

################################################################################
# Avoid  'no visible binding for global variable' notes from check of package:
db_type <- NULL
CDISCctCodeLists <- CodeList <- NULL
CDISCctCodeValues <- CodelistCode <- NULL

