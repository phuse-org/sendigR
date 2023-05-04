################################################################################
## The functions to populate a SQLite database with SEND study data
## Inclusive a set of internal helper functions for load and deletion of study
## data in a database.
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2021-03-03   Bo Larsen             Initial version
################################################################################

# Check for a valid database type based on the engine found in the database token
checkDbType <- function(dbToken) {
  # Check if dbType is valid
  if (dbToken$dbType != 'sqlite' && dbToken$dbType != 'postgresql') {
    stop('Database must be either SQLite or PostgreSQL')
  }
}


#' Create a SEND schema in an open and empty database
#'
#' Create all the domains and variables which are described in the SEND IG
#' versions 3.0 and 3.1 in the database - i.e. a union of domains from the
#' SEND IG versions and in each domain a union of variables from the SEND IG
#' versions.
#'
#' The database must be an SQLite database - no other types of databases are
#' supported by this function.
#'
#' @param dbToken Mandatory\cr
#'   Token for the open database connection (see \code{\link{initEnvironment}}).
#'
#' @return No return value, called for side effects\cr
#' @export
#'
#' @examples
#' \dontrun{
#' # Create an empty SQLite database and create the SEND schema
#' myDbToken <- initEnvironment(dbType = 'sqlite',
#'                              dbPath ='/mydatapath/db/send.db',
#'                              dbCreate = TRUE)
#' dbCreateSchema(myDbToken)
#' }
dbCreateSchema <- function(dbToken) {
  checkDbType(dbToken)

  # Check if any tables already exist
  nTab <- genericQuery(dbToken,
                      "select count(0)  n
                         from sqlite_master
                        where type ='table'
                          and name not like 'sqlite_%'")$n
  if (nTab != 0)
    stop('One or more tables exist in the database - it must be empty to create a new SEND db schema.')

  # Create each domain with all variables described in the SEND IG metadata
  # included in the package
  # - SUPPQUAL is not included, it's only included in metadata to enable check
  #   of required columns in a potential input SUPPQUAL domain
  for (tab in sendIGtables[TABLE_NAME != 'SUPPQUAL']$TABLE_NAME) {
    # Generated column definition clause - columns sorted as defined in SEND IG
    cols <-
      paste(sapply(data.table::setorder(sendIGcolumns[TABLE_NAME == tab,
                                                      list(SEQ,
                                                           col_def = paste0("'",
                                                                            COLUMN_NAME,
                                                                            "' ",
                                                                            DATATYPE))],
                                        SEQ)[,SEQ := NULL],
                   paste0),
            collapse = ' ,')
    # print(cols)
    # Generate and execute create table stmt
    sqlStmt <- paste0("create table '", tab, "' (", cols, ")" )
    # print(sqlStmt)
    res <- RSQLite::dbSendStatement(dbToken$dbHandle, sqlStmt)
    RSQLite::dbClearResult(res)
  }
}

#' Import SEND study data in SAS xport format into a SEND database from a single
#' study folder
#'
#' Check each of the SAS xpt file located in the specified folder - import
#' content from file and load it into the corresponding SEND domain table in the
#' open database.
#'
#' These requirements to the content of the folder must be fulfilled:
#' \enumerate{
#'   \item The folder must contain some SAS xport files named
#'   \code{[send domain].xpt} - the case of the file names doesn't care
#'   \item A minimum set of required domain files must be included:
#'   \code{ts.xpt}, \code{tx.xpt}, \code{dm.xpt}.
#'   \item Each xpt file must contain one data table with same name as the file
#'   name - i.e. a send domain name.
#'   \item Each xpt file must contain a non-empty STUDYID value in each row equal
#'   to the value of TS.STUDYID.
#'   \item Each xpt file must contain a set of required column(s).\cr
#'   In general it's (where relevant for the different kinds of domains):\cr
#'    \code{STUDYID, DOMAIN, --SEQ, USUBJID, --TESTCD, --TEST,--ORRES, --ORRESU,
#'    --STRESC, --STRESN, --STRESU}
#'    \item The DOMAIN variable must contain the name of the actual domain in
#'    all rows
#' }
#'
#' The last two requirements are checked for the required domains in all cases.
#' For other domains, these two requirements are only checked if parameter
#' \code{checkRequiredVars = TRUE}.\cr
#'
#' If an error is detected, the import and load of data is canceled, and further
#' execution is aborted (i.e. error message is written to the console).\cr
#' These error situations are checked and reported:
#' \itemize{
#'   \item Any of the requirements 1 to 3 are not fulfilled or any of the
#'   following requirements are not fulfilled for one of the required domains
#'   \item A study with the same value if STUDYID exists in the database and
#'   parameter \code{overWrite = FALSE}.
#' }
#'
#' If one of the requirements 4 to 6 are not fulfilled for a not-required
#' domain, this domain is excluded from the import. These kinds of issues are
#' reported as one warning message to the console when data has been loaded.\cr
#'
#' Some non-critical issues, which doesn't prohibit data to be loaded to the
#' database may be detected. These are reported as one warning message to the
#' console when data has been loaded (together with eventual warning messages
#' for skipped domains).\cr
#' These non-critical issues are checked and reported:
#' \itemize{
#'   \item The study folder contains one or more xpt file(s) with names(s) not
#'   matching SEND domain name(s).\cr
#'   Such files are ignored by the import/load process.
#'   \item An imported data tables contains one or more column(s) which
#'   do(es)n't exist(s) in the corresponding domain.
#' }
#'
#' The database must be an SQLite database - no other types of databases are
#' supported by this function.
#'
#' @param dbToken Mandatory\cr
#'   Token for the open database connection (see \code{\link{initEnvironment}}).
#' @param xptPath Mandatory, character\cr
#'  Location of the SAS xport files
#' @param overWrite Mandatory, boolean\cr
#'  Whether an already existing study in the database may be overwritten by
#'  newly imported data.
#' @param checkRequiredVars Mandatory, boolean\cr
#'  Whether not-required domains are checked for existence and content of
#'  required variables
#' @return No return value, called for side effects\cr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Do not overwrite if study already exists in the database
#' dbImportOneStudy(myDbToken,'/mydatapath/studies/1213443')
#' # Allow to overwrite data if study already exists in the database
#' dbImportOneStudy(myDbToken,'/mydatapath/studies/786756', overwrite = TRUE)
#' }
dbImportOneStudy <- function(dbToken,
                             xptPath,
                             overWrite = FALSE,
                             checkRequiredVars = TRUE)
{
  if (dbToken$dbType != 'sqlite')
    stop("Function is only valid to execute for dbType = 'sqlite'")

  if (!file.exists(xptPath))
    stop(sprintf('Specified path %s cannot be found', xptPath))

  loadStudyData(dbToken, xptPath, overWrite, checkRequiredVars)
}


#' Import SEND study data in SAS xport format into a SEND database from a
#' hierarchy study folders.
#'
#' For each non-empty folder below the specified root folder, the actions to
#' import a set of SAS xpt files into the opened SQLlite database described
#' for function [dbImportOneStudy].
#'
#' The status for the processing of each sub folder is caught and returned as
#' described below.\cr
#' If parameter \code{verbose = TRUE}, the status for each processed sub folder
#' is also printed to the console each time a sub folder has been processed -
#' i.e. it's possible to followed the progress of the import process.
#' If parameter \code{logFilePath} has been specified with an existing path to a
#' folder, the status for each processed sub folder is also printed to a log
#' file in this folder each time a sub folder has been processed.
#'
#' The database must be an SQLite database - no other types of databases are
#' supported by this function.
#'
#' @param dbToken Mandatory\cr
#'   Token for the open database connection (see \code{\link{initEnvironment}}).
#' @param xptPathRoot Mandatory, character\cr
#'  Root location of a set of sub folders - each sub folder with a set of SAS xport
#'  files for one study to import.\cr
#'  The folder tree is traversed recursively - i.e. a multilevel folder
#'  hierarchy is allowed.
#' @param overWrite Mandatory, boolean\cr
#'   Whether an already existing study in the database may be overwritten by
#'   newly imported data.
#' @param checkRequiredVars Mandatory, boolean\cr
#'  Whether not-required domains are checked for existence and content of
#'  required variables
#' @param verbose Mandatory, boolean\cr
#'   Whether the status of the import shall be continuously written to the
#'   console for for each processed sub folder.
#' @param logFilePath Optional, character\cr
#'   A path to a folder to contain a log file with the status of the import for
#'   each processed sub folder.\cr
#'   The name of the log file is \code{logFilePath/dbImportStudies_<date &
#'   time>.log} where \code{<date & time>} is the actual date and time in format
#'   \code{YYYYmmdd_HH24MISS} - e.g. \code{dbImportStudies_20210323_084150.log}
#'   if the function was called 23. March 2021 at 8:41:50
#' @return A list containing a named element with the import status for each
#'   of the processed sub folders.\cr
#'   Each of the statuses are one of three variants:
#'   \itemize{
#'      \item 'OK' - the SAS xport files has been imported to the database with
#'      no errors or warnings
#'      \item 'Warning: \[list of warnings\]' - the SAS xport files has been
#'      imported to the database but have one or more warnings
#'      \item 'Cancelled: \[error message\]' - the SAS xport files have not been
#'      imported to the database because an error has been detected.
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Import studies from a set of folders - do not allow to overwrite
#' # existing study data in the database, follow the progress
#' dbImportStudies(myDbToken,'/mydatapath/studies', verbose = TRUE)
#' # Import studies from another set of folders - allow to overwrite existing
#' # study data in the database
#' dbImportStudies(myDbToken,'/mydatapath/project123/studies', overwrite = TRUE)
#' # Import studies from a set of folders , save the status of each study load
#' # in a log file
#' dbImportStudies(myDbToken,'/mydatapath/studies',
#'                 logFilePath = '/my/log file/path')

#' }
dbImportStudies <- function(dbToken,
                            xptPathRoot,
                            overWrite = FALSE,
                            checkRequiredVars = TRUE,
                            verbose = FALSE,
                            logFilePath = NULL)
{
  if (dbToken$dbType != 'sqlite')
    stop("Function is only valid to execute for dbType = 'sqlite'")

  if (!file.exists(xptPathRoot))
    stop(sprintf('Specified XPT path %s cannot be found', xptPathRoot))

  if (!is.null(logFilePath))
    if (!file.exists(logFilePath))
      stop(sprintf('Specified log file path %s cannot be found', logFilePath))
    else {
      logFileName <- paste0(logFilePath, '/', 'dbImportStudies_', format(Sys.time(), '%Y%m%d_%H%M%S'), '.log')
      logr::log_open(logFileName, logdir = FALSE, show_notes = FALSE)
    #   print(paste0('Writing status to log file: ', logFileName))
    }

  # initiate list to hold status for load of each study folder
  statusAll <- list()

  # Loop through each sub folder below specified folder
  for (pathName in list.dirs(xptPathRoot, full.names = FALSE)) {
    pathNameFull <- paste0(xptPathRoot,'/',pathName)
    # Exclude the root folder itself and sub folders with files included
    if (nchar(pathName) > 1 &
        length(list.files(pathNameFull, '\\..+')) != 0) {
      statusTxt <-
        tryCatch(
          {
            loadStudyData(dbToken, pathNameFull, overWrite, checkRequiredVars)
            'OK'
          }
          ,
          warning = function(warn) {
            paste0('OK with warning(s): ', warn$message)
          }
          ,
          error = function(err) {
            paste0('Cancelled: ', err$message)
          }
        )
      statusAll[[pathName]] <- statusTxt
      if (verbose)
        print(paste0(pathName, ': ', statusTxt))
      if (!is.null(logFilePath))
        logr::log_print(paste0(pathName, ': ', statusTxt),
                        console = FALSE,
                        blank_after = FALSE)
    }
  }
  if (!is.null(logFilePath))
    logr::log_close()
  statusAll
}

#' Delete one or more studies in SEND database
#'
#' Deletes data from all domains for one or more studies in an SQLite based SEND
#' database
#'
#' The database must be a SQLite database - no other types of databases are
#' supported by this function.

#' @param dbToken Mandatory\cr
#'   Token for the open database connection (see \code{\link{initEnvironment}}).
#' @param studyIdList Mandatory, character\cr
#'  A list or vector of study id values
#'
#' @return No return value, called for side effects\cr
#' @export
#'
#' @examples
#' \dontrun{
#' # delete one study
#' dbDeleteStudies(myDbToken, '122312')
#' # delete multiple studies
#' dbDeleteStudies(myDbToken, list('122312', '552343', '0942347'))
#' }
dbDeleteStudies <- function(dbToken,
                            studyIdList)
{
  if (dbToken$dbType != 'sqlite')
    stop("Function is only valid to execute for dbType = 'sqlite'")

  for (studyId in studyIdList) {
    deleteStudyData(dbToken, studyId)
  }
}


#' Create indexes in SEND database
#'
#' Create a set of indexes on the tables in an SQLite SEND database to
#' optimize performance of extraction of data from the different functions in
#' the package.
#'
#' All the indexes are named \code{<domain name>_sendigr_<nn>} - .e.g.
#' \code{TS_sendigr_01}.\cr
#' If any additional indexes are manually created in the database, avoid to
#' include 'sendigr' in the name, because all existing indexes with that
#' included in the name will be initially deleted when execution the function
#' with \code{replaceExisting = TRUE}.\cr
#' It's recommended to wait with the creation of the indexes until the major
#' amount of studies to be loaded in to the database are loaded.
#'
#' The database must be an SQLite database - no other types of databases are
#' supported by this function.

#' @param dbToken  Mandatory\cr
#'   Token for the open database connection (see \code{\link{initEnvironment}}).
#' @param replaceExisting Mandatory, character\cr
#'   Whether an already existing set of indexes in the database may be replaced
#'   by a new set of indexes.
#' @return No return value, called for side effects\cr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' createAllIndexes(myDbToken)
#' }
#'
dbCreateIndexes <- function(dbToken, replaceExisting = FALSE) {

  # Create one index
  creIdx <- function(tab, idxName, colListStr) {
    RSQLite::dbClearResult(RSQLite::dbSendStatement(dbToken$dbHandle,
                                                    sprintf("create index %s_sendigr_%s on %s (%s)",
                                                            tab, idxName, tab, colListStr)))
  }

  if (dbToken$dbType != 'sqlite')
    stop("Function is only valid to execute for dbType = 'sqlite'")

  ## Check if any sendigr indexes exist - and delete if appropriate
  idxList <-
    genericQuery(dbToken,
                 "select name from sqlite_master
                   where type = 'index'
                     and name like '%sendigr%'")$name
  if (length(idxList) != 0)
    if (replaceExisting) {
      for (idxName in idxList)
        RSQLite::dbClearResult(RSQLite::dbSendStatement(dbToken$dbHandle,
                                                        sprintf("drop index %s",
                                                                idxName)))
    } else {
      stop('There are already existing indexes, execute with replaceExisting=TRUE to replace with new set of indexes')
    }

  ## Generate indexes for specific optimization of the data extraction functions

  # TS
  creIdx('ts','01', 'studyid, tsparmcd, tsval')
  creIdx('ts','02', 'studyid, tsparmcd, tsgrpid, tsval')

  # TX
  creIdx('tx','01', 'studyid, txparmcd, setcd, txval')

  # DM
  creIdx('dm', '01', 'studyid, setcd')
  creIdx('dm', '02', 'studyid, usubjid')
  creIdx('dm', '03', 'studyid, setcd, sex, usubjid')

  # POOLDEF
  creIdx('pooldef', '01', 'studyid, poolid, usubjid')

  # EX
  creIdx('ex', '01', 'studyid, exroute, usubjid')
  creIdx('ex', '02', 'studyid, exroute, poolid')

  ## Generate general indexes for the remaining tables on STUDYID and
  ## (if included) USUBJID

  exclTabList = c('TS','TX','DM','POOLDEF','EX')

  for (tab in setdiff(getDbTables(dbToken), exclTabList)) {
    if ('USUBJID' %in% dbListFields(dbToken, tab))
      creIdx(tab, '01', 'studyid, usubjid')
    else
      creIdx(tab, '01', 'studyid')
  }
}



##############################################################################
# Helper functions used internally for load of study data
##############################################################################

##############################################################################
# Extract and return list of tables in the database
getDbTables <- function(dbToken) {
  if (dbToken$dbType == 'sqlite') {
    genericQuery(dbToken,
                 "select name
                        from sqlite_master
                        where type ='table'
                        and name not like 'sqlite_%'")$name;
  } else if (dbToken$dbType == 'postgresql') {
    genericQuery(dbToken,
                 "SELECT table_name 
                 FROM information_schema.tables 
                 WHERE table_schema='public'")$name;
  }
}

##############################################################################
# Delete rows for specified study in all tables in the database
deleteStudyData <- function(dbToken, studyId) {
  if (dbToken$dbType == 'sqlite') {
    for (tab in getDbTables(dbToken)) {
      res <-
        RSQLite::dbSendStatement(dbToken$dbHandle,
                                 sprintf('delete from %s where studyid = ?',tab),
                                 studyId)
      RSQLite::dbClearResult(res)
    }
  } else if (dbToken$dbType == 'postgresql') {
    for (tab in getDbTables(dbToken)) {
      res <-
        RPostgres::dbSendStatement(dbToken$dbHandle,
                                 sprintf('delete from %s where studyid = ?',tab),
                                 studyId)
      RPostgres::dbClearResult(res)
    }
  }
}

##############################################################################
# Validate xpt files in specified path
# - if any errors are identified, the study data is not loaded and an error
#   message is returned
# - if no errors are identified, the study data is loaded
#     - if any warnings are identified, these are returned in a message
#     - else an empty message is returned
loadStudyData <- function(dbToken,
                            # Path to files:
                          xptPath,
                            # Whether already exiting data for actual study shall
                            # be replaced or not:
                          overWrite = FALSE,
                            # Whether domain is only imported if required columns
                            # are included
                          checkRequiredVars = TRUE)
{

  ##############################################################################
  # Import domain from xpt file - return content in a data table
  importXptFile <- function(file, domain) {
    # Extract content of xpt file using haven
    xptContent <- haven::read_xpt(file.path(xptPath, file))
    # ...and check if it's valid
    # if (length(names(xptContent)) != 1)
    #   stop(paste0('Too many tables included in xpt file: ', file))
    # if (toupper(names(xptContent)) != domain)
    #   stop(sprintf('The in xpt file %s contains an unexpected table name %s - should have been %s',
    #                file, names(xptContent), domain))

    # Convert to data.table and return
     data.table::as.data.table(sjlabelled::remove_all_labels(xptContent))
  }
  ### End of importXptFile

  ##############################################################################
  # Import SUPPQUAL file and write subsets of data to domain specific
  # SUPP-- tables
  loadSuppData <- function(suppqual) {
    rdomainsInvalid <- c()
    for (rdomain in unique(suppqual$RDOMAIN)) {
      if (dbExistsTable(dbToken, toupper(rdomain ))) {
        rdomainsInvalid <- c(rdomainsInvalid, rdomain)
        next
      }

      RSQLite::dbWriteTable(dbToken$dbHandle,
                            name = paste0('SUPP',rdomain),
                            value = suppqual[RDOMAIN == rdomain],
                            append = TRUE)
    }

    if (length(rdomainsInvalid) != 0)
      paste0('Domain SUPPQUAL contains RDOMAIN references to not-existing domains: ',
                    paste(rdomainsInvalid, collapse = ','))
    else
      c()
  }
  ### End of loadSuppData

  ##############################################################################
  # If imported domain data  fulfills minimum requirements, insert data into
  # the database
  loadDomainData <- function(dtDomain, domain, checkRequiredVars) {
    warnTxt <- c()
    errMsg <- ''
    requiredCols <- sendIGcolumns[TABLE_NAME == domain & REQUIRED == 'Y']$COLUMN_NAME
    requiredTab <- (nrow(sendIGtables[TABLE_NAME == domain & REQUIRED == 'Y']) == 1)

    # Do checks for other domains than TS (these are already done for TS)
    if (domain != 'TS') {
      if (nrow(dtDomain) == 0)
        errMsg <- sprintf('Domain %s is empty', domain)
      else  # Check existence of studyid var
        if (! 'STUDYID' %in% names(dtDomain))
          errMsg <- sprintf('Domain %s misses a STUDYID variable',
                            domain)
        else {
          #  Check STUDYID column
          studyIdDomain <- as.character(unique(dtDomain$STUDYID))
          if ('' %in% studyIdDomain | NA %in% studyIdDomain) {
            errMsg <- sprintf('Domain %s misses a study ID value in one or more rows',
                              domain)
          }
          else if (length(studyIdDomain) != 1) {
            errMsg <- sprintf('Domain %s contains more than one distinct STUDYID value',
                              domain)
          } else if (studyIdDomain != studyId) {
            errMsg <- sprintf('Domain %s contains another STUDYID value than TS',
                              domain)
        }
      }
    }

    if (errMsg == '' &
        (checkRequiredVars | requiredTab ) ) {
      # Check for required column condtions
      #  - always for required domains and option for other domains

      # Check for existence of required columns
      missCols <- setdiff(requiredCols, names(dtDomain))
      if (length(missCols) != 0)
        errMsg <- sprintf('Domain %s misses variable(s): %s',
                          domain, missCols)
      else
        # Check for correct value of col DOMAIN
        if ('DOMAIN' %in% names(dtDomain)) {
          domainvalue <- unique(dtDomain$DOMAIN)
          if (length(domainvalue) != 1 | domainvalue != domain )
            errMsg <- sprintf('Domain %s contains a DOMAIN value different from domain name',
                              domain)
        }
    }

    if (errMsg != '') {
      # An error has been identified
      # - if it's in a required domain, report as an error
      # - else report as a warning
      if (requiredTab)
        stop(errMsg)
      else
        return(paste0(errMsg, ' (skipped)'))
    }

    # Check if imported table contains columns not in the database table
    extraCols <- setdiff(names(dtDomain), dbListFields(dbToken, domain))
    if (length(extraCols) != 0) {
      # delete additional columns from imported data
      data.table::set(dtDomain, j = extraCols, value = NULL)
      warnTxt <- c(warnTxt,
                   sprintf('Additional columns in domain %s has been ignored: %s',
                           domain, paste(extraCols, collapse=',')))
    }

    if (domain == 'SUPPQUAL')
      warnTxt <- c(warnTxt, loadSuppData(dtDomain))
    else
      RSQLite::dbWriteTable(dbToken$dbHandle,
                            name = domain,
                            value = dtDomain,
                            append = TRUE)
    warnTxt
  }
  ### End of loadDomainData

  ##############################################################################

  # Get list of xpt files - if any
  filesAll <-
    list.files(path = xptPath,
               pattern = paste0('.*\\.xpt$'),
               ignore.case = FALSE,
               full.names = FALSE)
  if (length(filesAll) == 0)
    stop('No xpt files found')

  filesSEND <- tolower(paste0(sendIGtables$TABLE_NAME, '.xpt'))
  filesRequired <- tolower(paste0(sendIGtables[REQUIRED == 'Y']$TABLE_NAME, '.xpt'))
  filesMiss <- setdiff(filesRequired,
                       filesRequired[filesRequired %in% tolower(filesAll)])
  if (length(filesMiss) != 0)
    stop(paste0('Missing xpt file(s): ', paste(filesMiss, collapse = ',')))

  ## Check TS domain

  # Get the TS xpt file name in correct case and import
  fileTS <- stringr::str_match(filesAll, stringr::regex('ts.xpt', ignore_case = TRUE))
    dtTS <- importXptFile(fileTS[!is.na(fileTS)], 'TS')

  # Check it's not empty
  if (nrow(dtTS) == 0)
    stop('TS domain is empty')

  # Check existence of studyid var
  if (! 'STUDYID' %in% names(dtTS))
    stop('TS domain misses a STUDYID variable')

  # Get studyid and check if it's unique
  studyId <- as.character(unique(dtTS$STUDYID))
  if ('' %in% studyId | NA %in% studyId)
    stop('TS domain misses a STUDYID value in one or more rows')
  else if (length(studyId) != 1)
    stop('TS domain contains more than one distinct STUDYID value')

  # Check if study already exists in the database
  studyExists <- (genericQuery(dbToken, 'select count(1) as n from "TS" where "STUDYID" = ?', studyId)$n != 0)
  if (studyExists & !overWrite)
    stop('The study exists in the database, but it is specified not to overwrite existing studies')

  ## Import and load all domains in a transaction

  # Do a rollback to ensure we are not unexpected in an open transaction
  #  - ignore error message if no transaction is open
  tryCatch(
    { RSQLite::dbRollback(dbToken$dbHandle) }
    , error = function(errMsg) { } )
  # Open new transaction
  if (dbToken$dbType == 'sqlite') {
    RSQLite::dbBegin(dbToken$dbHandle)
  } else if (dbToken$dbType == 'postgresql') {
    RPostgres::dbBegin(dbToken$dbHandle)
  }
  
  tryCatch(
    {
      if (studyExists)
        deleteStudyData(dbToken, studyId)

      # Loop through all xpt files
      filesNotSEND <- c()
      loadWarnings <- c()
      for (file in filesAll) {
        if (! tolower(file) %in% filesSEND
            & tolower(file) != 'suppqual') {
          # Not a SEND domain file - add to list of non-compliant files
          filesNotSEND <- c(filesNotSEND, file)
          next
        }
        # Import data from xpt file and load into the database
        domain <- toupper(strsplit(file, '\\.')[[1]][1])
        if (domain == 'TS')
          # TS has already been imported
          dt <- dtTS
        else
          dt <- importXptFile(file, domain)
        loadWarnings <- c(loadWarnings,
                          loadDomainData(dt, domain, checkRequiredVars))
      }
    }
    ,
    error = function(errMsg) {
      # Error detected - rollback database changes an exit
      if (dbToken$dbType == 'sqlite') {
        RSQLite::dbRollback(dbToken$dbHandle)
      } else if (dbToken$dbType == 'postgresql') {
        RPostgres::dbRollback(dbToken$dbHandle)
      }
      stop(errMsg)
    }
  )
  
  if (dbToken$dbType == 'sqlite') {
    RSQLite::dbCommit(dbToken$dbHandle)
  } else if (dbToken$dbType == 'postgresql') {
    RPostgres::dbCommit(dbToken$dbHandle)
  }

  # Check if any warnings are to be reported
  warningMessage <-
    paste0(ifelse(!is.null(filesNotSEND),
                  paste0('Non-SEND XPT file(s) ignored: ',paste(filesNotSEND,collapse = ',')),
                  ''),
           ifelse(!is.null(filesNotSEND) & !is.null(loadWarnings),
                  '; ',
                  ''),
           ifelse(!is.null(loadWarnings),
                  paste(loadWarnings, collapse = '; '),
                  ''))
  if (nchar(warningMessage) != 0)
    warning(warningMessage)
}

################################################################################
# Avoid  'no visible binding for global variable' notes from check of package:
DATATYPE <- RDOMAIN <- REQUIRED <- NULL
