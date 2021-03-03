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

#' Create a SEND schema in an open and empty database
#'
#' Create all the domains and variables which are described in the SEND IG
#' versions 3.0 and 3.1 in the database - i.e. a union of domains from the
#' SEND IG versions and in each domain a union of variables from the the
#' SEND IG versions.
#'
#' The database must be a SQLite database
#'
#' @param dbToken Mandatory\cr
#'  Token for the open database connection
#'
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
  # Check if dbType is valid
  if (dbToken$dbType != 'sqlite')
    stop('Function is only supported for SQLite databases')

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
    # Generate and execute create table stmt
    sqlStmt <- paste0("create table '", tab, "' (", cols, ")" )
    res <- RSQLite::dbSendStatement(dbToken$dbHandle, sqlStmt)
    RSQLite::dbClearResult(res)
  }
}

#' Import SEND study data in SAS xport format into a SEND database from a single
#' study folder
#'
#' Check each of the SAS xpt file located in the specified folder.\cr
#' If relevant, import content from file and load it into the corresponding SEND
#' domain table in the open database.
#'
#' These requirements to the content of the folder must be fulfilled:
#' \itemize{
#'   \item The folder must contain some SAS xport files named
#'   \code{[send domain].xpt} - the case of the file names doesn't care
#'   \item A set of required domain files must be included: \code{ts.xpt},
#'   \code{tx.xpt}, \code{dm.xpt}.
#'   \item Each xpt file must contain one data table with same name as the file
#'   name - i.e. a send domain name.
#'   \item Each xpt file must contain a set of required column(s).\cr
#'   In general it's (where relevant for the different kinds of domains):\cr
#'    \code{STUDYID, DOMAIN, --SEQ, USUBJID, --TESTCD, --TEST,--ORRES, --ORRESU,
#'    --STRESC, --STRESN, --STRESU}
#'   \item Each xpt file must contain a non-empty STUDYID value equal to the
#'   value of TS.STUDYID.
#' }
#'
#' If an error is detected, the import and load of data is canceled, and further
#' execution is aborted (i.e. error message is written to the console).\cr
#' These error situations are checked and reported:
#' \itemize{
#'   \item The requirements described above are not fulfilled.
#'   \item A study with the same value if STUDYID exists in the database and
#'   parameter \code{overWrite = FALSE}.
#' }
#'
#' Some non-critical issues, which doesn't prohibit data to be loaded to the
#' database may be detected. These are reported as one warning message to the
#' console when data has been loaded.\cr
#' These non-critical issues are checked and reported:
#' \itemize{
#'   \item The study folder contains one or more xpt file(s) with names(s) not
#'   matching SEND domain name(s).\cr
#'   Such files are ignored by the import/load process.
#'   \item An imported data tables contains one or more column(s) which
#'   do(es)n't exist(s) in the corresponding domain.
#' }
#'
#' The function must be used against an SQLite based SEND database.
#'
#' @param dbToken Mandatory\cr
#'  Token for the open database connection
#' @param xptPath Mandatory\cr
#'  Location of the SAS xport files
#' @param overWrite Optional\cr
#'   Whether an already existing study in the database may be overwritten by
#'   newly imported data.
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
                             overWrite = FALSE)
{
  if (dbToken$dbType != 'sqlite')
    stop("Function is only valid to execute for dbType = 'sqlite'")

  if (!file.exists(xptPath))
    stop(sprintf('Specified path %s cannot be found', xptPath))

  loadStudyData(dbToken, xptPath, overWrite)
}


#' Import SEND study data in SAS xport format into a SEND database from a
#' hierarchy study folders.
#'
#' For each non-empty folder below the specified root folder, the actions to
#' import a set of SAS xpt files into the opened SQLlite database described
#' for function [dbImportOneStudy].
#' The status for the processing of each sub folder is catched an returned a
#' described below.\cr
#' If parameter \code{verbose = TRUE}, the status for each processed sub folder
#' is also printed to the console each time a sub folder has been processed -
#' i.e. it's possible to followed the progress of the import process.
#'
#' The function must be used against an SQLite based SEND database.
#'
#' @param dbToken Mandatory\cr
#'  Token for the open database connection
#' @param xptPathRoot Mandatory\cr
#'  Root location of a set of sub folders - each sub folder with a if SAS xport
#'  files per study to import.\cr
#'  The folder tree is traversed recursively - i.e. a multilevel folder
#'  hierarchy is allowed.
#' @param overWrite Optional\cr
#'   Whether an already existing study in the database may be overwritten by
#'   newly imported data.
#' @param verbose Optional\cr
#'   Whether the status of the import shall be continuously written to the
#'   console for for each processed sub folder.
#' @return A list containing a named element with the the import status for each
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
#' }
dbImportStudies <- function(dbToken,
                            xptPathRoot,
                            overWrite = FALSE,
                            verbose = FALSE)
{
  if (dbToken$dbType != 'sqlite')
    stop("Function is only valid to execute for dbType = 'sqlite'")

  if (!file.exists(xptPathRoot))
    stop(sprintf('Specified path %s cannot be found', xptPathRoot))

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
            loadStudyData(dbToken, pathNameFull, overWrite)
            statusTxt <- 'OK'
          }
          ,
          warning = function(warn) {
            paste0('Warning: ', warn$message)
          }
          ,
          error = function(err) {
            paste0('Cancelled: ', err$message)
          }
        )
      statusAll[[pathName]] <- statusTxt
      if (verbose)
        print(paste0(pathName, ': ', statusTxt))
    }
  }
  return(statusAll)
}

#' Delete one or more studies in SEND database
#'
#' Deletes data from all domains for one or more studies in a SQLite based SEND
#' database
#'
#' @param dbToken Mandatory\cr
#'  Token for the open database connection
#' @param studyIdList Mandatory\cr
#'  A list or vector of study id values
#'
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


##############################################################################
# Helper functions used internally for load of study data
##############################################################################

##############################################################################
# Extract and return list of tables in the database
getDbTables <- function(dbToken) {
  return(genericQuery(dbToken,
                     "select name
                        from sqlite_master
                       where type ='table'
                         and name not like 'sqlite_%'")$name);
}

##############################################################################
# Delete rows for specified study in all tables in the database
deleteStudyData <- function(dbToken, studyId) {
  for (tab in getDbTables(dbToken)) {
    res <-
      RSQLite::dbSendStatement(dbToken$dbHandle,
                               sprintf('delete from %s where studyid = :1',tab),
                               studyId)
    RSQLite::dbClearResult(res)
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
                          overWrite = FALSE)
{

  ##############################################################################
  # Import domain from xpt file - return content in a data table
  importXptFile <- function(file, domain) {
    # Extract content of xpt file
    xptContent <- SASxport::read.xport(file.path(xptPath, file),
                                       as.is = TRUE,
                                       as.list = TRUE)
    # ...and check if it's valid
    if (length(names(xptContent)) != 1)
      stop(paste0('Too many tables included in xpt file: ', file))
    if (toupper(names(xptContent)) != domain)
      stop(sprintf('The in xpt file %s contains an unexpected table name %s - should have been %s',
                   file, names(xptContent), domain))

    # Convert to data.table and return
    return(
      data.table::as.data.table(sjlabelled::remove_all_labels(xptContent[[names(xptContent)]])))
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
      return(paste0('Domain SUPPQUAL contains RDOMAIN references to not-existing domains: ',
                    paste(rdomainsInvalid, collapse = ',')))
    else
      return(c())
  }
  ### End of loadSuppData

  ##############################################################################
  # If imported domain data  fulfills minimum requirements, insert data into
  # the database
  loadDomainData <- function(dtDomain, domain) {
    warnTxt <- c()
    requiredCols <- sendIGcolumns[TABLE_NAME == domain & REQUIRED == 'Y']$COLUMN_NAME

    # Check for existence of required columns
    missCols <- setdiff(requiredCols, names(dtDomain))
    if (length(missCols) != 0)
      stop(sprintf('Imported domain %s misses variable(s): %s', domain, missCols))

    # Check STUDYID column (for other domains than TS)
    if (domain != 'TS') {
      studyIdDomain <- unique(dtDomain$STUDYID)
      if (typeof(studyIdDomain) != 'character' | studyIdDomain == '')
        stop(sprintf('Imported domain %s misses a study ID value', domain))
      else if (length(studyIdDomain) != 1)
        stop(sprintf('Imported domain %s contains more than one STUDYID value', domain))
      if (studyIdDomain != studyId)
        stop(sprintf('Imported domain %s contains another STUDYID value than TS', domain))
    }

    # # Check if all required columns are not empty
    # emptyCols <- c()
    # for (col in setdiff(requiredCols, 'STUDYID')) {
    #   if (nrow(dtDomain[!eval(quote(isTRUE(nchar(as.character(col)) > 0))),..col]) == 0)
    #     emptyCols <- c(emptyCols, col)
    # }
    # if (!is.null(emptyCols))
    #   stop(sprintf('Imported domain %s contains rows with values in required column(s): %s', domain, emptyCols))

    # Check for correct value of col DOMAIN
    if ('DOMAIN' %in% names(dtDomain)) {
      domainvalue <- unique(dtDomain$DOMAIN)
      if (length(domainvalue) != 1 | domainvalue != domain )
        stop(sprintf('Imported domain %s contains a DOMAIN value different from domain name', domain))
    }

    # Check if imported table contains columns not in the database table
    extraCols <- setdiff(names(dtDomain), dbListFields(dbToken, domain))
    if (length(extraCols) != 0) {
      # delete additional columns from imported data
      data.table::set(dtDomain,, extraCols, NULL)
      warnTxt <- c(warnTxt,
                   sprintf('Additional columns in imported domain %s has been ignored: %s',
                           domain, paste(extraCols, collapse=',')))
    }

    if (domain == 'SUPPQUAL')
      warnTxt <- c(warnTxt, loadSuppData(dtDomain))
    else
      RSQLite::dbWriteTable(dbToken$dbHandle,
                            name = domain,
                            value = dtDomain,
                            append = TRUE)
    return(warnTxt)
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

  # Get studyid and check if it's unique
  studyId <- unique(dtTS$STUDYID)
  if (typeof(studyId) != 'character' | studyId == '')
    stop('TS domain misses a STUDYID value')
  else if (length(studyId) != 1)
    stop('TS domain contains more than one STUDYID value')

  # Check if study already exists in the database
  studyExists <- (genericQuery(dbToken, 'select count(1) as n from ts where studyid = :1', studyId)$n != 0)
  if (studyExists & !overWrite)
    stop('The study exists in the database, but it is specified not to overwrite existing studies')

  ## Import and load all domains in a transaction

  RSQLite::dbBegin(dbToken$dbHandle)
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
        dt <- importXptFile(file, domain)
        loadWarnings <- c(loadWarnings,
                          loadDomainData(dt, domain))
      }
    }
    ,
    error = function(errMsg) {
      # Error detected - rollback database changes an exit
      RSQLite::dbRollback(dbToken$dbHandle)
      stop(errMsg)
    }
  )
  RSQLite::dbCommit(dbToken$dbHandle)

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
