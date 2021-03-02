################################################################################
## The functions
##    ...
##
##
## A set of internal helper functions for load of study data into a database
## are included too
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2021-02-18   Bo Larsen             Initial version
################################################################################



#' Create a SEND schema in an open and empty database
#'
#' Create all the tables and variables which are described in the SEND ID
#' versions 3.0 and 3.1 in the database.
#'
#' @param dbToken Mandatory - token for the open database connection
#'
#' @export
#'
#' @examples
#' \dontrun{
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
  for (tab in sendIGtables$TABLE_NAME) {
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


dbDeleteOneStudy <- function(dbToken,
                             studyId)
{
  if (dbToken$dbType != 'sqlite')
    stop("Function is only valid to execute for dbType = 'sqlite'")

  deleteStudyData(dbToken,studyId)
}


dbDeleteStudiesStudy <- function(dbToken,
                                 studyIdList)
{
  if (dbToken$dbType != 'sqlite')
    stop("Function is only valid to execute for dbType = 'sqlite'")
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
    if (length(warnTxt) != 0)
      warning(paste(warnTxt, collapse = '; '))
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
        loadDomainData(dt, domain)
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

  if (!is.null(filesNotSEND))
    warning(paste0('Non-SEND XPT file(s) ignored: ',paste(filesNotSEND,collapse = ',')))
}

################################################################################
# Avoid  'no visible binding for global variable' notes from check of package:
DATATYPE <- NULL
