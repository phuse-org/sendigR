###################################################################################
# Script name   : initSENDFunctions.R
# Date Created  : 23-Jul-2020
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Initiate the set of SEND functions 
#
# Description   : - Defines an initiation function to be called for setup the 
#                   database environment and import of CDISC CT
#                 - Multiple types of databases are supported - for each of the
#                    supported db types:
#                     - A metadata record is included describing the relevant
#                       properties for a type of database
#                     - A function to connect to the specific database
#                     - A function to import data for a SEND domain from the 
#                       specific database
#                 - Compiles functions in all modules
#                   
###################################################################################

library(tools)
library(data.table)
library(readxl)

###################################################################################
# Function name : initEnvironment
#
# Purpose       : Initialize the environment 
#
# Description   : Defines relevant global variables for
#                   - type of SEND database
#                   - pointer to database (filename, connect string....)
#                   - username/password to database (if relevant)
#                 Import CDISC CT data from specified CT file in Excel format
#                 
# Input         :  n/a
#
# Output        : Global variables:
#                   - GdbHandle - handle to opened db connection
#                   - GdbSchenma - schema name for SEND db tables
#                 Global function pointing at the db type specific function to 
#                 import a domain from db:
#                   - doImportDomain
#                 Global data tables with imported CDISC CT code lists and values
#                   - CDISCctCodeLists
#                   - CDISCctCodeValues
#
# Parameters    : - dbType
#                   Mandatory - type of database
#                 - dbPath
#                   Mandatory - the pointre to the database (path to file or db name)
#                 - dbUser
#                 - dbPwd
#                   Mandatory if login credendtiales are required for the specific 
#                   db type - The user name and password respectively
#                 - dbSchema
#                   Optional - The table owner of the SEND table in the specific 
#                   database
#                 - ctFile
#                   Mandatory - name (full path) of CDISC CT file in Excel xls 
#                   format to be imported
#
###################################################################################
initEnvironment<-function(dbType=NULL, dbPath=NULL,  dbUser=NULL, dbPwd=NULL, dbSchema=NULL, ctFile=NULL) {
  
  ## Import specified CT file
  importCtFile<-function() {
    # Check file is XLS and exists
    if (! tolower(file_ext(ctFile)) == 'xls')
      stop(paste0('The ctFile ' , ctFile, ' is not an XLS file'))
    if (!file.exists(ctFile)) {
      stop(paste0('The ctFile ' , ctFile, 'could not be found'))
    }
    
    # Import content from worksheet named SEND<sep>Terminology<something> - include relevant columns and all rows
    ctSheets<-excel_sheets(ctFile)
    ctAll<-as.data.table(read_xls(ctFile, sheet=ctSheets[grepl('send[_ ]terminology', 
                                                               tolower(ctSheets) )]))[,c("Code", "Codelist Code", "CDISC Submission Value")]
    setnames(ctAll, c("Codelist Code","CDISC Submission Value"),c("CodelistCode","CDISCSubmissionValue"))
    
    # Extract all CDISC CT code list names
    CDISCctCodeLists<<-ctAll[is.na(CodelistCode), .(CodelistCode=Code, CodeList=CDISCSubmissionValue)]
    
    # Extract all CDISC CT code list values
    CDISCctCodeValues<<-ctAll[!is.na(CodelistCode), .(CodelistCode,CDISCSubmissionValue)]
  }
  
  ## Verify database specification parameters
  
  # dbType
  errMsg<-paste0('Parameter dbType must be one of: ', paste(sapply(GvalidDbTypes[,.(db_type)], paste0), collapse=','))
  if (is.null(dbType) | isTRUE(is.na(dbType)) | isTRUE(dbType==''))
    stop(errMsg)
  else if (nrow(dbProperties<-GvalidDbTypes[db_type==tolower(dbType)]) == 0)
    stop(errMsg)
  GdbType<<-tolower(dbType)
  
  errMsg<-'Parameter %s must be a non-empty string'
  # dbPath
  if (is.null(dbPath) | isTRUE(is.na(dbPath)) | isTRUE(dbPath==''))
    stop(sprintf(errMsg, 'dbPath')) 
  
  # dbUSer and dbPwd
  if (as.logical(dbProperties[,.(req_credentials)])) {
    if (is.null(dbUser) | isTRUE(is.na(dbUser)) | isTRUE(dbUser==''))
      stop(sprintf(errMsg, 'dbUser')) 
    if (is.null(dbPwd) | isTRUE(is.na(dbPwd)) | isTRUE(dbPwd==''))
      stop(sprintf(errMsg,'dbPwd')) 
  }
  
  # dbSchema
  if (!is.null(dbSchema) & isFALSE(is.na(dbSchema)) & isFALSE(dbSchema==''))
    GdbSchema<<-dbSchema
  else
    GdbSchema<<-""
  
  ## Verifying specified CT file - and import
  if (is.null(ctFile) | isTRUE(is.na(ctFile)) | isTRUE(ctFile==''))
    stop(sprintf(errMsg,'ctFile')) 
  else 
    importCtFile()
  
  ## Import package for the specified db type
  library(as.character(dbProperties[,.(package_name)]), character.only=TRUE)
  
  ## Connect to the database 
  ## - execute the function specific for the db type to create the connections
  if (as.logical(dbProperties[,.(req_credentials)]))
    dbInputParams<-paste0('(dbPath, dbUser, dbPwd)')
  else
    dbInputParams<-paste0('(dbPath)')
  GdbHandle<<-eval(parse(text=paste0('connectDB_',dbProperties[,.(db_type)],dbInputParams)))
  
  
  ## Verify existence of function specific for db type to execute a generic query
  if (!exists(paste0('genericQuery_', dbProperties[,.(db_type)])))
    stop(sprintf('A function named %s is missing', disconnectDBName))
  
  ## Assign function specific  for db type to disconnect from db
  disconnectDBName<-paste0('disconnectDB_', dbProperties[,.(db_type)])
  if (exists(disconnectDBName))
    disconnectDB<<-get(disconnectDBName)
  else
    stop(sprintf('A function named %s is missing', doImportDomainName))
  
  ## Assign function specific  for db type to import a SEND domain from db
  doImportDomainName<-paste0('doImportDomain_', dbProperties[,.(db_type)])
  if (exists(doImportDomainName))
    doImportDomain<<-get(doImportDomainName)
  else
    stop(sprintf('A function named %s is missing', doImportDomainName))
}

##############################################################################################
## Valid db types
GvalidDbTypes<-
    data.table(db_type         = c('sqlite',  'oracle'),
               req_credentials = c( FALSE,     TRUE), 
               package_name    = c('RSQLite', 'ROracle'))
# data.table(db_type         = c('sqlite',  'oracle', 'odbc_login', 'odbc_nologin'),
#            req_credentials = c( FALSE,     TRUE,     TRUE,         FALSE), 
#            package_name    = c('RSQLite', 'ROracle','RODBCDBI',   'RODBCDBI'))

##############################################################################################
## Connect function specific for each db type
##############################################################################################

# SQLite
connectDB_sqlite<-function(dbPath) {
  return(RSQLite::dbConnect(RSQLite::SQLite(), dbPath))
}

# Oracle
connectDB_oracle<-function(dbPath, dbUser, dbPwd) {
  return(ROracle::dbConnect(dbDriver("Oracle"), username=dbUser, password=dbPwd, dbname=dbPath))
}

# ODBC with login credentials
# connectDB_odbc_login<-function(dbPath, dbUser, dbPwd) {
#   return(dbConnect(RODBCDBI::ODBC(), dsn=dbPath, user=dbUser, password=dbPwd))
# }

# ODBC without login credentials
# connectDB_odbc_nologin<-function(dbPath) {
#   return(dbConnect(RODBCDBI::ODBC(), dsn=dbPath))
# }

##############################################################################################
## Disconnect function specific for each db type
##############################################################################################

# SQLite
disconnectDB_sqlite<-function() {
  return(RSQLite::dbDisconnect(GdbHandle))
}

# Oracle
disconnectDB_oracle<-function() {
  return(ROracle::dbDisconnect(GdbHandle))
}



##############################################################################################
## Functions to execute a generic query specific for each db 
## Result data set always returned as data table
## NOTE: Does not currently support 'in' functionality in the where clause !
##############################################################################################

## Helper function 
# Take a SQL statement as input 
# - if a schema name has been defined, 
#   return the statement modified in this way:
#   - substitute lineshifts (\n) with space
#   - substitute multiple consecutive spaces with one space
#   - add the schema plus '.' in front of all table names, 
#     i.e. names following a 'from ' or 'join '
# - else return the statement unchanged
selectStmtAddSchema <- function(stmt) {
  if (GdbSchema != '')
    return(str_replace_all(str_replace_all(str_replace_all(stmt, 
                                                           '\n', ' '), 
                                           ' +', ' '), 
                           regex('(from |join )', ignore_case = TRUE), 
                           paste0('\\1', GdbSchema, '.')))
  else 
    return(stmt)
}

##
# The function to be called by programs
#  It prepares the given select statement by adding potential schema name 
#  to all tables and execute the genericQuery funtcion specific for the 
#  actual db type
genericQuery <- function(query_string, query_params=NULL) {
  get(paste0('genericQuery_', GdbType))(selectStmtAddSchema(query_string),
                                                           query_params)
}

## DB specific functions:
# SQLite
genericQuery_sqlite<-function(query_string, query_params) {
  #print(query_string)
  
  if (is.null(query_params)){
    query_result <- setDT(RSQLite::dbGetQuery(GdbHandle, query_string))
  } else {
    # Input query parameters are converted to a unnamed list used as bind variable 
    # regardless of the type of input
    query_result <- setDT(RSQLite::dbGetQuery(GdbHandle, query_string, 
                                              list(unname(unlist(list(list(query_params)))))))
  }
  
  return(query_result) 
}


# Oracle
genericQuery_oracle<-function(query_string, query_params=NULL) {
  #print(query_string)
  if (is.null(query_params)){
    cur <- ROracle::dbSendQuery(GdbHandle, query_string)
  } else {
    cur <- ROracle::dbSendQuery(GdbHandle, query_string, query_params)
  }
  
  # Fetch all rows, clear buffer and return data
  query_result<-setDT(ROracle::fetch(cur))
  ROracle::dbClearResult(cur)
  
  return(query_result) 
}


##############################################################################################
## Functions to import a domain specific for each db type
##############################################################################################

# SQLite
doImportDomain_sqlite<-function(domain, studyList=NULL) {
  if (!is.null(studyList)) {
    # Construct the select statement with a filtering of studyid values
    stmt<-sprintf( "select * from %s where studyid in (:1)", domain)

    # Parse select statement and bind input list of studyids
    cur<-RSQLite::dbSendQuery(GdbHandle, stmt)
    RSQLite::dbBind(cur, list(unname(unlist(list(studyList)))))

    # # Save the content of studyList parameter as a temporary table in the database
    # dbWriteTable(GdbHandle, "temp_studyList", studyList, temporary=TRUE, overwrite=TRUE)
    # stmt<-sprintf("select t.* from %s t join temp_studyList s on s.studyid = t.studyid", domain)
    # cur<-RSQLite::dbSendQuery(GdbHandle, stmt)
  }
  else {
    # Construct select statement of all rows and parse it
    stmt<-sprintf("select * from %s", domain)
    cur<-RSQLite::dbSendQuery(GdbHandle, stmt)
  }
  
  # Fetch all rows, clear buffer and return data
  domainData<-setDT(RSQLite::dbFetch(cur))
  RSQLite::dbClearResult(cur)
  
  return(domainData) 
}


# Oracle
doImportDomain_oracle<-function(domain, studyList=NULL) {
  if (!is.null(studyList)) {
    # construct the select statement with a filtering of studyid values
    # - converts to an in-memory table in oracle to limit the set of 
    #   studies to extract
    stmt<-sprintf(
      "with studylist (studyid) as 
        (
            select
              regexp_substr(studyid, '[^,]+', 1, column_value) studyid
            from (select :1 as studyid from dual) t,
                 table(cast(multiset(select level from dual
                                     connect by level <= regexp_count(studyid, ',') + 1
                                    ) as sys.odcinumberlist))
        )
        select * from %s.%s
        where studyid in (select studyid from studylist)", 
      GdbSchema, domain)
    
    # Parse select statement and bind input list of studyids 
    # - convert list  to a comma separated string
    cur<-ROracle::dbSendQuery(GdbHandle, stmt, paste0(unlist(list(studyList)), collapse=',')) 
  }
  else {
    # Construct select statement of all rows and parse it
    stmt<-sprintf("select * from %s.%s", GdbSchema, domain)
    cur<-ROracle::dbSendQuery(GdbHandle, stmt)
  }
  
  # Fetch all rows, clear buffer and return data
  domainData<-setDT(ROracle::fetch(cur))
  ROracle::dbClearResult(cur)
  
  return(domainData) 
}

###################################################################################

# define the path to R scripts to actual script location
dummyuseCaseQuestionMiFindings<-function() {
  # dummy function only to be used to get this script's location
}
setwd(getSrcDirectory(dummyuseCaseQuestionMiFindings))


# Compiling functions included in all modules
source("miscFunctions.R")
source("importSENDDomains.R")
source("studyListStudyDesign.R")
source("filterStudyAnimalSpeciesStrain.R")
source("filterStudyAnimalRoute.R")
source("studyListStudyStartDate.R")
source("animalListControl.R")
source("filterAnimalsSex.R")
source("subjDataExtract.R")
source("filterFindingsPhase.R")
source("addFindingsAnimalAge.R")
source("filterFindingsAnimalAge.R")
