###################################################################################
# Script name   : poolAllStudyDomainsMDf.R
# Date Created  : 7-May-2020
# Documentation : <if relevant, reference to specification document>
# Programmer    : Daniel P. Russo
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Pool SEND data for a set of studies stored in SAS xpt format
#                 together into a SQLite database
#
# Description   : Pool the content of all SEND domains for a set of studies located 
#                 in a folder structure with a set of XPT files per study.
#                 Save eached pooled domain as a table in a SQLite database
#
# Input         : - A set of folders - one for each SEND study containinng an xpt file 
#                 per SEND domain.
#                 The folders must be located below the root folder specified in the
#                 variable studyRoot (direct below the root folder or in a hierarchy
#                 of sub folders below the root folder)
#                 - The SEND IG 3.1 domain/variable metadata downloaded from the CDISC 
#                 library archive - sendig-3-1-excel.xls located in a folder specified
#                 in the variable metadataRoot
#
# Output        : An SQLite database named send.db located below a folder specified 
#                 in the variable dbRoot
#
# Usage notes   : Before execution of the program - ensure that
#                 - The folders in the 'Base folder' section below are updated to 
#                   actual locations
#                 - all input folders and files exists below the correct locations
#                 - no SQLite database file named send.db exists below dbRoot
#
###################################################################################

# Load Packages
library(SASxport)
#library(stringi)
#library(Stack)
library(readxl)
library(dplyr)
library(RSQLite)
library(data.table)
library(sjlabelled)

start<-Sys.time()


studyRoot <- Sys.getenv('SEND_DATA_V2')
metadataRoot <- file.path(dirname(studyRoot), 'metadata')
domainVariablesFile30 <- file.path(metadataRoot, 'variables30.csv')
domainVariablesFile31 <- file.path(metadataRoot, 'variables31.csv')
domainVariables30 <- read.csv(domainVariablesFile30, stringsAsFactors = FALSE)[c('Domain.Prefix', 'Variable.Name', 'Type')]
domainVariables31 <- read.csv(domainVariablesFile31, stringsAsFactors = FALSE)[c('Domain.Prefix', 'Variable.Name', 'Type')]

dbRoot <- Sys.getenv('SEND_DB_V3')

# connect to database
db<-dbConnect(RSQLite::SQLite(), paste(dbRoot,"/send_3_2.db", sep=""))

# Get list of all SEND 3.1 domains from CDISC SEND 3.1 library archive
allDomains<-pull(read_excel(paste(metadataRoot,"sendig-3-1-excel.xls", sep="/"), sheet=3), 1)
# list of trial design domains
notGeneralDataDomains<-c("TS","TA","TE","TX","DM","POOLDEF","SUPPQUAL", "RELREC")
# list of data domains
suppDomains<-paste("SUPP",setdiff(allDomains, notGeneralDataDomains), sep="")



# list all define.xml

defineFiles <- list.files(path.expand(studyRoot)
                      ,pattern = "^define.xml"
                      ,recursive = T
                      ,full.names = T
                      ,ignore.case = T)

# create a list of ALL accepable variables within a domain

variableList <- list()
for (domain in allDomains){
  variables <- as.vector(domainVariables30[domainVariables30$Domain.Prefix == domain,]$Variable.Name)
  variables <- c(variables, as.vector(domainVariables31[domainVariables31$Domain.Prefix == domain,]$Variable.Name))
  variables <- c(variables, c(paste(domain, 'REFID', sep="")))
  
  # if any variables end in DY make VISITDY permissible
  if (any(grepl("[a-zA-Z]{2}DY", variables))) {
    variables <- c(variables, c("VISITDY"))
  }
  
  variableList[[domain]] <- unique(variables)
}



# define some functions for filtering studyies
# basically, just check to make sure a certain
# study folder has TS, DM, and TX domains
# Note: the TS checker is redudant bc this is
# already in Bo's code, but just adding it for
# completeness and readability
HasValidDM <- function(studyRoot) {
  dmFile <- file.path(studyRoot, "dm.xpt")
  return(file.exists(dmFile))
}

HasValidTS <- function(studyRoot) {
  tsFile <- file.path(studyRoot, "ts.xpt")
  return(file.exists(tsFile))
}

HasValidTX <- function(studyRoot) {
  txFile <- file.path(studyRoot, "tx.xpt")
  return(file.exists(txFile))
}

# some more functions to check that the all the xpt 
# files in a folder are readable

# TODO: potential flags for studies/domains


HasValidXPTs <- function(studyRoot) {
  xptFiles <- list.files(path.expand(studyRoot), pattern = "*.xpt")
  
  allValid <- TRUE
  
  
  # iterate through all the xptFiles in a directory
  # which should include custom domains as well
  # try to read the file.  if any are invalid
  # the function will return false
  
  for (xptFile in xptFiles) {
    validXpt <- try({
      read.xport(file.path(studyRoot, xptFile), as.is=T)
    })
    
    if (class(validXpt) == "try-error"){
      allValid <- FALSE
      break
    }
  }
  return(allValid)
}

HasProperVaraiblesInDomains <- function(folder) {
  for (domain in allDomains) {
    
    allTrue <- TRUE
    
    variables <- variableList[[domain]]
    
    if (domain != 'SUPPQUAL') {
  
      fName <- sprintf('%s.xpt', tolower(domain))
      dmFile <- file.path(folder, fName)
    
      
      if (file.exists(dmFile)) {
        studyData <- remove_all_labels(read.xport(dmFile, as.is=T))
        
        valRegEx <- paste(domain, "VAL[0-9]+", sep="")

        if (!all(colnames(studyData) %in% variables | grepl(valRegEx, colnames(studyData)))) {
          allTrue <- FALSE
          # print(variables)
          print(colnames(studyData)[(!colnames(studyData) %in% variables)])
          # print(dmFile)
          break
        }
      }
    }
    
    if (domain == 'SUPPQUAL') {
      
      for (suppdomain in suppDomains) {
        fName <- sprintf('%s.xpt', tolower(suppdomain))
        dmFile <- file.path(folder, fName)
        
        if (file.exists(dmFile)) {
          studyData <- remove_all_labels(read.xport(dmFile, as.is=T))
          valRegEx <- paste(suppdomain, "VAL[0-9]+", sep="")
          
          # only allow varaibles listed in the domain or any of the
          # sort DMVAL1, DMVAL2, etc...
          if (!all(colnames(studyData) %in% variables | grepl(valRegEx, colnames(studyData)))) {
            allTrue <- FALSE
            break
          }
        }
      }
    }
  }
  return(allTrue)
}

AllStudyIDsMatch <- function(folder) {
  # this function will go through each domain in a folder
  # and make sure that there is only one studyid across
  # all domains 
  
  # empty vector to store all the resulting
  domainStudyIDs <- c()
  dms <- c()
  
  IsValid <- TRUE
  
  for (domain in c(allDomains,suppDomains)) {
    fName <- sprintf('%s.xpt', tolower(domain))
    dmFile <- file.path(folder, fName)
    
    if (file.exists(dmFile)) {
      studyData <- remove_all_labels(read.xport(dmFile, as.is=T))
      # get unique studyids for this domain
      studyid <- unique(studyData$STUDYID)
      if (length(studyid) > 1) {
        IsValid <- FALSE
        break
      } else {
        domainStudyIDs <- append(domainStudyIDs, studyid)
        dms <- append(dms, domain)
      }
    }
    
    if (length(unique(domainStudyIDs)) > 1) {
      IsValid <- FALSE
    }
  }
  return(IsValid)
}

RemoveInvalidFiles <- function(root) {
  # function to remove all the invalid.csv 
  # files in the study root directory 
  invalidFiles <- list.files(path.expand(root)
                             ,pattern = "^invalid.csv"
                             ,recursive = T
                             ,full.names = T
                             ,ignore.case = T)
  for (fn in invalidFiles) {
    file.remove(fn)
  }
}




# after each folder is inputted
# a file called inputedDB.csv
# should be written to the directory
# this saves on having to reload the 
# xpt files after each file is 
# processed.
IsInputted <- function(studyRoot) {
  inputedFile <- file.path(studyRoot, "inputed2DB.csv")
  return(file.exists(inputedFile))
}


# wrapper function that combines all the checks 
# to see if a particular study is valid

IsValidStudy <- function(folder) {
  # returns True if all the folders containing the 
  # particular STUDYID passes all the validity check,
  # if it fails it will return the order of the
  # 
  if (IsInputted(folder)) {
    return("Already inputted")
  } else if (!HasValidDM(folder)) {
    return("No valid DM")
  } else if (!HasValidTS(folder)) {
    return("No valid TS")
  } else if (!HasValidTX(folder)) {
    return("No valid TX")
  } else if (!HasValidXPTs(folder)) {
    return("Not all XPTs are valid")
  } else if (!HasProperVaraiblesInDomains(folder)) {
    return("Missing varaible domains")
  } else if (!AllStudyIDsMatch(folder)) {
    return("Study ID's do not match")
  } else {
    return(TRUE)
  }
}  



## Collect and save data per data domain for all studies 
## - i.e. one domain is collected  for all studies, saved in database and released
## from workspace in each iteration of the loop

# using the defined filter functions above, 
# loop through all the define folders
# and determine whether that folder has 
# a 'valid' study based on those criteria

# create a main table to store 
# application numbers, INDs and 
# the time that specific IND
# was modified on the EDR

if (!'AN' %in% dbListTables(db)) {
  dbExecute(db, 'CREATE TABLE AN (STUDYID, APPNUMBER, EDRMDF)')
}

for (domain in allDomains) {
  
  # for making the tables, only use the variables
  # listed in the IGs, instead of all the acceptable
  # variables for a domain
  variables <- as.vector(domainVariables30[domainVariables30$Domain.Prefix == domain,]$Variable.Name)
  v <- unique(c(variables, as.vector(domainVariables31[domainVariables31$Domain.Prefix == domain,]$Variable.Name)))
  

  if ((!domain %in% dbListTables(db)) & (domain != 'SUPPQUAL')) {
    
    queryString <- paste('CREATE TABLE ', domain, ' ( ')
    
    for (i in 1:length(v)) {
      queryString <- paste(queryString, " '", v[i], "',", sep="")
    }
    
    queryString <- substr(queryString, 1, nchar(queryString)-1)
    queryString <- paste(queryString, ' )', sep="")
    
    dbExecute(db, queryString)
  }
  
  if (domain == 'SUPPQUAL') {
  
    for (suppdomain in suppDomains) {
    
      if (!suppdomain %in% dbListTables(db)) {
      
        queryString <- paste("CREATE TABLE ", suppdomain, ' ( ', sep="")
        
        for (i in 1:length(v)) {
          queryString <- paste(queryString, " '", v[i], "',", sep="")
        }
        queryString <- substr(queryString, 1, nchar(queryString)-1)
        queryString <- paste(queryString, ' )', sep="")
        dbExecute(db, queryString)
      }
    }
  }
  
}




studyFolders <- c()

invalidFolders <- c()
reason <- c()
times <- c()

for (defineFile in defineFiles[1:100]) {
  studyFolder <- dirname(defineFile)
  invalidFile <- file.path(studyFolder, 'invalid.csv')
  if (!file.exists(invalidFile)) {
    
    message <- IsValidStudy(studyFolder)
    
    if (message == TRUE) {
      studyFolders <- append(studyFolders, studyFolder)
    } else {
      invalidFile <- file.path(studyFolder, 'invalid.csv')
      time <- as.POSIXlt(Sys.time(), "EST", "%Y-%m-%dT%H:%M:%S")
      ts <- c(format(time, format="%Y-%m-%dT%H:%M:%S"))
      write.csv(data.frame(REASON=c(message), TIME=c(ts)), invalidFile)
      
      invalidFolders <- append(invalidFolders, c(studyFolder))
      reason <- append(reason, c(message))
      times <- append(times, c(ts))
      
    }
  }
}

logFrame <- data.frame(INVALID_FOLDERS=invalidFolders, REASON=reason, TIME=times)

logFileName <- file.path(studyRoot, '.sqliteload.log')

if (file.exists(logFileName)) {
  oldLogFrame <- read.csv(logFileName, stringsAsFactors=FALSE)
  logFrame <- rbind(oldLogFrame, logFrame)
}

write.csv(logFrame, logFileName)


for (studyFolder in studyFolders) {
  
  # us ts domain to get study name 
  
  fName <- 'ts.xpt'
  dmFile <- file.path(studyFolder, fName)
  tsData <- remove_all_labels(read.xport(dmFile, as.is=T))
  studyID <- unique(tsData$STUDYID)
  appNumber <- basename(dirname(studyFolder))
  
  queryString <- sprintf("SELECT * FROM AN WHERE STUDYID == '%s' AND APPNUMBER == '%s'", studyID, appNumber)
  
  lastModifiedRecord <- dbGetQuery(db, queryString)$EDRMDF
  
  lastModifiedEDR <- read.csv(file.path(studyFolder, '.lastupdate_EDR.log'), header=FALSE, stringsAsFactors=FALSE)
  timeString <- strptime(lastModifiedEDR$V1, "%Y-%m-%dT%H:%M:%S")
  lastModifiedFileObj <- as.POSIXlt(timeString)
  
  ISNEW <- FALSE
  
  # if no date, add 
  if (length(lastModifiedRecord) == 0) {

    df <- data.frame(STUDYID=c(studyID), APPNUMBER=c(appNumber), EDRMDF=(as.character(timeString)))
    dbWriteTable(db, 'AN', df, append=TRUE) 
    ISNEW <- TRUE
  } else {
    
    lastModifiedRecordObj <- as.POSIXlt(lastModifiedRecord)
    
    if (lastModifiedRecordObj <= lastModifiedFileObj) {
      ISNEW <- TRUE
    } else {
      ISNEW <- FALSE
    }
  }
  
  if (ISNEW) {
    for (domain in c(allDomains,suppDomains)) {
      
      
      fName <- sprintf('%s.xpt', tolower(domain))
      dmFile <- file.path(studyFolder, fName)
      
      if (file.exists(dmFile)) {
      
        # delete any previous instances of the study in this domain
        # and then load the study data
        deleteString <- sprintf("DELETE FROM %s wHERE STUDYID == '%s'", domain, studyID)
        dbExecute(db, deleteString)
        
        studyData <- remove_all_labels(read.xport(dmFile, as.is=T))
        
        
        # only allow cerain the right columns 
        # for this domain but also used regular
        # expressions to allow for any of the
        # varaibles that can have VAL1, Val2, etc....
        columns <- dbGetQuery(db, sprintf('PRAGMA table_info(%s);', domain))$name
        valRegEx <- paste(domain, "VAL[0-9]+", sep="")
        studyData <- studyData[colnames(studyData) %in% variableList[domain] | grepl(valRegEx, colnames(studyData))]
        
        # now on the fly we need to write columns 
        # that may not be in the database 
        
        for (col in setdiff(colnames(studyData), columns)) {
          alterTableQuery <- paste("ALTER TABLE ", domain, ' ADD COLUMN ', toupper(col), sep="")
          dbExecute(db, alterTableQuery)
          
        }
        
        dbWriteTable(db, domain, unique(studyData), append=TRUE)
        
      }
    }
  }
}

dbDisconnect(db)

paste("Start: ", start,sep="")
paste("End: ", Sys.time(),sep="")



