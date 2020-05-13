###################################################################################
# Script name   : poolAllStudyDomains.R
# Date Created  : 23-Dec-2019
# Documentation : <if relevant, reference to specification document>
# Programmer    : Bo Larsen
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

# Avoid that character strings are converted to factors
#options(stringsAsFactors = FALSE)

# Base folders
baseDir <-'S:/DATA MANAGEMENT/@Data Science/BioCelerate/SEND Harmonization'
studyRoot <- paste(baseDir,'Data/studies',sep='/')
metadataRoot <- paste(baseDir,'Data/metadata',sep='/')
dbRoot <- paste(baseDir,'Data/db',sep='/')

# connect to database
db<-dbConnect(RSQLite::SQLite(), paste(dbRoot,"/send.db", sep=""))

# Get list of all SEND 3.1 domains from CDISC SEND 3.1 library archive
allDomains<-pull(read_excel(paste(metadataRoot,"sendig-3-1-excel.xls", sep="/"), sheet=3), 1)
# list of trial design domains
notGeneralDataDomains<-c("TS","TA","TE","TX","DM","POOLDEF","SUPPQUAL", "RELREC")
# list of data domains
suppDomains<-paste("SUPP",setdiff(allDomains, notGeneralDataDomains), sep="")


# list all TS domain files 
tsFiles <- list.files(path.expand(studyRoot)
                      ,pattern = "^ts.xpt"
                      ,recursive = T
                      ,full.names = T
                      ,ignore.case = T)

# initiate empty list of study folders with missing dm/tx domains
invalidStudyFolders<-c()


## Collect and save data per data domain for all studies 
## - i.e. one domain is collected  for all studies, saved in database and released
## from workspace in each iteration of the loop

for (domain in c(allDomains,suppDomains)) {
  # Create initial empty data table for collection 
  assign(domain, data.table())
  
  # get a list of all existing domain files in study folders
  domainFiles <- list.files(path.expand(studyRoot)
                           ,pattern = paste("^", domain, "\\.xpt", sep="")
                           ,recursive = T
                           ,full.names = T
                           ,ignore.case = T)
  if (length(domainFiles) != 0) {
    # do only process if any files are found
    for (domainFile in domainFiles) {
      # Read domain file and the extracted rows to the domain collection
      assign(domain, rbindlist(list(get(domain), setDT(remove_all_labels(read.xport(domainFile, as.is=T)))),fill=TRUE))
    }
    # Save domain data in db 
    DT<-get(domain)
    if (nrow(DT) != 0) {
      dbWriteTable(db, domain, DT)
    }
    # Delete domain data table from workspace to save space
    rm(list=c(domain))
    rm(DT)
  }
}
dbDisconnect(db)

paste("Start: ", start,sep="")
paste("End: ", Sys.time(),sep="")




