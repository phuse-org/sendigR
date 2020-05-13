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
# Purpose       : Pool SEND data for control groups for a set of studies stored in 
#                 SAS xpt format together into a SQLite database
#
# Description   : Pool the content of all SEND domains for a set of studies located 
#                 in a folder structure with a set of XPT files per study.
#                 Do anly include data for control groups. 
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
library(stringi)
library(Stack)
library(readxl)
library(dplyr)
library(RSQLite)

start<-Sys.time()

# Avoid that character strings are converted to factors
options(stringsAsFactors = FALSE)

# Base folders
baseDir <-'S:/DATA MANAGEMENT/@Data Science/BioCelerate/SEND Harmonization'
studyRoot <- paste(baseDir,'Data/studies',sep='/')
metadataRoot <- paste(baseDir,'Data/metadata',sep='/')
dbRoot <- paste(baseDir,'Data/db_ctrl',sep='/')

# connect to database
db<-dbConnect(RSQLite::SQLite(), paste(dbRoot,"/send.db", sep=""))

# Get list of all SEND 3.1 domains from CDISC SEND 3.1 library archive
allDomains<-pull(read_excel(paste(metadataRoot,"sendig-3-1-excel.xls", sep="/"), sheet=3), 1)
# list of trial design domains
metadataDomains<-c("TS","TA","TE","TX","DM","POOLDEF")
# list of data domains
dataDomains<-setdiff(allDomains, metadataDomains)

##  Collect metadata domains for all studies 

# Create initial empty data frame shell for each metadata domain
for (df_nm in metadataDomains) {
  assign(df_nm, data.frame(STUDYID=character()
                          ,stringsAsFactors=FALSE))
}

# Create empty data frame for list of animals per study 
dmSetAll<-data.frame(STUDYID=character()
                       ,USUBJID=character()
                       ,stringsAsFactors=FALSE)

# Create empty data frame for list of pools per study 
poolSetAll<-data.frame(STUDYID=character()
                        ,POOLID=character()
                        ,stringsAsFactors=FALSE)

# list all TS domain files 
tsFiles <- list.files(path.expand(studyRoot)
                      ,pattern = "^ts.xpt"
                      ,recursive = T
                      ,full.names = T
                      ,ignore.case = T)

# initiate empty list of study folders with missing dm/tx domains
invalidStudyFolders<-c()

# For each study - extract  metadata domains add them to the full collections for each study
for (tsFile in tsFiles) {

  studyFolder<-substring(tsFile, 1,max(stri_locate_last_fixed(tsFile,"/"))-1)
  
  # Check if the required domains DM and TX exist
  
  reqFiles <- list.files(path.expand(studyFolder)
                        ,pattern = "^dm.xpt|^tx.xpt"
                        ,recursive = F
                        ,full.names = T
                        ,ignore.case = T)
  if (length(reqFiles) != 2) {
    invalidStudyFolders<-c(invalidStudyFolders,studyFolder)
  } else {
    
    # Import TS and add all rows to collection 
    # ?? shall we exclude some parameters ??
    TS<-Stack(TS, read.xport(paste(studyFolder,'ts.xpt',sep='/'), as.is=T))
    
    # Import TX and extract control sets 
    tx0 <- read.xport(paste(studyFolder,'tx.xpt',sep='/'), as.is=T)
    txCtrlSet<-as.character(tx0[tx0$TXPARMCD == "TCNTRL",]$SETCD)
    # Extract rows for all control sets
    tx1<-tx0[tx0$SETCD %in% txCtrlSet,]
    # Add TX rows for control sets to collection
    TX<-Stack(TX, tx1)
    
    # Import DM, extract animals from the control sets and add to list of all control animals
    dm0 <- read.xport(paste(studyFolder,'dm.xpt',sep='/'), as.is=T)
    dmSetAll<-Stack(dmSetAll, with(dm0[dm0$SETCD %in% txCtrlSet,], data.frame(STUDYID, USUBJID)))
    # Add DM rows for animals in the control sets to collection
    DM<-Stack(DM, dm0[dm0$SETCD %in% txCtrlSet,])
    
    # Check of POOLDEF exists
    pooldefFile <- list.files(path.expand(studyFolder)
                  ,pattern = "^pooldef.xpt"
                  ,recursive = F
                  ,full.names = T
                  ,ignore.case = T)
    if (length(pooldefFile) != 0) {
      # Import POOLDEF, extract pools containing control animals
      pooldef1<-subset(read.xport(paste(studyFolder,'pooldef.xpt',sep='/'), as.is=T), USUBJID %in% dm0[dm0$SETCD %in% txCtrlSet,]$USUBJID)
      # add pools to list of all control pools
      poolSetAll<-Stack(poolSetAll, distinct(with(pooldef1, data.frame(STUDYID, POOLID))))
      # Add pooldef to collection
      POOLDEF<-Stack(POOLDEF, pooldef1)
    }
    
    # Import TA, extract arms in the control sets and add to collection
    ta1<-subset(read.xport(paste(studyFolder,'ta.xpt',sep='/'), as.is=T), ARMCD %in% tx1[tx0$TXPARMCD == "ARMCD",]$TXVAL)
    TA<-Stack(TA, ta1)
    
    # Import TE, extract only elements from  the control arms and add to collection
    TE<-Stack(TE, subset(read.xport(paste(studyFolder,'te.xpt',sep='/'), as.is=T), ETCD %in% ta1$ETCD))
  }
  
}

# save the full metadata collections database
for (df_nm in metadataDomains) {
  df<-get(df_nm)
  dbWriteTable(db, df_nm, df)
}
# Delete data frames from workspace
rm(list=metadataDomains)


## Collect and save data per data domain and potential related supp domain for all studies 
## - i.e. one pair of domain/supp<domain> is collect for all studies, saved in database and released from workspace 
## in each iteration of the loop

for (domain in dataDomains) {
  # Create initial empty data frame shells for main collection and potential related supp domain collection
  assign(domain, data.frame(STUDYID=character()
                           ,stringsAsFactors=FALSE))
  suppDomain<-paste("SUPP", domain, sep="")
  assign(suppDomain, data.frame(STUDYID=character()
                          ,stringsAsFactors=FALSE))
  
  # get a list of all existing domain files in study folders
  domainFiles <- list.files(path.expand(studyRoot)
                           ,pattern = paste("^", domain, "\\.xpt", sep="")
                           ,recursive = T
                           ,full.names = T
                           ,ignore.case = T)
  if (length(domainFiles) != 0) {
    # do only process if any files are found
    for (domainFile in domainFiles) {
      # Read domain file
      mainDf<-read.xport(domainFile, as.is=T)
      # Check if domain contains a USUBJID variable
      if ("USUBJID" %in% colnames(mainDf)) {
        # extract subject level rows for control animals
        mainDfCtrl<-subset(mainDf,STUDYID %in% dmSetAll$STUDYID & USUBJID %in% dmSetAll$USUBJID)
      }
      # Check if domain contains a POOLID variable
      if ("POOLID" %in% colnames(mainDf)) {
        # extract pool level rows for control animal pools
        mainDfCtrl<-Stack(mainDfCtrl,subset(mainDf,STUDYID %in% poolSetAll$STUDYID & POOLID %in% poolSetAll$POOLID))
      }
      # add the extracted rows to the domain collection
      assign(domain, Stack(get(domain), mainDfCtrl))
             
      # Check if a related supp<domain>.xpt file exists 
      suppDomainFile<-list.files(path.expand(substring(domainFile, 1,max(stri_locate_last_fixed(domainFile,"/"))-1))
                                 ,pattern = paste("^", suppDomain, "\\.xpt", sep="")
                                 ,recursive = F
                                 ,full.names = T
                                 ,ignore.case = T)
      if (length(suppDomainFile) != 0) {
        # Read supp domain file 
        suppDf<-read.xport(suppDomainFile, as.is=T)
        # Check if supp domain contains a USUBJID variable
        if ("USUBJID" %in% colnames(suppDf)) {
          # extract rows for control animals
          suppDfCtrl<-subset(suppDf,STUDYID %in% dmSetAll$STUDYID & USUBJID %in% dmSetAll$USUBJID)
        }
        # Check if supp domain contains a POOLID variable
        if ("POOLID" %in% colnames(suppDf)) {
          # extract pool level rows for control animals
          suppDfCtrl<-Stack(suppDfCtrl,subset(suppDf,STUDYID %in% poolSetAll$STUDYID & POOLID %in% poolSetAll$POOLID))
        }
        if (nrow(suppDf) > 0) {
          # add the extracxted rows to the domain collection
          assign(suppDomain, Stack(get(suppDomain), suppDfCtrl))
        }
      }
      
    }
    # Save main and supp domain data in db 
    mainDfAll<-get(domain)
    suppDfAll<-get(suppDomain)
    if (nrow(mainDfAll) != 0) {
      dbWriteTable(db, domain, mainDfAll)
      if (nrow(suppDfAll)) {
        dbWriteTable(db, suppDomain, suppDfAll)
      }
    }
    # Delete supp and main domain data frames from workspace to save space
    rm(list=c(domain, suppDomain))
    rm(mainDfAll)
    rm(suppDfAll)
  }
}
dbDisconnect(db)

paste("Start: ", start,sep="")
paste("End: ", Sys.time(),sep="")




