###################################################################################
# Script name   : studyListDoseDuration.R
# Date Created  : 23-Dec-2019
# Documentation : Specification - R script - studyListDoseDuration.docx
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Generate a function GetStudyListDOSDUR to extract a list of SEND  
#                 study ids which fulfills a specified dose duration period
#
# Description   : The function initTS_DOSDUR:
#                   Defines location to and connect to a SQL database (default SQLite) 
#                   which must contain a set of SEND domains with pooled data for a set 
#                   of studies (SEND IG versions 3.0 and/or 3.1).
#                   A set of rows for TS parameter for dose duration is extracted and 
#                   returned as a data frame with these columns:
#                     STUDYID   (<-TS.STUDYID)
#                     DOSDUR    (<-TS.TSVAL where TSPARMCD='DOSDUR')
#                   the columns are characters.
#                 
#                 The function prepareTS_DOSDUR:
#                   Extracts list of studies with a valid DOSDUR value (ISO8601 interval)
#                   following these rules:
#                     - Include only DOSDUR with a valid ISO8601 interval value
#                     - If TS contains multiple DOSDUR parameters for a study, use the 
#                       longest interval.
#                     - Convert the DOSDUR ISO8601 interval into days in variable DOSDURd
#                     - If multiple DOSDUR values for a study results in same DOSDURd value,
#                       exclude others than the largest one (sorted alphabetically).
#
#                 The function GetStudyListDOSDUR:
#                   Extracts a list of which fulfils the input parameter value DurInterval, 
#                 using these rules for conversion of months to days:
#                     1 month  =>  28 days
#                     3 months =>  91 days  
#                     6 months => 182 Days 
#                     9 months => 273 days
#
# Input         : Pooled SEND domain TS.
#                 The current implementation uses a SQLite database named send.db as 
#                 data store.
#
# Output        : The function GetStudyListDOSDUR returns a data frame with these columns:
#                     STUDYID (character)
#                     DOSDUR  (character) - original DOSDUR value 
#                     DOSURd  (number)    - DOSDUR convertered to days
#
# Parameters    : The function GetStudyListDOSDUR must be called with one input parameter
#                 DurInterval specifying the wanted dose duration using one of these values:
#                   "A": less than or equal to 1 month,
#                   "B": greater than 1 and less than 3 months
#                   "C": 3 to 6 months
#                   "D": greater than 6 months
#                   A duration in weeks specified as (where n is the specified number):
#                       "< n", "> n", "= n"
#
# Usage notes   : Change the paths to the database in the function initTS_DOSDUR to
#                 point to the correct locations.
#                 If you are not using SQLite - update the function initTS_DOSDUR to 
#                 match the actual data store containing the TS domain with data for a set of 
#                 studies - remember to include potentiel extra package(s).
#                 Ensure function initTS_DOSDUR still returns a dataframe with the character variables 
#                 described for the fucntion in the Description section above.

#                 Execute the script to extract the total list of valid STUDYID/DOSDUR/DOSDURd 
#                 values.
#                 Execute the function GetStudyListDOSDUR as many times as needed to extract  
#                 list(s) of studies for the wanted dose duration periods.
# 
#######################################################################################################################################################################

library(RSQLite)
library(lubridate)
library(DescTools)
library(varhandle)

########################################################################
# Initial stuff

# Connect to the database and extractlist of all studies and DOSDUR values
initTS_DOSDUR<-function() {
  # define path to databse and connect
  baseDir <-'S:/DATA MANAGEMENT/@Data Science/BioCelerate/SEND Harmonization'
  dbRoot <- paste(baseDir,'Data/db_ctrl',sep='/')
  db<-dbConnect(RSQLite::SQLite(), paste(dbRoot,"/send.db", sep=""))
  
  # Extract all rows for param DOSDUR 
  ts_0<-dbGetQuery(db, "
    select studyid
          ,tsval as DOSDUR
      from TS
    where tsparmcd = 'DOSDUR'
              ")
  
  # disconnect from database and return extracted list of data
  dbDisconnect(db)
  ts_0
}

# Extract complete list of studies which includes the TS DOSDUR parameter 
# with a valid value
# - includes the original DOSDUR value and value converted to days (DOSDURd)
prepareTS_DOSDUR<-function() {
  # Extract all rows for param DOSDUR 
  ts_1<-initTS_DOSDUR()

  # Convert DOSDUR to days and add column to ts - exclude rows with missing/invalid DOSDUR value
  DOSDURd<-round(as.numeric(duration(as.character(ts_1$DOSDUR)))/60/60/24, digits=0)
  ts_2<-na.omit(cbind(ts_1,DOSDURd))
  
  # Extract one row per study with max DOSDURd value
  ts_dosdurd<-aggregate(DOSDURd ~ STUDYID, data=ts_2, max)
  
  # Join with total list to get the DOSDUR value(s) per max DOSDURd
  ts_3<-merge(x=ts_2, y=ts_dosdurd)
  
  # Extract one row per study with max DOSDUR value 
  # - to eliminate potential mulitple DOSDUR value giving the same DOSDURd value for a study
  ts_3["DOSDUR"]<-lapply(ts_3["DOSDUR"], as.character)
  ts_dosdur<-aggregate(DOSDUR ~ STUDYID, data=ts_3, max)
  
  # Join the lists of max DOSDUDd and related max DOSDUR values
  # - return the data frame
  merge(x=ts_dosdur, y=ts_dosdurd)
}

# Save the extracted list of valid DOSDUR values for use in GetStudyList
StudyListAllDOSDUR<-prepareTS_DOSDUR()

#########################################################################
# Define functiuon to extract a list of studies fullfiling the specified 
# duration interval

GetStudyListDOSDUR<-function(DurInterval) {
  # Remove all spaces, parse string and set where condition
  DurInterval<-gsub(" ", "", DurInterval,fixed=TRUE)
  errMsg<-""
  if (DurInterval == "A") {
    # less than/equal to a month (A)
    where<-"<= 28"
  } else if (DurInterval == "B") {
    # 1 to 3 months (no inclusive) (B)
    where<-"%in% 29:90"
  } else if (DurInterval == "C") {
    # 3 to 6 months (C)
    where<-"%in% 91:182" 
  } else if (DurInterval == "D") {
    # more than 6 months (D)
    where<-"> 182"
  } else  {
    ValidOper<-c('>','<','=')
    oper<-StrLeft(DurInterval,1)
    if (oper %in% ValidOper) {
      weeks<-StrRight(DurInterval,-1)
      if (check.numeric(v=weeks,only.integer=TRUE) ) {
        if (oper == "=") {
          # convert one '=' to the necessary '=='
          oper<-"=="
        }
        where<-paste(oper, weeks,"* 7") 
      } else {
        errMsg<-"Invalid number of weeks"  
      }
    } else {
      errMsg<-"Invalid interval"
    }
  }
  if (errMsg=="") {
    expr<-paste("StudyListAllDOSDUR[StudyListAllDOSDUR$DOSDURd ", where, ", ]", sep="")
    eval(str2lang(expr))
  } else {
    paste('ERROR: ', errMsg)
  }
}


##########################################################
# Examples

# valid input intervals
# tsList<-GetStudyListDOSDUR("= 4")
# dfDosDur<-GetStudyListDOSDUR("< 4")
# GetStudyListDOSDUR("> 12")
# GetStudyListDOSDUR("A")
# GetStudyListDOSDUR("B")
# GetStudyListDOSDUR("C")
# GetStudyListDOSDUR("D")

# Invalid input intervals
# GetStudyListDOSDUR("X")
# GetStudyListDOSDUR("=4w")
##########################################################




