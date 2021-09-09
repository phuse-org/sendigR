library(shiny)
library(shinydashboard)
library(tidyverse)
library(MASS)
library(htmltools)
library(DT)
library(sendigR)
library(scales)
library(dplyr)
library(parsedate)
library(Hmisc)
library(ini)


# define the path to R scripts to actual script location
dummyGetLocation<-function() {
  # dummy function only to be used to get this script's location
}
setwd(getSrcDirectory(dummyGetLocation))

# source('sendDB_sendigR.R')
# source('controlFiltering_sendigR.R')
source('sendDB_shiny.R')
source('controlFiltering_shiny.R')
source('sendDashboard_shiny.R')
source('downloadModule_shiny.R')

# Import parameter file from same folder as program is located
iniParmsFile <- 'sendDashboard.ini'
if (! file.exists(iniParmsFile))
  stop(sprintf('The init file %s cannot be found in %s',iniParmsFile, getwd()))

iniParms<-read.ini(iniParmsFile)
iniDbType <- iniParms$`database`$`dbType`   ## add check for valid dbType
iniDbPath <- iniParms$`database`$`dbPath`
iniDbSchema <- iniParms$`database`$`dbSchema`
iniCtFile <- iniParms$`ct`$`ctFile`
if (is.null(iniDbType) | is.null(iniDbPath) | is.null(iniCtFile))
  stop(sprintf('All of these parameters in %s/%s must have assigned a value: dbType,dbPath, ctFile', 
               getwd(),iniParmsFile))

# Execute the SEND function initiation

dbToken <- sendigR::initEnvironment(dbType = iniDbType, 
                                    dbPath = iniDbPath, 
                                    dbSchema = iniDbSchema,
                                    ctFile = iniCtFile)

execSendDashboard(dbToken)
dbDisconnect(dbToken)
