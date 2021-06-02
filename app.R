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
library(ini)

# Source functions
source('https://raw.githubusercontent.com/phuse-org/BioCelerate/c5dd2daf7665a064ecfc015aaee9486f0746f5df/R/sendDB_shiny.R')
source('https://raw.githubusercontent.com/phuse-org/BioCelerate/429044c36a0697017c77247c9ed3f2184801c660/R/controlFiltering_shiny.R')
source('https://raw.githubusercontent.com/phuse-org/BioCelerate/c5dd2daf7665a064ecfc015aaee9486f0746f5df/R/sendDashboard_shiny.R')
source('https://raw.githubusercontent.com/phuse-org/BioCelerate/5913b2251e97cfe7eb6075e935df7a1bbae0e523/R/downloadModule_shiny.R')

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

# execSendDashboard(dbToken)
execSendDashboardSource <- deparse(execSendDashboard)
# a <- eval(parse(text = execSendDashboardSource))
removeIndex <- c(1, 2, grep('shiny::shinyApp(', execSendDashboardSource, fixed =T), length(execSendDashboardSource))
# removeIndex <- c(grep('shiny::shinyApp(', execSendDashboardSource, fixed =T))
execSendDashboardSourceMod <- execSendDashboardSource[-removeIndex]
parsedCode <- parse(text = execSendDashboardSourceMod)
# b <- eval(parse(text = execSendDashboardSourceMod))
# x <- withAutoprint(eval(parse(text = execSendDashboardSource)))
eval(parsedCode)

# ui <- ui
# 
# server <- server

##### Run the application ----
shiny::shinyApp(ui = ui, server = server)


###################################################################
# disconnectDB(dbToken)
