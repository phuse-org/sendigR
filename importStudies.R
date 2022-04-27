###################################################################################
library(RSQLite)
library(data.table)
library(reticulate)
#devtools::install_github("rstudio/reticulate")

use_python("/opt/python/3.9.5/bin/python")

setwd('~/GitHub/sendigr')

baseDir <- "~/SEND_DATASETS"
studyRoot <- paste(baseDir,'SEND_DATASETS-test',sep='/')
dbRoot <- paste(baseDir,'SQLLite_DB',sep='/')

# Set working directory to location of script
#set the working directory to the subdirectory with the python App.py file
(pathBase <- getwd())
print(pathBase)
pyPathBase <- paste0(pathBase, "/python/XPTcleaner")
if (!is.null(pyPathBase)) setwd(pyPathBase)

#The source_python() function will source a Python script and make the objects
#it creates available within an R environment
source_python("App.py")

#Call the gen_vocab function with the source CDISC CT file and the target json file
infile <- "../../data-raw/SEND Terminology.txt"
jsonfile <- "generated_vocabs/current.json"
py$gen_vocab(infile,jsonfile )

#Call the standardize_file function to clean the xpt file, current Python only clean one folder, not searching subfolder
rawXptFolder <- "C:/BioCelerate/TDSStudies/35449"
cleanXptFolder <- "C:/BioCelerate/TDSStudiesClean/35449"
py$standardize_file(rawXptFolder, cleanXptFolder, jsonfile )

# xpt clean was done, start to import studies to in cleaXptFolder to the sqllite database.
if (!is.null(pathBase)) setwd(pathBase)


dbToken <- sendigR::initEnvironment(dbType='sqlite',
                           dbPath=paste(dbRoot,'send_tds_cleaned.db',sep='/'),
                           dbCreate=FALSE)
#dbCreateSchema(dbToken)
cleanXptFolder <- "C:/BioCelerate/TDSStudiesClean"
xptfolders <- list.dirs(cleanXptFolder, recursive=FALSE)
for (oneStudy in xptfolders){
  print(oneStudy);
  sendigR::dbImportOneStudy(dbToken, oneStudy, overWrite=TRUE)
}

#PDS datast not loading
# sendigR::dbDeleteStudies(dbToken, list('96481', 'DT20001'))
# sendigR::dbImportOneStudy(dbToken, paste(studyRoot, 'CDISC-Safety-Pharmacology-POC',sep='/'), overWrite=TRUE)
# sendigR::dbImportOneStudy(dbToken, paste(studyRoot, 'CJ16050',sep='/'), overWrite=TRUE)
# sendigR::dbImportOneStudy(dbToken, paste(studyRoot, '10310',sep='/'), overWrite=TRUE)
# sendigR::dbImportOneStudy(dbToken, paste(studyRoot, '11060',sep='/'), overWrite=TRUE)
# sendigR::dbImportOneStudy(dbToken, paste(studyRoot, '96481',sep='/'), overWrite=TRUE) #studyid = '8326556'
#dbImportOneStudy(dbToken, paste(studyRoot, 'Study2-Vaccine',sep='/'), overWrite=TRUE) #CBER-POC
#dbImportOneStudy(dbToken, paste(studyRoot, 'Study3-Gene-Therapy',sep='/'), overWrite=TRUE) #VECTORSTUDYU1
#sendigR::dbImportOneStudy(dbToken, paste(studyRoot, 'Study4-Vaccine',sep='/'), overWrite=TRUE) #RABBITV1
#sendigR::dbCreateIndexes(dbToken, replaceExisting=TRUE)
#Dashboard to started.
#sendigR::execSendDashboard(dbToken)
sendigR::disconnectDB(dbToken)


