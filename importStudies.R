###################################################################################
#reticulate::py_config()
library(sendigR)
baseDir <- "C:/BioCelerate"
studyRoot <- paste(baseDir,'TDSStudies',sep='/')
dbRoot <- "C:/Project/src/R/sendigR/tests/data/"

#set the working directory to the subdirectory with the python xptCleaner.py file
pathBase <-"C:/Project/src/R/sendigRPkg"
if (!is.null(pathBase)) setwd(pathBase)

#Call the gen_vocab function with the source CDISC CT file and the target json file
infile2 <- "SEND_Terminology_EXTENSIBLE.txt"
infile1 <- "SEND Terminology_2021_12_17.txt"
jsonfile <- "SEND3.1_test.json"
sendigR::gen_vocab(list(infile1, infile2),jsonfile)

#Call the standardize_file function to clean the xpt file, current Python only clean one folder, not searching subfolder
rawXptFolder <- "C:/BioCelerate/TDSStudies/96298/"
cleanXptFolder <- "C:/BioCelerate/TDSStudiesClean/96298/"
sendigR::standardize_file(rawXptFolder, cleanXptFolder, jsonfile )

# xpt clean was done, start to import studies to in cleaXptFolder to the sqllite database.
dbToken <- sendigR::initEnvironment(dbType='sqlite',
                           #dbPath=paste(dbRoot,'send_tds_cleaned.db',sep='/'),
                           dbPath=paste(dbRoot,'test_db.db',sep='/'),
                           dbCreate=FALSE)
#dbCreateSchema(dbToken)
# cleanXptFolder <- "C:/BioCelerate/TDSStudiesClean"
# xptfolders <- list.dirs(cleanXptFolder, recursive=FALSE)
# for (oneStudy in xptfolders){
#   print(oneStudy);
#   sendigR::dbImportOneStudy(dbToken, oneStudy, overWrite=TRUE)
# }

#sendigR::dbImportOneStudy(dbToken, paste(studyRoot, 'Study4-Vaccine',sep='/'), overWrite=TRUE) #RABBITV1
#sendigR::dbCreateIndexes(dbToken, replaceExisting=TRUE)
#Dashboard to started.
#sendigR::execSendDashboard(dbToken)
sendigR::disconnectDB(dbToken)


