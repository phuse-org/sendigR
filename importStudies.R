###################################################################################
library(sendigR)
baseDir <- "C:/BioCelerate"
studyRoot <- paste(baseDir,'TDSStudies',sep='/')
dbRoot <- "C:/BioCelerate/SQLLite_DB/"

# set the working directory
pathBase <-"C:/Project/src/R/sendigRPkg"
if (!is.null(pathBase)) setwd(pathBase)

# Call the gen_vocab function with the CDISC CT file, extensible CT file and the target json file
infile2 <- "SEND_Terminology_EXTENSIBLE.txt"
infile1 <- "SEND Terminology_2021_12_17.txt"
jsonfile <- "SEND3.1_test.json"
sendigR::gen_vocab(list(infile1, infile2),jsonfile)

# Call the standardize_file function to harmonize the xpt file.
rawXptFolder <- "C:/BioCelerate/TDSStudies/96298/"
cleanXptFolder <- "C:/BioCelerate/TDSStudiesClean/96298/"
sendigR::standardize_file(rawXptFolder, cleanXptFolder, jsonfile )

rawXptFolder <- "C:/BioCelerate/TDSStudies/35449/"
cleanXptFolder <- "C:/BioCelerate/TDSStudiesClean/35449/"
sendigR::standardize_file(rawXptFolder, cleanXptFolder, jsonfile )

rawXptFolder <- "C:/BioCelerate/TDSStudies/43066/"
cleanXptFolder <- "C:/BioCelerate/TDSStudiesClean/43066/"
sendigR::standardize_file(rawXptFolder, cleanXptFolder, jsonfile )

rawXptFolder <- "C:/BioCelerate/TDSStudies/87497/"
cleanXptFolder <- "C:/BioCelerate/TDSStudiesClean/87497/"
sendigR::standardize_file(rawXptFolder, cleanXptFolder, jsonfile )

# xpt clean was done, start to import studies in cleaXptFolder to the sqllite database

# Initialization
dbToken <- sendigR::initEnvironment(dbType='sqlite',
                           dbPath=paste(dbRoot,'tds_study_sub.db',sep='/'),
                           dbCreate=TRUE)
# Create database schema
dbCreateSchema(dbToken)

# import studies
cleanXptFolder <- "C:/BioCelerate/TDSStudiesClean"
xptfolders <- list.dirs(cleanXptFolder, recursive=FALSE)
for (oneStudy in xptfolders){
   print(oneStudy);
   sendigR::dbImportOneStudy(dbToken, oneStudy, overWrite=TRUE)
}

# Create Indexes if needed
sendigR::dbCreateIndexes(dbToken, replaceExisting=TRUE)

# Start Dashboard
sendigR::execSendDashboard(dbToken)
# Disconnect from database
sendigR::disconnectDB(dbToken)
