###################################################################################
library(sendigR)
baseDir <- "C:/BioCelerate"
studyRoot <- paste(baseDir,'TDSStudies',sep='/')
dbRoot <- "C:/BioCelerate/SQLLite_DB/"

# CT files folder
pathBase <-"C:/Project/src/R/sendigRPkg"

# Output to temp folder
tempDir <- tempdir()
print(tempDir)

# Call the gen_vocab function with the CDISC CT file, extensible CT file and the target json file
infile2 <- paste0(pathBase, "/SEND_Terminology_EXTENSIBLE.txt")
infile1 <- paste0(pathBase, "/SEND Terminology_2021_12_17.txt")
jsonfile <- paste0(tempDir, "/SEND3.1_test.json")
sendigR::gen_vocab(list(infile1, infile2),jsonfile)

# Call the standardize_file function to harmonize the xpt file.
rawXptFolder <- "C:/BioCelerate/TDSStudies/96298/"
cleanXptFolder <- paste0(tempDir, "/Studies/96298/")
sendigR::standardize_file(rawXptFolder, cleanXptFolder, jsonfile )

rawXptFolder <- "C:/BioCelerate/TDSStudies/35449/"
cleanXptFolder <- paste0(tempDir, "/Studies/35449/")
sendigR::standardize_file(rawXptFolder, cleanXptFolder, jsonfile )

rawXptFolder <- "C:/BioCelerate/TDSStudies/43066/"
cleanXptFolder <- paste0(tempDir, "/Studies/43066/")
sendigR::standardize_file(rawXptFolder, cleanXptFolder, jsonfile )

rawXptFolder <- "C:/BioCelerate/TDSStudies/87497/"
cleanXptFolder <- paste0(tempDir, "/Studies/87497/")
sendigR::standardize_file(rawXptFolder, cleanXptFolder, jsonfile )

# xpt clean was done, start to import studies in cleaXptFolder to the sqllite database

# Initialization
dbToken <- sendigR::initEnvironment(dbType='sqlite',
                           dbPath=paste(dbRoot,'tds_study_sub.db',sep='/'),
                           dbCreate=FALSE)
# Create database schema
#dbCreateSchema(dbToken)

# import studies
cleanXptFolder <- paste0(tempDir, "/Studies")
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
