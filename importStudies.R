###################################################################################
library(sendigR)
studyRoot <- "path/to/files"
dbRoot <- "path/to/db"

# CT files folder
pathBase <-"path/to/ct"

# Output to temp folder
tempDir <- tempdir()

# Call the gen_vocab function with the CDISC CT file, extensible CT file and the target json file
infile2 <- paste0(pathBase, "/SEND_Terminology_EXTENSIBLE.txt")
infile1 <- paste0(pathBase, "/SEND Terminology_2021_12_17.txt")

jsonfile <- paste0(tempDir, "/SEND3.1_test.json")
sendigR::gen_vocab(list(infile1, infile2),jsonfile)

# Call the standardize_file function to harmonize the xpt file.
rawXptFolder <- "path/to/study"
cleanXptFolder <- paste0(tempDir, "/Studies/studydir/")
sendigR::standardize_file(rawXptFolder, cleanXptFolder, jsonfile )

# xpt clean was done, start to import studies in cleaXptFolder to the sqllite database
# Initialization
dbToken <- sendigR::initEnvironment(dbType='sqlite',
                           dbPath=paste(dbRoot,'study_sub.db',sep='/'),
                           dbCreate=TRUE)
# Create database schema
dbCreateSchema(dbToken)

# import studies
cleanXptFolder <- paste0(tempDir, "/Studies")
xptfolders <- list.dirs(cleanXptFolder, recursive=FALSE)
for (oneStudy in xptfolders){
   sendigR::dbImportOneStudy(dbToken, oneStudy, overWrite=TRUE)
}

# Create Indexes if needed
sendigR::dbCreateIndexes(dbToken, replaceExisting=TRUE)

# Start Dashboard
sendigR::execSendDashboard(dbToken)
# Disconnect from database
sendigR::disconnectDB(dbToken)
