
dbToken <- initEnvironment(dbType='sqlite',
                           dbPath='//FSDKHQ001/dep402$/000-4284/DATA MANAGEMENT/@Data Science/BioCelerate/SEND Harmonization/Data/db/send02.db')
execSendDashboard(dbToken)
disconnectDB(dbToken)


