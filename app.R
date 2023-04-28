pkgload::load_all()
#need to figure out how to add database
# path <- system.file(".", package = "sendigR")
path <- "tests/data/send_no_duplicates.db"

db <- sendigR::initEnvironment("sqlite", path)
sendigR::execSendDashboard(db)


# PostgreSQL setup --------------------------------------------------------


#Code is commented out for now


## Note: Connection string for postgres db called "send_no_duplicates" -
## "send_no_duplicates" hasn't been written to postgres yet

# db <- sendigR::initEnvironment(
#   dbType = "postgresql",
#   dbPath = "send_no_duplicates2",
#   dbHost = "cder-rapid-prod-datahub.cfb51geqvgd5.us-gov-west-1.rds.amazonaws.com",
#   dbUser = "nanotech",
#   dbPwd = rstudioapi::askForPassword("Database password:"),
#   dbPort = 5432)
#
# sendigR::execSendDashboard(db)
