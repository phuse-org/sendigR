pkgload::load_all()
#need to figure out how to add database
# path <- system.file(".", package = "sendigR")
path <- "tests/data/send_no_duplicates.db"

db <- sendigR::initEnvironment("sqlite", path)
sendigR::execSendDashboard(db)

