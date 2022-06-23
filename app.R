pkgload::load_all()
#need to figure out how to add database
# path <- system.file(".", package = "sendigR")
path <- "tests/data/test_db.db"

db <- sendigR::initEnvironment("sqlite", path)
sendigR::execSendDashboard(db)

