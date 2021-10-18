################################################################################
## Test cases for the function initEnvironment
##
################################################################################

test_that('DB token',
  {
    # Open database and verify created DB token
    db <- initEnvironment(dbType='sqlite',
                          dbPath='../data/test_db.db')
    expect_equal(db$dbType, 'sqlite')
    expect_equal(class(db$dbHandle)[1], 'SQLiteConnection')
    expect_equal(db$dbHandle@dbname, normalizePath('../data/test_db.db'))
    expect_equal(db$dbSchema, '')
    expect_equal(capture.output(str(db$genericQuery)),
                 capture.output(str(genericQuery_sqlite)))
    expect_equal(capture.output(str(db$disconnectDB)),
                 capture.output(str(disconnectDB_sqlite)))
    expect_equal(capture.output(str(db$dbExistsTable)),
                 capture.output(str(dbExistsTable_sqlite)))
    expect_equal(capture.output(str(db$dbExistsTable)),
                 capture.output(str(dbExistsTable_sqlite)))
    disconnectDB(db)
  }
)

test_that('Error handling',
  {
    expect_error(initEnvironment(dbType='sqlite',
                                 dbPath='../data/none.db'),
                 regexp = 'database file .+ could not be found')
    expect_error(initEnvironment(dbType='sqlite',
                                 dbPath='../data/dummy.db',
                                 dbCreate = TRUE),
                 regexp = 'database file .+ exists.')
  }
)

