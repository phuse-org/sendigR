################################################################################
## Test cases for the function initEnvironment with PostgreSQL database
##
################################################################################


test_that('DB token',
          {
            # Open database and verify created DB token
            db <- initEnvironment(
              dbType = 'postgresql',
              dbPath = 'sendigr_test',
              dbHost = 'cder-rapid-prod-datahub.cfb51geqvgd5.us-gov-west-1.rds.amazonaws.com',
              dbUser = 'sendigr_read',
              dbPwd = rstudioapi::askForPassword("Database password:")
            )

            expect_equal(db$dbType, 'postgresql')
            expect_equal(class(db$dbHandle)[1], 'PqConnection')
            expect_equal(db$dbSchema, '')
            expect_equal(capture.output(str(db$genericQuery)),
                         capture.output(str(genericQuery_postgresql)))
            expect_equal(capture.output(str(db$disconnectDB)),
                         capture.output(str(disconnectDB_postgresql)))
            expect_equal(capture.output(str(db$dbExistsTable)),
                         capture.output(str(dbExistsTable_postgresql)))
            expect_equal(capture.output(str(db$dbListFields)),
                         capture.output(str(dbListFields_postgresql)))
            disconnectDB(db)
          })


test_that('Error handling',
          {
            expect_error(
              initEnvironment(
                dbType = 'postgresql',
                dbPath = 'none',
                dbHost = 'cder-rapid-prod-datahub.cfb51geqvgd5.us-gov-west-1.rds.amazonaws.com',
                dbUser = 'sendigr_read',
                dbPwd = rstudioapi::askForPassword("Database password:")),
              regexp = 'FATAL:  database .+ does not exist'
            )

            expect_error(
              initEnvironment(
                dbType = 'postgresql',
                dbPath = 'sendigr_test',
                dbHost = 'cder-rapid-prod-datahub.cfb51geqvgd5.us-gov-west-1.rds.amazonaws.com',
                dbCreate = TRUE,
                dbUser = 'sendigr_read',
                dbPwd = rstudioapi::askForPassword("Database password:")),
              regexp = 'Parameter dbCreate = TRUE only allowed for SQLite database'
            )
          })
