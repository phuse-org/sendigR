
# rootPath <- '//FSDKHQ001/dep402$/000-4284/DATA MANAGEMENT/@Data Science/BioCelerate/sendigR/tests/data/'

rootPath <-  '../data/'

getExpected <- function(sheetName) {
  data.table::setorder(data.table::as.data.table(
        readxl::read_xls(normalizePath(paste0(rootPath,
                                              '021_expected_getStudiesSTSTDTC.xls')),
                         sheet=sheetName)), 'STUDYID')
}

db <- initEnvironment(dbType='sqlite',
                      dbPath=normalizePath(paste0(rootPath, 'test_db.db')))

test_that('01 - No input study list, no filtering',
  {
    # 1 - Report uncertainties
    expect_equal(data.table::setorder(getStudiesSTSTDTC(db), 'STUDYID'),
                 getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG) ])

    # 2 - Do not report uncertainties
    expect_equal(data.table::setorder(getStudiesSTSTDTC(db,noFilterReportUncertain = FALSE),
                                      'STUDYID'),
                 getExpected('expect01')[,NOT_VALID_MSG := NULL ])
    })

test_that('02 - No input study list, start day filter, open end',
  {
    # 1 - Full date, do not include uncertain rows
    expect_equal(data.table::setorder(getStudiesSTSTDTC(db,
                                                        fromDTC = '2014-08-01'), 'STUDYID'),
                 getExpected('expect02_1')[is.na(UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

    # 2 - Include uncertain rows
    expect_equal(data.table::setorder(getStudiesSTSTDTC(db,
                                                        fromDTC = '2014-08-01',
                                                        inclUncertain = TRUE), 'STUDYID'),
                 getExpected('expect02_1'))

    # 3 - Partial date, month
    expect_equal(data.table::setorder(getStudiesSTSTDTC(db,
                                                        fromDTC = '2014-07'), 'STUDYID'),
                 getExpected('expect02_3'))

    # 4 - Partial date, year
    expect_equal(data.table::setorder(getStudiesSTSTDTC(db,
                                                        fromDTC = '2016'), 'STUDYID'),
                 getExpected('expect02_4'))

  })

test_that('03 - No input study list, end day filter, open start',
  {
    # 1 - Full date
    expect_equal(data.table::setorder(getStudiesSTSTDTC(db,
                                                        toDTC = '2015-12-31' ), 'STUDYID'),
                 getExpected('expect03_1'))

    # 2 - Partial date, month
    expect_equal(data.table::setorder(getStudiesSTSTDTC(db,
                                                        toDTC = '2014-07' ), 'STUDYID'),
                 getExpected('expect03_2'))

    # 2 - Partial date, year
    expect_equal(data.table::setorder(getStudiesSTSTDTC(db,
                                                        toDTC = '2014' ), 'STUDYID'),
                 getExpected('expect03_1'))
  })

test_that('04 - No input study list, start and end day filters',
  {
    expect_equal(data.table::setorder(getStudiesSTSTDTC(db,
                                                        fromDTC = '2010',
                                                        toDTC = '2014'), 'STUDYID'),
                 getExpected('expect04'))
  })


# test_that('11 - Input study list, no filters',
#           {
#             expect_equal(data.table::setorder(getStudiesSTSTDTC(db,
#                                                                 fromDTC = '2010',
#                                                                 toDTC = '2014'), 'STUDYID'),
#                          getExpected('expect04'))
#           })

# test_that('12 - Input study list, filters',
#           {
#             expect_equal(data.table::setorder(getStudiesSTSTDTC(db,
#                                                                 fromDTC = '2010',
#                                                                 toDTC = '2014'), 'STUDYID'),
#                          getExpected('expect04'))
#           })


test_that('91 - Error handling',
          {
            # Invalid filter dates
            expect_error(getStudiesSTSTDTC(db, fromDTC = '214'),
                         regexp = 'not a valid .* date')
            expect_error(getStudiesSTSTDTC(db),
                         regexp = 'not a valid .* date')
            expect_error(getStudiesSTSTDTC(db, fromDTC = '214', toDTC = '20016'),
                         regexp = 'not a valid .* date')
          })

disconnectDB(db)
