
# rootPath <- '//FSDKHQ001/dep402$/000-4284/DATA MANAGEMENT/@Data Science/BioCelerate/sendigR/tests/data/'

rootPath <-  '../data/'


db <- initEnvironment(dbType='sqlite',
                      dbPath=normalizePath(paste0(rootPath, 'test_db.db')))

test_that('01 - No input study list, no filtering',
  {
    expect_equal(data.table::setorder(getStudiesSTSTDTC(db), 'STUDYID'),
                 data.table::setorder(data.table::as.data.table(readxl::read_xls(normalizePath(paste0(rootPath, 'expected_getStudiesSTSTDTC.xls')),
                                                                                 sheet='expect01'))[,NOT_VALID_MSG := as.character(NOT_VALID_MSG) ],
                                      'STUDYID'))

    expect_equal(data.table::setorder(getStudiesSTSTDTC(db,noFilterReportUncertain = FALSE),
                                      'STUDYID'),
                 data.table::setorder(data.table::as.data.table(readxl::read_xls(normalizePath(paste0(rootPath, 'expected_getStudiesSTSTDTC.xls')),
                                                                                 sheet='expect01'))[,NOT_VALID_MSG := NULL ],
                                      'STUDYID'))
    })

# test_that('02 - No input study list, start day filter, open end',
#   {
#     expect_equal(data.table::setorder(getStudiesSTSTDTC(db,...), 'STUDYID'),
#                  data.table::setorder(data.table::as.data.table(readxl::read_xls(normalizePath(paste0(rootPath, 'expected_getStudiesSTSTDTC.xls')),
#                                                                                  sheet='expect02')),
#                                       'STUDYID'))
#
#
#   })
#
# test_that('03 - No input study list, end day filter, open start',
#   {
#     expect_equal(data.table::setorder(getStudiesSTSTDTC(db,...), 'STUDYID'),
#                  data.table::setorder(data.table::as.data.table(readxl::read_xls(normalizePath(paste0(rootPath, 'expected_getStudiesSTSTDTC.xls')),
#                                                                                  sheet='expect03')),
#                                       'STUDYID'))
#
#
#   })
#
# test_that('04 - No input study list, start and end day filters',
#   {
#     expect_equal(data.table::setorder(getStudiesSTSTDTC(db,...), 'STUDYID'),
#                  data.table::setorder(data.table::as.data.table(readxl::read_xls(normalizePath(paste0(rootPath, 'expected_getStudiesSTSTDTC.xls')),
#                                                                                  sheet='expect04')),
#                                       'STUDYID'))
#
#
#   })

disconnectDB(db)
