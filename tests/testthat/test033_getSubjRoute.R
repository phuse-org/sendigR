################################################################################
## Test cases for the function getSubjRoute
##
## Expected data is located in: 032_expected_getSubjRoute.xls
## Input list of animals is also located in this Excel sheet
##
################################################################################

# Test data covers these situations:
#
# Valid ROUTE values
# Invalid ROUTE CT value (in TS and EX)
# Missing SEX value (in TS and EX)
# ...

# rootPath <- '//FSDKHQ001/dep402$/000-4284/DATA MANAGEMENT/@Data Science/BioCelerate/sendigR/tests/data/'

#### Prepare for test execution
rootPath <-  '../data/'

getExpected <- function(sheetName) {
  data.table::setkeyv(data.table::as.data.table(
    readxl::read_xls(normalizePath(paste0(rootPath,
                                          '033_expected_getSubjRoute.xls')),
                     sheet=sheetName)), c('STUDYID','USUBJID'))
}

db <- initEnvironment(dbType='sqlite',
                      dbPath=normalizePath(paste0(rootPath, 'test_db.db')))

animalListIn1 <- getExpected('input_animalListIn')
animalListIn2 <- data.table::copy(animalListIn1)[,UNCERTAIN_MSG := NULL]

test_that('01 - No filtering',
{
  animalListIn0 <- data.table::copy(animalListIn1)
  data.table::setnames(animalListIn0, 'UNCERTAIN_MSG' ,'NOT_VALID_MSG')

  # 1 - Report uncertainties
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn0,
                                                noFilterReportUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])


  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                noFilterReportUncertain = FALSE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])


  # 3 - Parameter inclUncertain have no influence of reporting of uncertainties
  #   a - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                noFilterReportUncertain = FALSE,
                                                inclUncertain = TRUE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])
  #   b - Report uncertainties
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn0,
                                                noFilterReportUncertain = TRUE,
                                                inclUncertain = FALSE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])


  # 4 - Parameters matchAll and exclusively have no influence of returned list off animals
  #   a - both FALSE
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                matchAll = FALSE,
                                                exclusively = FALSE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])

  #   b - both TRUE
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                matchAll = TRUE,
                                                exclusively = TRUE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])

})

test_that('02 - Specified one filter value',
{
  # 1 - Report uncertainties, matchAll is FALSE,  Exclusively is FALSE
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn1,
                                                routeFilter = 'ORAL',
                                                matchAll = FALSE,
                                                exclusively = FALSE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02')[,UNCERTAIN_MSG := NULL ])

  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                routeFilter = 'ORAL',
                                                matchAll = FALSE,
                                                exclusively = FALSE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02')[,UNCERTAIN_MSG := NULL ])


  # 3 - Parameter noFilterReportUncertain have no influence of reporting of uncertainties
  #   a - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn1,
                                                routeFilter = 'ORAL',
                                                matchAll = FALSE,
                                                exclusively = FALSE,
                                                inclUncertain = FALSE,
                                                noFilterReportUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02')[,UNCERTAIN_MSG := NULL ])

  #   b - Report uncertainties
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn1,
                                                routeFilter = 'ORAL',
                                                matchAll = FALSE,
                                                exclusively = FALSE,
                                                inclUncertain = TRUE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02')[,UNCERTAIN_MSG := NULL ])

  # 4 - matchAll is TRUE, Exclusively is FALSE
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                routeFilter = 'ORAL',
                                                matchAll = TRUE,
                                                exclusively = FALSE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02_4')[,UNCERTAIN_MSG := NULL ])

  # 5 - matchAll is FALSE, Exclusively is TRUE
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                routeFilter = 'ORAL',
                                                matchAll = FALSE,
                                                exclusively = TRUE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02_5')[,UNCERTAIN_MSG := NULL ])

  # 6 - matchAll is TRUE, Exclusively is TRUE
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                routeFilter = 'ORAL',
                                                matchAll = TRUE,
                                                exclusively = TRUE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02_6')[,UNCERTAIN_MSG := NULL ])
})

test_that('03 - Specified multiple filter values',
{

  # 1 - Report uncertainties, matchAll is FALSE,  Exclusively is FALSE
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn1,
                                                routeFilter = list('ORAL','ORAL GAVAGE'),
                                                matchAll = FALSE,
                                                exclusively = FALSE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect03')[,UNCERTAIN_MSG := NULL ])

  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                routeFilter = c('ORAL','ORAL GAVAGE'),
                                                matchAll = FALSE,
                                                exclusively = FALSE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect03')[,UNCERTAIN_MSG := NULL ])

  # 3 - matchAll is TRUE, Exclusively is FALSE
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                routeFilter = c('ORAL','ORAL GAVAGE'),
                                                matchAll = TRUE,
                                                exclusively = FALSE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect03_3')[,UNCERTAIN_MSG := NULL ])

  # 4 - matchAll is FALSE, Exclusively is TRUE
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                routeFilter = c('ORAL','ORAL GAVAGE'),
                                                matchAll = FALSE,
                                                exclusively = TRUE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect03_4')[,UNCERTAIN_MSG := NULL ])

  # 5 - matchAll is TRUE, Exclusively is TRUE
  expect_equal(data.table::setkeyv(getSubjRoute(db,
                                                animalList = animalListIn2,
                                                routeFilter = c('ORAL','ORAL GAVAGE'),
                                                matchAll = TRUE,
                                                exclusively = TRUE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect03_5')[,UNCERTAIN_MSG := NULL ])
})


disconnectDB(db)
