################################################################################
## Test cases for the function getSubjSex
##
## Expected output data is located in: 032_expected_getSubjSex.xls
## Input list of animals is also located in this Excel sheet
##
################################################################################

# Test data covers these situations:
#
# Valid SEX values
# Invalid SEX CT value
# Missing SEX value
# Without and with given filter criteria

# rootPath <- '//FSDKHQ001/dep402$/000-4284/DATA MANAGEMENT/@Data Science/BioCelerate/sendigR/tests/data/'

#### Prepare for test execution
rootPath <-  '../data/'

getExpected <- function(sheetName) {
  data.table::setkeyv(data.table::as.data.table(
    readxl::read_xls(normalizePath(paste0(rootPath,
                                          '032_expected_getSubjSex.xls')),
                     sheet=sheetName)), c('STUDYID','USUBJID'))
}

db <- initEnvironment(dbType='sqlite',
                      dbPath=normalizePath(paste0(rootPath, 'test_db.db')))

animalList <- getExpected('input_animalListIn')

test_that('01 - No filtering',
{
  animalListIn1 <- data.table::copy(animalList)
  data.table::setnames(animalListIn1, 'UNCERTAIN_MSG' ,'NOT_VALID_MSG')
  animalListIn2 <- data.table::copy(animalListIn1)[,NOT_VALID_MSG := NULL]

  # 1 - Report uncertainties
  expect_equal(data.table::setkeyv(getSubjSex(db,
                                             animalList = animalListIn1,
                                             noFilterReportUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])


  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSex(db,
                                             animalList = animalListIn2,
                                             noFilterReportUncertain = FALSE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])


  # 3 - Parameter inclUncertain have no influence of reporting of uncertainties
  #   a - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSex(db,
                                             animalList = animalListIn2,
                                             noFilterReportUncertain = FALSE,
                                             inclUncertain = TRUE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])
  #   b - Report uncertainties
  expect_equal(data.table::setkeyv(getSubjSex(db,
                                             animalList = animalListIn1,
                                             noFilterReportUncertain = TRUE,
                                             inclUncertain = FALSE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])
})

test_that('02 - Specified one filter value',
{
  animalListIn1 <- data.table::copy(animalList)
  animalListIn2 <- data.table::copy(animalListIn1)[,UNCERTAIN_MSG := NULL]

  # 1 - Report uncertainties
  expect_equal(data.table::setkeyv(getSubjSex(db,
                                             animalList = animalListIn1,
                                             sexFilter = 'F',
                                             inclUncertain = TRUE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect02')[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)])


  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSex(db,
                                             animalList = animalListIn2,
                                             sexFilter = 'F',
                                             inclUncertain = FALSE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect02')[!grepl('SEX:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])


  # 3 - Parameter noFilterReportUncertain have no influence of reporting of uncertainties
  #   a - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSex(db,
                                             animalList = animalListIn2,
                                             sexFilter = 'F',
                                             noFilterReportUncertain = TRUE,
                                             inclUncertain = FALSE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect02')[!grepl('SEX:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

  #   b - Report uncertainties
  expect_equal(data.table::setkeyv(getSubjSex(db,
                                             animalList = animalListIn1,
                                             sexFilter = 'F',
                                             noFilterReportUncertain = FALSE,
                                             inclUncertain = TRUE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect02')[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)])
})

test_that('03 - Specified multiple filter values',
{
  animalListIn1 <- data.table::copy(animalList)
  animalListIn2 <- data.table::copy(animalListIn1)[,UNCERTAIN_MSG := NULL]

  # 1 - Report uncertainties
  expect_equal(data.table::setkeyv(getSubjSex(db,
                                             animalList = animalListIn1,
                                             sexFilter = c('F', 'M'),
                                             inclUncertain = TRUE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect03')[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)])


  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSex(db,
                                             animalList = animalListIn2,
                                             sexFilter = list('F', 'M'),
                                             inclUncertain = FALSE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect03')[!grepl('SEX:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])



})


disconnectDB(db)
