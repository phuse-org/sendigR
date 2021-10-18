################################################################################
## Test cases for the function getFindingsSubjAge
##
## Expected output data is located in: 042_expected_getFindingsSubjAge.xls
## Input list of findings is also located in this Excel sheet
##
################################################################################

# Test data covers these situations:
#
# Valid --DTC, -DY, RFSTDTC and DM_AGEDAYS values
# Invalid or missing --DTC, -DY, RFSTDTC and DM_AGEDAYS values
# Not filter options
# Input date filter conditions for toAge and fromAge only
# Input date filter conditions for both toAge and fromAge
# All valid variants of age units given as filter condition
# Both subject and pool level findings included

# rootPath <- '//FSDKHQ001/dep402$/000-4284/DATA MANAGEMENT/@Data Science/BioCelerate/sendigR/tests/data/'

#### Prepare for test execution
rootPath <-  '../data/'

getExpected <- function(sheetName) {
  data.table::setkeyv(data.table::as.data.table(
    readxl::read_xls(normalizePath(paste0(rootPath,
                                          '042_expected_getFindingsSubjAge.xls')),
                     sheet=sheetName)), c('STUDYID','FWSEQ'))[,`:=` (POOLID = as.character(POOLID),
                                                                     FWDTC = as.character(FWDTC))]
}

getInput <- function(sheetName) {
  data.table::as.data.table(
    readxl::read_xls(normalizePath(paste0(rootPath,
                                          '042_expected_getFindingsSubjAge.xls')),
                     sheet=sheetName))[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)]
}

db <- initEnvironment(dbType='sqlite',
                      dbPath=normalizePath(paste0(rootPath, 'test_db.db')))

findingsList <- getInput('input_findingsListIn')[,POOLID := as.character(POOLID)]
animalsList <- getInput('input_animalsListIn')[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)]

test_that('01 - No filtering',
{
  findingsListIn1 <- data.table::copy(findingsList)
  data.table::setnames(findingsListIn1, 'UNCERTAIN_MSG', 'NOT_VALID_MSG')
  findingsListIn2 <- data.table::copy(findingsListIn1)[,NOT_VALID_MSG := NULL]

  animalsListIn1 <- data.table::copy(animalsList)
  data.table::setnames(animalsListIn1, 'UNCERTAIN_MSG', 'NOT_VALID_MSG')
  animalsListIn2 <- data.table::copy(animalsListIn1)[,NOT_VALID_MSG := NULL]

  # 1 - Report uncertainties
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn1,
                                                      animalList = animalsListIn1,
                                                      noFilterReportUncertain = TRUE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])


  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn2,
                                                      animalList = animalsListIn2,
                                                      noFilterReportUncertain = FALSE),
                                  c('STUDYID','FWSEQ')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])


  # 3 - Parameter inclUncertain have no influence of reporting of uncertainties
  #   a - Do not report uncertainties
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn2,
                                                      animalList = animalsListIn2,
                                                        noFilterReportUncertain = FALSE,
                                                    inclUncertain = TRUE),
                                  c('STUDYID','FWSEQ')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])
  #   b - Report uncertainties
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn1,
                                                      animalList = animalsListIn1,
                                                      noFilterReportUncertain = TRUE,
                                                      inclUncertain = FALSE),
                                  c('STUDYID','FWSEQ')),
               getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])
})

test_that('02 - Specified filter value fromAge',
{
  findingsListIn1 <- data.table::copy(findingsList)
  findingsListIn2 <- data.table::copy(findingsListIn1)[,UNCERTAIN_MSG := NULL]

  animalsListIn1 <- data.table::copy(animalsList)
  animalsListIn2 <- data.table::copy(animalsListIn1)[,UNCERTAIN_MSG := NULL]

  # 1 - Condition specified in days - report uncertainties
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn1,
                                                      animalList = animalsListIn1,
                                                      fromAge = '65d',
                                                      inclUncertain = TRUE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect02d')[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)])


  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn2,
                                                      animalList = animalsListIn2,
                                                      fromAge = '65 D',
                                                      inclUncertain = FALSE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect02d')[!grepl(':',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])


  # 3 - Parameter noFilterReportUncertain have no influence of reporting of uncertainties
  #   a - Do not report uncertainties
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn2,
                                                      animalList = animalsListIn2,
                                                      fromAge = '65Days',
                                                      noFilterReportUncertain = TRUE,
                                                      inclUncertain = FALSE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect02d')[!grepl(':',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

  #   b - Report uncertainties
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn1,
                                                      animalList = animalsListIn1,
                                                      fromAge = '  65  days  ',
                                                      noFilterReportUncertain = FALSE,
                                                      inclUncertain = TRUE),
                                  c('STUDYID','FWSEQ')),
               getExpected('expect02d')[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)])

  # 4 - Condition specified in weeks
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn2,
                                                      animalList = animalsListIn2,
                                                      fromAge = '3w',
                                                      inclUncertain = FALSE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect02w'))

  # 4 - Condition specified in months
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn2,
                                                      animalList = animalsListIn2,
                                                      fromAge = '12m',
                                                      inclUncertain = FALSE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect02m'))

  # 4 - Condition specified in years
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn2,
                                                      animalList = animalsListIn2,
                                                      fromAge = '1y',
                                                      inclUncertain = FALSE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect02y'))

})



test_that('03 - Specified filter value toAge',
{

  findingsListIn <- data.table::copy(findingsList)[,UNCERTAIN_MSG := NULL]

  animalsListIn <- data.table::copy(animalsList)[,UNCERTAIN_MSG := NULL]

  # 1 - Condition specified in days
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn,
                                                      animalList = animalsListIn,
                                                      toAge = '90 day',
                                                      inclUncertain = FALSE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect03d'))


  # 2 - Condition specified in weeks
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn,
                                                      animalList = animalsListIn,
                                                      toAge = '3 week',
                                                      inclUncertain = FALSE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect03w'))


  # 3 - Condition specified in months
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn,
                                                      animalList = animalsListIn,
                                                      toAge = '1 month',
                                                      inclUncertain = FALSE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect03m'))

  # 4 - Condition specified in years
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn,
                                                      animalList = animalsListIn,
                                                      toAge = '1 year',
                                                      inclUncertain = FALSE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect03y'))

})



test_that('04 - Specified filter values fromAge and  toAge',
{

  findingsListIn <- data.table::copy(findingsList)[,UNCERTAIN_MSG := NULL]

  animalsListIn <- data.table::copy(animalsList)[,UNCERTAIN_MSG := NULL]

  # 1 - Conditions specified in same unit
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn,
                                                      animalList = animalsListIn,
                                                      fromAge = '3 weeks',
                                                      toAge = '10 weeks',
                                                      inclUncertain = FALSE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect04_1'))


  # 2 - Conditions specified in different units
  expect_equal(data.table::setkeyv(getFindingsSubjAge(db,
                                                      findings = findingsListIn,
                                                      animalList = animalsListIn,
                                                      fromAge = '3 months',
                                                      toAge = '2 years',
                                                      inclUncertain = FALSE),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect04_2'))
})


disconnectDB(db)
