################################################################################
## Test cases for the function getFindingsPhase
##
## Expected output data is located in: 043_expected_getFindingsPhase.xls
## Input list of findings is also located in this Excel sheet
##
################################################################################

# Test data covers these situations:
#
# Known EPOCH values
# Unknown and missing EPOCH value
# Valid --DTC, SESTDTC, SEENDTC values
# Invalid and missing  --DTC, SESTDTC, SEENDTC values
# Missing SE rows
# Overlapping subject element periods

#### Prepare for test execution
rootPath <-  '../data/'

getExpected <- function(sheetName) {
  data.table::setkeyv(data.table::as.data.table(
    readxl::read_xls(normalizePath(paste0(rootPath,
                                          '043_expected_getFindingsPhase.xls')),
                     sheet=sheetName)), c('STUDYID','USUBJID','BWSEQ'))
}

db <- initEnvironment(dbType='sqlite',
                      dbPath=normalizePath(paste0(rootPath, 'test_db.db')))

findingsList <- getExpected('input_findingsListIn')

test_that('01 - No filtering',
{
  findingsListIn1 <- data.table::copy(findingsList)
  data.table::setnames(findingsListIn1, 'UNCERTAIN_MSG', 'NOT_VALID_MSG')
  findingsListIn2 <- data.table::copy(findingsListIn1)[,NOT_VALID_MSG := NULL]

  # 1 - Report uncertainties
  expect_equal(data.table::setkeyv(getFindingsPhase(db,
                                                    findings = findingsListIn1,
                                                    noFilterReportUncertain = TRUE),
                                   c('STUDYID','USUBJID','BWSEQ')),
               getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])


  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getFindingsPhase(db,
                                                    findings = findingsListIn2,
                                                    noFilterReportUncertain = FALSE),
                                  c('STUDYID','USUBJID','BWSEQ')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])


  # 3 - Parameter inclUncertain have no influence of reporting of uncertainties
  #   a - Do not report uncertainties
  expect_equal(data.table::setkeyv(getFindingsPhase(db,
                                                    findings = findingsListIn2,
                                                    noFilterReportUncertain = FALSE,
                                                    inclUncertain = TRUE),
                                  c('STUDYID','USUBJID','BWSEQ')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])
  #   b - Report uncertainties
  expect_equal(data.table::setkeyv(getFindingsPhase(db,
                                                    findings = findingsListIn1,
                                                    noFilterReportUncertain = TRUE,
                                                    inclUncertain = FALSE),
                                  c('STUDYID','USUBJID','BWSEQ')),
               getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])
})

test_that('02 - Specified one filter value',
{
  findingsListIn1 <- data.table::copy(findingsList)
  findingsListIn2 <- data.table::copy(findingsListIn1)[,UNCERTAIN_MSG := NULL]

  # 1 - Report uncertainties
  expect_equal(data.table::setkeyv(getFindingsPhase(db,
                                                    findings = findingsListIn1,
                                                    phaseFilter = 'treatment',
                                                    inclUncertain = TRUE),
                                  c('STUDYID','USUBJID', 'BWSEQ')),
               getExpected('expect02')[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)])


  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getFindingsPhase(db,
                                                    findings = findingsListIn2,
                                                    phaseFilter = 'treatment',
                                                    inclUncertain = FALSE),
                                  c('STUDYID','USUBJID', 'BWSEQ')),
               getExpected('expect02')[!grepl('FindingsPhase:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])


  # 3 - Parameter noFilterReportUncertain have no influence of reporting of uncertainties
  #   a - Do not report uncertainties
  expect_equal(data.table::setkeyv(getFindingsPhase(db,
                                                    findings = findingsListIn2,
                                                    phaseFilter = 'treatment',
                                                    noFilterReportUncertain = TRUE,
                                                    inclUncertain = FALSE),
                                  c('STUDYID','USUBJID', 'BWSEQ')),
               getExpected('expect02')[!grepl('FindingsPhase:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

  #   b - Report uncertainties
  expect_equal(data.table::setkeyv(getFindingsPhase(db,
                                                    findings = findingsListIn1,
                                                    phaseFilter = 'treatment',
                                                    noFilterReportUncertain = FALSE,
                                                    inclUncertain = TRUE),
                                  c('STUDYID','USUBJID', 'BWSEQ')),
               getExpected('expect02')[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)])
})



test_that('03 - Specified multiple filter values',
{
  findingsListIn1 <- data.table::copy(findingsList)
  findingsListIn2 <- data.table::copy(findingsListIn1)[,UNCERTAIN_MSG := NULL]

  # 1 - Report uncertainties
  expect_equal(data.table::setkeyv(getFindingsPhase(db,
                                                    findings = findingsListIn1,
                                                    phaseFilter = c('treatment','recovery'),
                                                    inclUncertain = TRUE),
                                   c('STUDYID','USUBJID', 'BWSEQ')),
               getExpected('expect03')[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)])


  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getFindingsPhase(db,
                                                    findings = findingsListIn2,
                                                    phaseFilter = list('treatment','recovery'),
                                                    inclUncertain = FALSE),
                                   c('STUDYID','USUBJID', 'BWSEQ')),
               getExpected('expect03')[!grepl('FindingsPhase:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])




})


disconnectDB(db)
