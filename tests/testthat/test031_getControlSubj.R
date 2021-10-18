################################################################################
## Test cases for the function getControlSubj.
##
## Expected data is located in: 031_expected_getControlSubj.xls
################################################################################

# Test data covers these situations:
#
# Age in days calculations (use study  'Nimort-01'):
#  - from BRTHDTC (subj 81, 75 (AGEU/AGETXT also filled in))
#  - from AGE (all kinds of units)
#     - DAYS (subj 70)
#     - WEEKS (subj 73)
#     - MONTHS (subj 95)
#     - YEARS (subj 56)
#  - From AGETXT (all not mentioned subjects)
#  - not possible to calculate due to
#     - missing BRTHDTC, AGE and AGTEXT (subj 61)
#     - invalid BRTHDTC format (subj 74)
#     - missing or invalid RFSTDTC (when using BRTDTC) (subj 76 (miss) , 58 (invalid))
#     - missing or invalid AGEU (when using AGE or AGETXT) (subj 80, 59 (miss) and 79, 88 (invalid))
#     - invalid formatted AGETEXT (91)
#
# Identification of control group
#  - TX.TCNTRL parameter value included in
#     - negative stand alone list  (study 'CV01', setcd 1)
#     - combination of negative modifies and control nouns  (study 'CV01', setcd 1 & 'Study ID', 1)
#     - combination of positive modifies and control nouns (i.e. animal from these groups are not included in output) (study 'Study ID', setcd 3)
#  - Not possible to identify control group confidently due to
#     - missing TX.TCNTRL parameter (study 'CJUGSEND00')
#     - TX.TCNTRL value does not match any of the predefined lists of words for negative control (study 'Study ID', setcd 2)


#### Prepare for test execution
rootPath <-  '../data/'

getExpected <- function(sheetName) {
  data.table::setkeyv(data.table::as.data.table(
    readxl::read_xls(normalizePath(paste0(rootPath,
                                          '031_expected_getControlSubj.xls')),
                     sheet=sheetName)), c('STUDYID','USUBJID'))[,`:=` (RFSTDTC = ifelse(is.na(RFSTDTC),'',RFSTDTC),
                                                                       NO_AGE_MSG = as.character(NO_AGE_MSG))]
}

db <- initEnvironment(dbType='sqlite',
                      dbPath=normalizePath(paste0(rootPath, 'test_db.db')))

studyListFilter <- data.table::data.table(STUDYID = c('CJUGSEND00', 'Study ID',	'CV01'),
                                          SDESIGN = c('CROSSOVER' , 'PARALLEL' , 'PARALLEL'))

studyListAge <- data.table::data.table(STUDYID = c('Nimort-01'),
                                       SDESIGN = c('DOSE ESCALATION'))

test_that('01 - Filtering of animals',
{
  # 1 - Do not include uncertain rows
  expect_equal(data.table::setkeyv(getControlSubj(db,
                                                  studyList = studyListFilter,
                                                  inclUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect01')[is.na(UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL])

  # 2 - Include uncertain rows
  expect_equal(data.table::setkeyv(getControlSubj(db,
                                                  studyList = studyListFilter,
                                                  inclUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect01'))

})

test_that('02 - Calculation of age in days',
{
  expect_equal(data.table::setkeyv(getControlSubj(db,
                                                  studyList = studyListAge,
                                                  inclUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02'))

})

disconnectDB(db)
