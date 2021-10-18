################################################################################
## Test cases for the function getSubjData
##
## Expected output data is located in: 041_expected_getSubjData.xls
## Input list of findings is also located in this Excel sheet
##
################################################################################

# Test data covers these situations:
#
# Extract finidngs data
# Extract non-findings data
# Include subject and pool level findings
# With and without specification of columns to include

#### Prepare for test execution
rootPath <-  '../data/'

getExpected <- function(sheetName, sortKey) {
  data.table::setkeyv(data.table::as.data.table(
    readxl::read_xls(normalizePath(paste0(rootPath,
                                          '041_expected_getSubjData.xls')),
                     sheet=sheetName)), sortKey)
}

getInput <- function(sheetName) {
  data.table::as.data.table(
    readxl::read_xls(normalizePath(paste0(rootPath,
                                          '041_expected_getSubjData.xls')),
                     sheet=sheetName))
}

db <- initEnvironment(dbType='sqlite',
                      dbPath=normalizePath(paste0(rootPath, 'test_db.db')))

animalsList <- getInput('input_animalsListIn')

test_that('01 - Extract findngs data',
{
  # 1 - No column list specified
  expect_equal(data.table::setkeyv(getSubjData(db,
                                               domain = 'fw',
                                               animalList = animalsList),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect01',
                           c('STUDYID','FWSEQ'))[,`:=` (USUBJID = ifelse(is.na(USUBJID),'',USUBJID),
                                                        POOLID = ifelse(is.na(POOLID),'',POOLID),
                                                        FWGRPID = ifelse(is.na(FWGRPID),'',FWGRPID),
                                                        FWSTAT = ifelse(is.na(FWSTAT),'',FWSTAT),
                                                        FWREASND = ifelse(is.na(FWREASND),'',FWREASND),
                                                        FWEXCLFL = ifelse(is.na(FWEXCLFL),'',FWEXCLFL),
                                                        FWREASEX = ifelse(is.na(FWREASEX),'',FWREASEX))])


  # 2 - Column list specified
  expect_equal(data.table::setkeyv(getSubjData(db,
                                               domain = 'fw',
                                               animalList = animalsList,
                                               colList = c('FWTESTCD','FWSTRESN','FWSTRESU')),
                                   c('STUDYID','FWSEQ')),
               getExpected('expect02',
                           c('STUDYID','FWSEQ'))[,`:=` (USUBJID = ifelse(is.na(USUBJID),'',USUBJID),
                                                        POOLID = ifelse(is.na(POOLID),'',POOLID))])

})


test_that('02 - Extract non-findngs data',
          {
            expect_equal(data.table::setkeyv(getSubjData(db,
                                                         domain = 'DS',
                                                         animalList = animalsList,
                                                         colList = c('dsdecod','dsstdtc')),
                                             c('STUDYID','DSSEQ')),
                         getExpected('expect03',
                                     c('STUDYID','DSSEQ')))

          })

disconnectDB(db)
