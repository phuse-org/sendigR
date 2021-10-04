################################################################################
## Test cases for the function getSubjSpeciesStrain
##
## Expected output data is located in: 034_expected_getSubjSpeciesStrain.xls
## Input list of animals is also located in this Excel sheet
##
################################################################################

# Test data covers these situations:
#
# No SPECIES/STRAIN input filter criteria
# One SPECIES & no STRAIN input filter criteria
# Multiple SPECIES & no STRAIN input filter criteria
# One SPECIES/one STRAIN input filter criteria
# One SPECIES & mutliple STRAIN input filter criteria
# Mutiple SPECIES & one/mutliple STRAIN input filter criteria
# Valid SPECIES/STRAIN values (in TS, TX and DM)
# Invalid SPECIES/STRAIN CT value (in TS, TX and DM)
# Missing SPECIES/STRAIN value (in TS, TX and DM)
# Mismatch in SPECIES/STRAIN values between TS, TX and DM
# Without and with given filter criteria
# exclusively parameter values true and false respectively
# Inclusion/exclusion of uncertain values

# rootPath <- '//FSDKHQ001/dep402$/000-4284/DATA MANAGEMENT/@Data Science/BioCelerate/sendigR/tests/data/'

#### Prepare for test execution
rootPath <-  '../data/'

getExpected <- function(sheetName) {
  data.table::setkeyv(data.table::as.data.table(
    readxl::read_xls(normalizePath(paste0(rootPath,
                                          '034_expected_getSubjSpeciesStrain.xls')),
                     sheet=sheetName))[,NO_AGE_MSG := as.character(NO_AGE_MSG)],
                      c('STUDYID','USUBJID'))
}

db <- initEnvironment(dbType='sqlite',
                      dbPath=normalizePath(paste0(rootPath, 'test_db.db')))

# Import set of animals used as input to getSubjSpeciesStrain
animalListIn1 <- getExpected('input_animalListIn')
animalListIn2 <- data.table::copy(animalListIn1)[,UNCERTAIN_MSG := NULL]

test_that('01 - No filtering',
{
  animalListIn0 <- data.table::copy(animalListIn1)
  data.table::setnames(animalListIn0, 'UNCERTAIN_MSG' ,'NOT_VALID_MSG')

  # 1 - Report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn0,
                                                noFilterReportUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])


  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn2,
                                                noFilterReportUncertain = FALSE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])


  # 3 - Parameter inclUncertain have no influence of reporting of uncertainties
  #   a - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn2,
                                                noFilterReportUncertain = FALSE,
                                                inclUncertain = TRUE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])
  #   b - Report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn0,
                                                noFilterReportUncertain = TRUE,
                                                inclUncertain = FALSE),
                                  c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])


  # 4 - Parameter exclusively has no influence of returned list off animals
  #   a - FALSE
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn2,
                                                exclusively = FALSE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])

  #   b - TRUE
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn2,
                                                exclusively = TRUE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect01')[,NOT_VALID_MSG := NULL ])

})

test_that('02 - Specified one species filter value',
{
  # 1 - Report uncertainties, Exclusively is FALSE
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn1,
                                                speciesFilter = 'RAT',
                                                exclusively = FALSE,
                                                inclUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02'))

  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn2,
                                                speciesFilter = 'RAT',
                                                exclusively = FALSE,
                                                inclUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])


  # 3 - Parameter noFilterReportUncertain has no influence of reporting of uncertainties
  #   a - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn2,
                                                speciesFilter = 'RAT',
                                                exclusively = FALSE,
                                                inclUncertain = FALSE,
                                                noFilterReportUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

  #   b - Report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn1,
                                                speciesFilter = 'RAT',
                                                exclusively = FALSE,
                                                inclUncertain = TRUE,
                                                noFilterReportUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02'))

  # 4 - exclusively is TRUE, report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn1,
                                                speciesFilter = 'RAT',
                                                exclusively = TRUE,
                                                inclUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02_4'))

  # 5 - Do not  report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                        animalList = animalListIn2,
                                                        speciesFilter = 'RAT',
                                                        exclusively = TRUE,
                                                        inclUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect02_4')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])
})

test_that('03 - Specified multiple species filter values',
{

  # 1 - Report uncertainties, exclusively is FALSE
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn1,
                                                speciesFilter = c('RAT','monkey'),
                                                exclusively = FALSE,
                                                inclUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect03'))

  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn2,
                                                speciesFilter = c('RAT','monkey'),
                                                exclusively = FALSE,
                                                inclUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect03')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

  # 3 - exclusively is TRUE, report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                animalList = animalListIn1,
                                                speciesFilter = c('RAT','monkey'),
                                                exclusively = TRUE,
                                                inclUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect03_3'))

  # 4 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                        animalList = animalListIn2,
                                                        speciesFilter = c('RAT','monkey'),
                                                        exclusively = TRUE,
                                                        inclUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect03_3')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

})

test_that('04 - Specified one species & one strain filter value',
{
  # 1 - Report uncertainties, Exclusively is FALSE
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                        animalList = animalListIn1,
                                                        speciesFilter = 'RAT',
                                                        strainFilter = 'wistar',
                                                        exclusively = FALSE,
                                                        inclUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect04'))

  # 2 - Do not report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                        animalList = animalListIn2,
                                                        speciesFilter = 'RAT',
                                                        strainFilter = 'WISTAR',
                                                        exclusively = FALSE,
                                                        inclUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect04')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

  # 3 - exclusively is TRUE, report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                        animalList = animalListIn1,
                                                        speciesFilter = 'RAT',
                                                        strainFilter = 'WISTAR',
                                                        exclusively = TRUE,
                                                        inclUncertain = TRUE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect04_3'))

  # 4 - Do report uncertainties
  expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                        animalList = animalListIn2,
                                                        speciesFilter = 'RAT',
                                                        strainFilter = 'WISTAR',
                                                        exclusively = TRUE,
                                                        inclUncertain = FALSE),
                                   c('STUDYID','USUBJID')),
               getExpected('expect04_3')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])
})

test_that('05 - Specified one species & mutiple strain filter values',
          {
            # 1 - Report uncertainties, Exclusively is FALSE
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn1,
                                                                  speciesFilter = 'RAT',
                                                                  strainFilter = c('WISTAR','SPRAGUE-DAWLEY'),
                                                                  exclusively = FALSE,
                                                                  inclUncertain = TRUE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect05'))

            # 2 - Do not report uncertainties
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn2,
                                                                  speciesFilter = 'RAT',
                                                                  strainFilter = c('WISTAR','SPRAGUE-DAWLEY'),
                                                                  exclusively = FALSE,
                                                                  inclUncertain = FALSE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect05')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

            # 3 - exclusively is TRUE, report uncertainties
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn1,
                                                                  speciesFilter = 'RAT',
                                                                  strainFilter = c('WISTAR','SPRAGUE-DAWLEY'),
                                                                  exclusively = TRUE,
                                                                  inclUncertain = TRUE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect05_3'))

            # 4 - Do not report uncertainties
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn2,
                                                                  speciesFilter = 'RAT',
                                                                  strainFilter = c('WISTAR','SPRAGUE-DAWLEY'),
                                                                  exclusively = TRUE,
                                                                  inclUncertain = FALSE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect05_3')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])
          })

test_that('06 - Specified multiple species & one strain filter values',
          {
            # 1 - Report uncertainties, Exclusively is FALSE
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn1,
                                                                  speciesFilter = c('RAT','DOG','MONKEY'),
                                                                  strainFilter = c('RAT:WISTAR','DOG: BEAGLE', 'MONKEY:  CYNOMOLGUS'),
                                                                  exclusively = FALSE,
                                                                  inclUncertain = TRUE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect06'))

            # 2 - Do not report uncertainties
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn2,
                                                                  speciesFilter = c('RAT','DOG','MONKEY'),
                                                                  strainFilter = c('RAT:WISTAR','DOG: BEAGLE', 'MONKEY:  CYNOMOLGUS'),
                                                                  exclusively = FALSE,
                                                                  inclUncertain = FALSE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect06')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

            # 3 - exclusively is TRUE, report uncertainties
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn1,
                                                                  speciesFilter = c('RAT','DOG','MONKEY'),
                                                                  strainFilter = c('RAT:WISTAR','DOG: BEAGLE', 'MONKEY:  CYNOMOLGUS'),
                                                                  exclusively = TRUE,
                                                                  inclUncertain = TRUE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect06_3'))

            # 4 - Do not report uncertainties
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn2,
                                                                  speciesFilter = c('RAT','DOG','MONKEY'),
                                                                  strainFilter = c('RAT:WISTAR','DOG: BEAGLE', 'MONKEY:  CYNOMOLGUS'),
                                                                  exclusively = TRUE,
                                                                  inclUncertain = FALSE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect06_3')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])
          })

test_that('07 - Specified multiple species & mutiple strain filter values',
          {
            # 1 - Report uncertainties, Exclusively is FALSE
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn1,
                                                                  speciesFilter = c('RAT','DOG','MONKEY'),
                                                                  strainFilter = c('RAT:WISTAR','RAT:SPRAGUE-DAWLEY','DOG:BEAGLE', 'MONKEY:CYNOMOLGUS'),
                                                                  exclusively = FALSE,
                                                                  inclUncertain = TRUE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect07'))

            # 2 - Do not report uncertainties
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn2,
                                                                  speciesFilter = c('RAT','DOG','MONKEY'),
                                                                  strainFilter = c('RAT:WISTAR','RAT:SPRAGUE-DAWLEY','DOG:BEAGLE', 'MONKEY:CYNOMOLGUS'),
                                                                  exclusively = FALSE,
                                                                  inclUncertain = FALSE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect07')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

            # 3 - exclusively is TRUE, report uncertainties
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn1,
                                                                  speciesFilter = c('RAT','DOG','MONKEY'),
                                                                  strainFilter = c('RAT:WISTAR','RAT:SPRAGUE-DAWLEY','DOG:BEAGLE', 'MONKEY:CYNOMOLGUS'),
                                                                  exclusively = TRUE,
                                                                  inclUncertain = TRUE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect07_3'))

            # 4 - Do not report uncertainties
            expect_equal(data.table::setkeyv(getSubjSpeciesStrain(db,
                                                                  animalList = animalListIn2,
                                                                  speciesFilter = c('RAT','DOG','MONKEY'),
                                                                  strainFilter = c('RAT:WISTAR','RAT:SPRAGUE-DAWLEY','DOG:BEAGLE', 'MONKEY:CYNOMOLGUS'),
                                                                  exclusively = TRUE,
                                                                  inclUncertain = FALSE),
                                             c('STUDYID','USUBJID')),
                         getExpected('expect07_3')[!grepl('SpeciesStrain:',UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])
          })

disconnectDB(db)
