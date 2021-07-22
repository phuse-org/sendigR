################################################################################
## Test cases for the function getStudiesSTSTDTC.
##
## Expected data is located in: 021_expected_getStudiesSTSTDTC.xls
################################################################################

#### Prepare for test execution
rootPath <-  '../data/'

getExpected <- function(sheetName) {
  data.table::setkey(data.table::as.data.table(
        readxl::read_xls(normalizePath(paste0(rootPath,
                                              '021_expected_getStudiesSTSTDTC.xls')),
                         sheet=sheetName)), STUDYID)
}

db <- initEnvironment(dbType='sqlite',
                      dbPath=normalizePath(paste0(rootPath, 'test_db.db')))

#### Test cases 0x - execution without any input set of studies

test_that('01 - No input study list, no filtering',
  {
    # 1a - Report uncertainties
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      noFilterReportUncertain = TRUE),
                                    STUDYID),
                 getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])

    # 1b - verify noFilterReportUncertain is TRUE as default
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db),
                                    STUDYID),
                 getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])


    # 2 - Do not report uncertainties
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      noFilterReportUncertain = FALSE),
                                    STUDYID),
                 getExpected('expect01')[,NOT_VALID_MSG := NULL ])


    # 3 - Parameter inclUncertain have no influence of reporting of uncertainties
    #   a - Do not report uncertainties
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      noFilterReportUncertain = FALSE,
                                                      inclUncertain = TRUE),
                                    STUDYID),
                 getExpected('expect01')[,NOT_VALID_MSG := NULL ])
    #   b - Report uncertainties
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      noFilterReportUncertain = TRUE,
                                                      inclUncertain = FALSE),
                                    STUDYID),
                 getExpected('expect01')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG)])
  })

test_that('02 - No input study list, start day filter, open end',
  {
    # 1a - Full date, do not include uncertain rows
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      fromDTC = '2014-08-01',
                                                      inclUncertain = FALSE),
                                    STUDYID),
                 getExpected('expect02_1')[is.na(UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL])

    # 1b - Verify inclUncertain is FALSE as default
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      fromDTC = '2014-08-01'),
                                    STUDYID),
                 getExpected('expect02_1')[is.na(UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL])

    # 2 - Include uncertain rows
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      fromDTC = '2014-08-01',
                                                      inclUncertain = TRUE),
                                    STUDYID),
                 getExpected('expect02_1'))

    # 3 - Partial date, month
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      fromDTC = '2014-07',
                                                      inclUncertain = FALSE),
                                    STUDYID),
                 getExpected('expect02_3'))

    # 4 - Partial date, year
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      fromDTC = '2016',
                                                      inclUncertain = FALSE),
                                    STUDYID),
                 getExpected('expect02_4'))

    # 5 - Parameter noFilterReportUncertain have no influence of inclusion of unceratin rows
    #   a - Do not include uncertain rows
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      fromDTC = '2014-08-01',
                                                      inclUncertain = FALSE,
                                                      noFilterReportUncertain = TRUE),
                                    STUDYID),
                 getExpected('expect02_1')[is.na(UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL])
    #   b - Include uncertain rows
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      fromDTC = '2014-08-01',
                                                      inclUncertain = TRUE,
                                                      noFilterReportUncertain = FALSE),
                                    STUDYID),
                 getExpected('expect02_1'))
  })

test_that('03 - No input study list, end day filter, open start',
  {
    # 1 - Full date
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      toDTC = '2015-12-31',
                                                      inclUncertain = FALSE),
                                    STUDYID),
                 getExpected('expect03_1'))

    # 2 - Partial date, month
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      toDTC = '2014-07',
                                                      inclUncertain = FALSE),
                                    STUDYID),
                 getExpected('expect03_2'))

    # 2 - Partial date, year
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      toDTC = '2014',
                                                      inclUncertain = FALSE),
                                    STUDYID),
                 getExpected('expect03_1'))
  })

test_that('04 - No input study list, start and end day filters',
  {
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      fromDTC = '2010',
                                                      toDTC = '2014',
                                                      inclUncertain = FALSE),
                                    STUDYID),
                 getExpected('expect04'))
  })


#### Test cases 1x - execution with an input set of studies

# Prepare study lists for input
studyListIn <- data.table::data.table(STUDYID = c('CJ16050' , 'Study ID',	'GLP003'  , 'Nimort-01', 'PDS2014' , 'PC201708'),
                                      SDESIGN = c('PARALLEL', 'PARALLEL', 'PARALLEL', 'PARALLEL' , 'PARALLEL', 'PARALLEL'))

studyListInMsg <- data.table::data.table(STUDYID =
                                           c('CJ16050'           , 'Study ID', 'GLP003'  , 'Nimort-01', 'PDS2014' ,	'PC201708'),
                                         SDESIGN =
                                           c('PARALLEL'          , 'PARALLEL', 'PARALLEL', 'PARALLEL' , 'PARALLEL', 'PARALLEL'),
                                         NOT_VALID_MSG =
                                           c('SDESIGN: A message', NA        , NA        , NA         , NA        , 'SDESIGN: Another message'))

studyListInUncertain <- data.table::data.table(STUDYID =
                                                 c('CJ16050'           , 'Study ID', 'GLP003'  , 'Nimort-01', 'PDS2014' ,	'PC201708'),
                                               SDESIGN =
                                                 c('PARALLEL'          , 'PARALLEL', 'PARALLEL', 'PARALLEL' , 'PARALLEL', 'PARALLEL'),
                                               UNCERTAIN_MSG =
                                                 c('SDESIGN: A message', NA         , NA       , NA         , NA        , 'SDESIGN: Another message'))

studyListInMsgUncertain <- data.table::data.table(STUDYID =
                                                 c('CJ16050'           , 'Study ID', 'GLP003'  , 'Nimort-01', 'PDS2014'      , 'PC201708'),
                                               SDESIGN =
                                                 c('PARALLEL'          , 'PARALLEL', 'PARALLEL', 'PARALLEL' , 'PARALLEL'     , 'PARALLEL'),
                                               NOT_VALID_MSG =
                                                 c(NA                  , NA        , NA        , NA         , 'TEST: Message', NA),
                                               UNCERTAIN_MSG =
                                                 c('SDESIGN: A message', NA         , NA       , NA         , NA             , 'SDESIGN: Another message'))


# Execute test cases

test_that('11 - Input study list, no filters',
  {
    # 1 - Do not report uncertainties, input contains no not_valid_msg col and no uncertain rows
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      studyList = studyListIn,
                                                      noFilterReportUncertain = FALSE),
                                    STUDYID),
                 getExpected('expect11_1')[,NOT_VALID_MSG := NULL])

    # 2 - Report uncertainties, input contains no not_valid_msg col and no uncertain rows
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      studyList = studyListIn,
                                                      noFilterReportUncertain = TRUE),
                                    STUDYID),
                 getExpected('expect11_1')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG) ])

    # 3 - Report uncertainties, input contains a non-empty not_valid_msg col and no uncertain rows
    expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                      studyList = studyListInMsg,
                                                      noFilterReportUncertain = TRUE),
                                    STUDYID),
                 getExpected('expect11_3')[,NOT_VALID_MSG := as.character(NOT_VALID_MSG) ])

    # TODO: Decide if NOT_VALID_MSG in input shall be kept (as currently) or dropped when
    #       function is called with noFilterReportUncertain = FALSE.
    #       Or maybe fail in check of input parameters
    # 4 - Do not report uncertainties, input contains a non-empty not_valid_msg col an no uncertain rows
    # expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
    #                                                   studyList = studyListInMsg,
    #                                                   noFilterReportUncertain = FALSE), STUDYID),
    #              getExpected('expect11_3')[,NOT_VALID_MSG := NULL ])

    # TODO: Fix error: keep UNCERTAIN_MSAG and add NOT_VALID_MSG if relevant
    #
    # # 5 - Report uncertainties, input contains no not_valid_msg col and some uncertain rows
    # expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
    #                                                   studyList = studyListInUncertain,
    #                                                   noFilterReportUncertain = TRUE),
    #                                 STUDYID),
    #              getExpected('expect11_5')[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)])
    #
    # # 6 - Do not report uncertainties, input contains no not_valid_msg col and some uncertain rows
    # expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
    #                                                   studyList = studyListIn,
    #                                                   noFilterReportUncertain = FALSE),
    #                                 STUDYID),
    #              getExpected('expect11_5'))


    # TODO: Finalize expected data when decision in step 4 has been taken and implemented and bug in step 5 and 6 has been fixed.
    #
    # # 7 - Report uncertainties, input contains non-empty not_valid_msg col and some uncertain rows
    # expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
    #                                                   studyList = studyListInUncertain,
    #                                                   noFilterReportUncertain = TRUE),
    #                                 STUDYID),
    #              getExpected('expect11_7')[,UNCERTAIN_MSG := as.character(UNCERTAIN_MSG)])
    #
    # # 8 - Do not report uncertainties, input contains non-empty  not_valid_msg col and some uncertain rows
    # expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
    #                                                   studyList = studyListIn,
    #                                                   noFilterReportUncertain = FALSE),
    #                                 STUDYID),
    #              getExpected('expect11_7'))

  })

  test_that('12 - Input study list, filters',
    {
      # 1 - Include uncertain rows, input contains no not_valid_msg col and no uncertain rows
      expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                        studyList = studyListIn,
                                                        fromDTC = '2014-08-01',
                                                        inclUncertain = TRUE),
                                      STUDYID),
                   getExpected('expect12_1'))

      # 2 - Do not include uncertain rows, input contains no not_valid_msg col and no uncertain rows
      expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                        studyList = studyListIn,
                                                        fromDTC = '2014-08-01'),
                                      STUDYID),
                   getExpected('expect12_1')[is.na(UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

      # 3 - Include uncertain rows, input contains no not_valid_msg col and some uncertain rows
      expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                        studyList = studyListInUncertain,
                                                        fromDTC = '2014-08-01',
                                                        inclUncertain = TRUE),
                                      STUDYID),
                   getExpected('expect12_3'))

      # TODO: Decide how to act, when executed with inclUncertain = FALSE and input set contains uncertain rows
      #   - excluded these and drop col UNCERTAIN_MSG ?
      #   - keep uncertain rows from previuos steps....?
      # # 4 - Do not include uncertain rows, input contains no not_valid_msg col and some uncertain rows
      # expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
      #                                                   studyList = studyListInUncertain,
      #                                                   fromDTC = '2014-08-01',
      #                                                   inclUncertain = FALSE),
      #                                 STUDYID),
      #              getExpected('expect12_3')[is.na(UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

      # 5 - Include uncertain rows, input contains non-empty not_valid_msg col and no uncertain rows
      expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                        studyList = studyListInMsg,
                                                        fromDTC = '2014-08-01',
                                                        inclUncertain = TRUE),
                                      STUDYID),
                   getExpected('expect12_5'))

      # 6 - Do not include uncertain rows, input contains non-empty not_valid_msg col and no uncertain rows
      expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                        studyList = studyListInMsg,
                                                        fromDTC = '2014-08-01'),
                                      STUDYID),
                   getExpected('expect12_5')[is.na(UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

      # 7 - Include uncertain rows, input contains non-empty not_valid_msg col and some uncertain rows
      expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
                                                        studyList = studyListInMsgUncertain,
                                                        fromDTC = '2010',
                                                        inclUncertain = TRUE),
                                      STUDYID),
                   getExpected('expect12_7'))

      # TODO: Same as step 4
      # # 8 - Do not include uncertain rows, input contains non-empty not_valid_msg col and som uncertain rows
      # expect_equal(data.table::setkey(getStudiesSTSTDTC(db,
      #                                                   studyList = studyListInMsgUncertain,
      #                                                   fromDTC = '2010'),
      #                                 STUDYID),
      #              getExpected('expect12_7')[is.na(UNCERTAIN_MSG)][,UNCERTAIN_MSG := NULL ])

    })


#### Test sets 9x - error handling

test_that('91 - Validation of input parameters',
          {
            # Invalid filter dates
            expect_error(getStudiesSTSTDTC(db, fromDTC = '214'),
                         regexp = 'not a valid .* date')
            expect_error(getStudiesSTSTDTC(db , toDTC = '214'),
                         regexp = 'not a valid .* date')
            expect_error(getStudiesSTSTDTC(db, fromDTC = '2014', toDTC = '20016'),
                         regexp = 'not a valid .* date')
          })

disconnectDB(db)
