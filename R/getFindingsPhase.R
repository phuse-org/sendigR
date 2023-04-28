################################################################################
## The function getFindingsPhase.
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2021-01-28   Bo Larsen             Initial version
################################################################################

#' Extract a set of findings for a specified study phase - or just add phase
#' for each animal.
#'
#' Returns a data table with the set of findings rows included in the
#' \code{findings} of the phase(s) specified in the \code{phaseFilter}.\cr
#' If the \code{phaseFilter} is empty (null, na or empty string), all rows from
#' \code{findings} are returned with the an additional PHASE column.
#'
#' The logic for the extraction is based on the subject elements and the trial
#' design domains - for each finding row:
#' \itemize{
#'    \item The related subject element is found in SE as the row where the
#'    value of domainDTC is within the interval from SESTDTC to SEENDTC
#'    \item The actual EPOCH is found in TA in the row matching the found
#'    element (via the ETCD value)
#'    \item The actual study phase is derived from the EPOCH value matching at
#'    set of text patterns
#' }
#' For pooled findings rows - i.e. POOLID is populated instead of USUBJID - the
#' phase is identified per animal included in the each pool and finding, and if all
#' identified phases are equal per pool and finding, the identified phase are
#' returned per pool and finding.
#'
#' The populated value of a phase is one of:
#' \itemize{
#'   \item 'Screening'\cr
#'      If TA.EPOCH fulfills one:
#'      \itemize{
#'        \item contains 'pre' followed by one of
#'              \['treat','trt','dos',test','study','exposure'\]
#'        \item contains one of
#'              \['acclimat','screen','baseline','allocat','random'\]
#'      }
#'   \item 'Recovery'\cr
#'      If TA.EPOCH doesn't fulfill the pattern for 'Screening' and fulfills one
#'      of:
#'      \itemize{
#'        \item contains 'recovery'
#'        \item contains 'post' followed by one of
#'              \['treat','trt','dos','test','study','exposure'\]
#'      }
#'   \item 'Treatment'\cr
#'      If TA.EPOCH doesn't fulfill the patterns for 'Screening' or 'Recovery'
#'      and fulfills both:
#'      \itemize{
#'        \item contains one of
#'              \['treat','trt','dos','test','exposure'\]
#'        \item does not contain any of
#'              \['off','non'|','free'|','holiday'\]
#'      }
#'   \item 'Uncertain'\cr
#'      If the TA.EPOCH is empty or does not fulfills any of the requirements
#'      described for the three phases above.
#' }
#'
#' If input parameter \code{inclUncertain=TRUE}, findings rows where the phase
#' cannot be confidently identified are included in the output set. These
#' uncertain situations are identified and reported (in column UNCERTAIN_MSG):
#' \itemize{
#'   \item One of the date/time values SESTDTC, SEENDTC or domainDTC is empty
#'   or contains an invalid ISO 8601 value
#'   \item The value of domainDTC is included in more then one SESTDTC/SEENDTC
#'   interval
#'   \item The EPOCH value does not match any of the patterns identifying the
#'   set of possible study phases.
#'   \item Different phases have been identified for individual subjects in a
#'   pool for a given finding
#' }
#' The same checks are performed and reported in column NOT_VALID_MSG if
#' \code{phaseFilter} is empty and \code{noFilterReportUncertain=TRUE}.
#'
#' @param dbToken Mandatory\cr
#'   Token for the open database connection (see \code{\link{initEnvironment}}).
#' @param findings Mandatory, data.table.\cr
#'  A data.table with the set of finding rows to process.\cr
#'  The table must include at least columns named
#'  \itemize{
#'     \item STUDYID
#'     \item USUBJID
#'     \item DOMAIN
#'     \item domainSEQ
#'     \item domainDTC
#'  }
#'  where domain is the name of the actual findings domain - e.g. LBSEQ and
#'  LBDTC
#' @param phaseFilter Optional, character.\cr
#'  The phase value criterion to be used for filtering of the list of animals.\cr
#'  It can be a single string, a vector or a list of multiple strings.
#' @param inclUncertain  Mandatory, boolean.\cr
#'  Only relevant if the \code{phaseFilter} is not empty.\cr
#'  Indicates whether finding rows for which the phase cannot be confidently
#'  identified shall be included or not in the output data table.
#' @param noFilterReportUncertain  Mandatory, boolean.\cr
#'  Only relevant if the \code{phaseFilter} is empty.\cr
#'  Indicates if the reason should be included if the phase cannot be
#'  confidently decided for an animal.
#'
#' @return The function returns a data.table with columns in this order:
#'   \itemize{
#'   \item All columns contained in the \code{findings} input table (original
#'   order except optional UNCERTAIN_MSG and NOT_VALID_MSG)
#'   \item PHASE          (character)
#'   \item UNCERTAIN_MSG  (character)\cr
#' Included when parameter \code{inclUncertain=TRUE}.\cr
#' In case the phase cannot be confidently matched during the
#' filtering of data, the column contains an indication of the reason.\cr
#' If any uncertainties have been identified for individual subjects included in
#' pools for pooled finding rows, all messages for subjects per pool/findings
#' are merged together and reported as one message per pool/finding.\cr
#' Is NA for rows where phase can be confidently matched.\cr
#' A non-empty UNCERTAIN_MSG value generated by this function is merged with
#' non-empty UNCERTAIN_MSG values which may exist in the input set of findings
#' specified in \code{findings} - separated by '|'.
#'   \item NOT_VALID_MSG (character)\cr
#' Included when parameter \code{noFilterReportUncertain=TRUE}.\cr
#' In case the phase cannot be confidently decided, the column
#' contains an indication of the reason.\cr
#' Is NA for rows where phase can be confidently decided.\cr
#' A non-empty NOT_VALID_MSG value generated by this function is merged with
#' non-empty NOT_VALID_MSG values which may exist in the input set of findings
#' \code{findings} - separated by '|'.
#'}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract LB rows for the Treatment phase - include uncertain rows
#' getFindingsPhase(dbToken, lb,
#'                  phaseFilter = 'Treatment',
#'                  inclUncertain = TRUE)
#' # No filtering, just add PHASE to FW rows - do not include messages when
#' # the phase cannot be confidently identified
#' getFindingsPhase(dbToken, fw,
#'                  noFilterReportUncertain = FALSE)
#' }
getFindingsPhase <-function(dbToken,
                            findings,
                            phaseFilter = NULL,
                            inclUncertain = FALSE,
                            noFilterReportUncertain = TRUE) {

  ################################################################################
  # Derive the phase for a given epoch text - using regular expressions
  getPhase<-function(epoch) {
    if (is.na(epoch) ) {
      'Uncertain'
    } else if (grepl("pre.*(tr(ea)?t|dos|test|study|exposure)|acclimat|screen|baseline|allocat|random",
                     epoch,
                     ignore.case = TRUE)) {
      'Screening'
    } else if (grepl("recovery|post.*(tr(ea)?t|dos|test|study|exposure)",
                     epoch,
                     ignore.case = TRUE)) {
      'Recovery'
    } else if (  grepl("tr(ea)?t|dos|test|exposure",
                       epoch,
                       ignore.case = TRUE)
                 & !grepl("off|non|free|holiday",
                          epoch,
                          ignore.case = TRUE)) {
      'Treatment'
    } else {
      'Uncertain'
    }
  }
  #####################################################################################

  # Evaluate input parameters

  if (!data.table::is.data.table(findings))
    stop('Input parameter findings must have assigned a data table ')

  if (is.null(phaseFilter) | isTRUE(is.na(phaseFilter)) | isTRUE(phaseFilter=='')) {
    execFilter <- FALSE
  } else
    execFilter <- TRUE


  if (!(inclUncertain %in% c(TRUE,FALSE)))
    stop("Parameter inclUncertain must be either TRUE or FALSE")

  if (!execFilter & !(noFilterReportUncertain %in% c(TRUE,FALSE)))
    stop("Parameter noFilterReportUncertain must be either TRUE or FALSE")

  # Get the domain name of findings table
  domain = utils::head(findings, n=1)[,c('DOMAIN')]

  # The specific --SEQ and --DTC variable names for the domain
  domainSEQ <- paste(toupper(domain), "SEQ", sep="")
  domainDTC <- paste(toupper(domain), "DTC", sep="")

  # Get list of relevant studies
  studyList <- unique(findings[,c('STUDYID')])
  # Get list of relevant animals - exclude potential pooled rows
  studyAnimalList <- unique(findings[nchar(USUBJID) > 0,
                                     c('STUDYID', 'USUBJID')])
  pooledFindings <- FALSE
  if ('POOLID' %in% names(findings)) {
    # Get list of pools included in input findings
    poolList <-
      unique(findings[nchar(POOLID) > 0,c('STUDYID', 'POOLID')])
    if (nrow(poolList) > 0) {
      # Input data contains pooled data
      pooledFindings <- TRUE

      # Extract subset of columns to be used for identification of study phase
      # - including POOLID
      subjFindings <- subset(findings, TRUE,
                             c("STUDYID",
                             "USUBJID",
                             "POOLID",
                             domainSEQ,
                             domainDTC))

      # Get subjects included in the pools
      studyAnimalList <-
        (genericQuery(dbToken,
                     "select * from pooldef
                       where studyid in (?)",
                     studyList) %>%
        data.table::merge.data.table(poolList,
                                    by = c('STUDYID', 'POOLID'))) %>%
        # - add list of pooled subjects to list of subjects
        {data.table::rbindlist(list(studyAnimalList, .),
                               use.names=TRUE,
                               fill=TRUE)} %>%
        # Ensure the list is unique
        unique()

      subjFindings <-
        # Expand the pooled findings rows to subject level - i.e. each pooled
        # row is expanded to one row per subject included in the pool
        data.table::merge.data.table(studyAnimalList, subjFindings[nchar(POOLID) > 0,!"USUBJID"],
                                     by=c('STUDYID', 'POOLID'),
                                     allow.cartesian = TRUE) %>%
        # Add the subject level rows
        {data.table::rbindlist(list(subjFindings[nchar(USUBJID) > 0], .),
                               use.names=TRUE,
                               fill=TRUE)}
    }
  }
  if (!pooledFindings)
    # Extract subset of columns to be used for identification of study phase
    # - excluding POOLID
    subjFindings <- subset(findings, TRUE,
                           c("STUDYID",
                             "USUBJID",
                             domainSEQ,
                             domainDTC))

  # Rename --SEQ and --DTC variable names to fixed names
  data.table::setnames(subjFindings,
                       c(domainSEQ,domainDTC),
                       c("SEQ","DTC"))

  # Get related DM rows
  DM <- getSubjData(dbToken,
                    studyAnimalList, 'DM',c('ARMCD'))[,c('STUDYID',
                                                         'USUBJID',
                                                         'ARMCD')]
  # Get related SE rows
  SE <- getSubjData(dbToken,
                    studyAnimalList, 'SE')[,c('STUDYID',
                                              'USUBJID',
                                              'ETCD',
                                              'SESTDTC',
                                              'SEENDTC')]
  # Get related TA rows
  TA <- genericQuery(dbToken,
                     'select * from "TA"
                      where "STUDYID" in (?)',
                     studyList)


  # Join DM with TA to get a list off all possible epochs per animal
                          # Extract DM rows for animals in the input list of findings
  dmEpoch <-
    data.table::merge.data.table(DM[,c('STUDYID','USUBJID','ARMCD')],
                                 TA[,list(STUDYID,
                                          ARMCD,
                                          ETCD,
                                          EPOCH=tolower(EPOCH))],
                                 by = c('STUDYID', 'ARMCD'),
                                 all.x = TRUE,
                                 allow.cartesian=TRUE)[,!"ARMCD"]

  # Join findings keys with Subject Elements to get all possible element(s) per subject
  findElementAll <-
    data.table::merge.data.table(subjFindings, SE,
                                 by = c('STUDYID', 'USUBJID'),
                                 all.x = TRUE,
                                 allow.cartesian=TRUE)

  # Evaluate each of all finding rows to find the associated element.
  # Check if the finding --DTC is within the data interval in one or two elements start/end dates
  # There may be more than one element associated with a finding if the --DTC is within the interval of the end date in
  # one element and start date in another element(i.e. a time part is missing in one or more of the --DTC, SESTDTC, SEENDTC).
  # All ISO date/time variables are converted to numerical datetime before the comparison
  # If a --DTC or SESTDTC value misses the timepart
  # - it's automatically calculated as 00:00:00
  # If a SEENDTC values misses the timepart - it's calculated as 23:59:59
  # Include additional rows where one or more involved ISO date values are empty or invalid
  # - add an message for these uncertain rows
  findElementEvaluated <-
    findElementAll[is.na(parsedate::parse_iso_8601(DTC))     |
                   is.na(parsedate::parse_iso_8601(SESTDTC)) |
                   is.na(parsedate::parse_iso_8601(SEENDTC)) |
                   data.table::between(parsedate::parse_iso_8601(DTC),
                                       parsedate::parse_iso_8601(SESTDTC),
                                       parsedate::parse_iso_8601(ifelse(grepl("T",SEENDTC),
                                                                        SEENDTC,
                                                                        paste(SEENDTC,"T23:59:59",sep="")))) |
                   (! is.na(parsedate::parse_iso_8601(DTC))     &
                    ! is.na(parsedate::parse_iso_8601(SESTDTC)) &
                    ! is.na(parsedate::parse_iso_8601(SEENDTC)) &
                    ! data.table::between(parsedate::parse_iso_8601(DTC),
                                          parsedate::parse_iso_8601(SESTDTC),
                                          parsedate::parse_iso_8601(ifelse(grepl("T",SEENDTC),
                                                                           SEENDTC,
                                                                           paste(SEENDTC,"T23:59:59",sep="")))))][,
                   MSG := ifelse(is.na(parsedate::parse_iso_8601(DTC))     |
                                   is.na(parsedate::parse_iso_8601(SESTDTC)) |
                                   is.na(parsedate::parse_iso_8601(SEENDTC)),
                                 paste0('Empty or invalid ISO8601 date/time value in: ', domainDTC, ', SESTDTC or SEENDTC'),
                                 ifelse((! is.na(parsedate::parse_iso_8601(DTC))     &
                                           ! is.na(parsedate::parse_iso_8601(SESTDTC)) &
                                           ! is.na(parsedate::parse_iso_8601(SEENDTC)) &
                                           ! data.table::between(parsedate::parse_iso_8601(DTC),
                                                                 parsedate::parse_iso_8601(SESTDTC),
                                                                 parsedate::parse_iso_8601(ifelse(grepl("T",SEENDTC),
                                                                                                  SEENDTC,
                                                                                                  paste(SEENDTC,"T23:59:59",sep=""))))),
                                        paste0(domainDTC, ' does fit into any SESTDTC/SEENDTC interval'),
                                        as.character(NA)))]

  # Extract all findings where one or more elements are identified
  findElement = findElementEvaluated[is.na(MSG)][,MSG := NULL]

  # Extract all findings where no element could be identified
  findElementErr <- unique(unique(
    data.table::merge.data.table(
      data.table::fsetdiff(unique(findElementEvaluated[! is.na(MSG),
                                                       list(STUDYID,USUBJID,SEQ)]),
                           findElement[,list(STUDYID,USUBJID,SEQ)]),
      findElementEvaluated[! is.na(MSG),
                           list(STUDYID,USUBJID,SEQ,MSG)],
      by = c('STUDYID', 'USUBJID', 'SEQ')))[, # Combine potential different error message per finding
                                              # into one message per finding separated by ';'
                                            MSG := paste(unlist(list(.SD)),
                                                          collapse = ';'),
                                            by = c('STUDYID', 'USUBJID', 'SEQ'),
                                            .SDcols='MSG'])


  # Add number of distinct elements per finding to identify if more then one element fits the finding date
  # - Remove the DTC column (--DTC is included later on)
  findElement[,NUM_ELEMENT := .N, by = c('STUDYID','USUBJID','SEQ')][,DTC := NULL]

  # Merge epochs per animal with findings keys/elements to get the epoch(s) per finding
  # Keep each element whether an epoch is found or not (left join)
  # - Derive the study phase based on the epoch
  findPhaseFound <-
    data.table::merge.data.table(findElement, dmEpoch,
                                 by = c('STUDYID', 'USUBJID', 'ETCD'),
                                 all.x = TRUE)[, PHASE := mapply(getPhase,EPOCH)]

  # Extract findings with a valid phase, ie.
  # - one identified element and phase is identified
  findPhaseOK <-
    findPhaseFound[NUM_ELEMENT == 1 & PHASE != 'Uncertain',c('STUDYID', 'USUBJID', 'SEQ', 'PHASE')]

  # Extract findings with 'Uncertain' PHASE, include an explanation
  findPhaseErr1 <- unique(
    findPhaseFound[PHASE == 'Uncertain',
                 c('STUDYID', 'USUBJID', 'SEQ', 'PHASE')][,
                      MSG := 'Could not decide PHASE based on EPOCH value'])

  # For findings with multiple elements
  # - collect all phases per finding in a message string
  findPhaseErr2 <- unique(
    findPhaseFound[NUM_ELEMENT > 1][,
                `:=` (MSG = paste0('Phase could not be decided due to date overlap - possibly: ',
                                   paste(PHASE, collapse=', '),
                                   ' (EPOCH: ', paste(EPOCH, collapse=', '),')',
                                   ' (ETCD: ', paste(ETCD, collapse=', '),')',
                                   ' (SESTDTC/SEENDTC: ',
                                   paste(paste(SESTDTC,SEENDTC,sep='/'),
                                         collapse=', '),')'),
                      PHASE = 'Uncertain'),
                by = c('STUDYID','USUBJID','SEQ')][, `:=` (ETCD = NULL,
                                                           SESTDTC = NULL,
                                                           SEENDTC = NULL,
                                                           EPOCH = NULL,
                                                           NUM_ELEMENT = NULL)])

  # Combine all findings - with or without errors
  findPhaseAll <-
    data.table::rbindlist(list(
      findPhaseOK[,MSG := as.character(NA)],
      unique(
        data.table::rbindlist(list(findPhaseErr1,
                                   findPhaseErr2))[,MSG := paste(unlist(list(.SD)),
                                                                 collapse = ';')
                                                   ,by = c('STUDYID',
                                                           'USUBJID',
                                                           'SEQ',
                                                           'PHASE'),
                                                   ,.SDcols='MSG']),
      findElementErr[,PHASE := 'Uncertain']), use.names = TRUE, fill=TRUE)

  if (!pooledFindings)
    findPhase <- findPhaseAll[, c('STUDYID','USUBJID','SEQ','PHASE','MSG')]
  else {
    # Collapse subject level rows for pooled findings to pool level

    findPhasePools <-
      unique(findPhaseAll[!is.na(POOLID),!c('USUBJID')])[,
                # Add count of distinct subject level rows per pooled finding
                N := .N, by = c('STUDYID', 'POOLID', 'SEQ')]

    # Look into pooled findings with multiple subject level rows
    findPhasePoolsMulti <- findPhasePools[N > 1]

    # Check for different identified  phases per pooled finding (with multiple
    # subj level rows)
    findPoolMultiPhase <-
      unique(findPhasePoolsMulti[,c('STUDYID',
                                    'POOLID',
                                    'SEQ',
                                    'PHASE')])[,
                            N_PHASE := .N, by = c('STUDYID', 'POOLID', 'SEQ')]

    # Extract pooled findings with a distinct identified phase
    findPoolMultiPhaseOK <-
      data.table::merge.data.table(findPhasePools, findPoolMultiPhase[N_PHASE == 1])
    # Merge each non empty MSG per pool per finding into one string and remove
    # duplicated rows
    findPoolMultiPhaseOK <-
      data.table::merge.data.table(
        unique(findPoolMultiPhaseOK[,c('STUDYID', 'POOLID', 'SEQ', 'PHASE')]),
        unique(unique(
          findPoolMultiPhaseOK[!is.na(MSG),
                                c('STUDYID',
                                  'POOLID',
                                  'SEQ',
                                  'MSG')])[, MSG_ALL := paste0('[',paste(unlist(list(.SD)), collapse = '],['),']'),
                                           by = c('STUDYID', 'POOLID', 'SEQ'),
                                           .SDcols='MSG'][,MSG := NULL]),
        by = c('STUDYID', 'POOLID', 'SEQ'),
        all.x = TRUE)
    data.table::setnames(findPoolMultiPhaseOK, 'MSG_ALL', 'MSG')

    # Extract pooled findings with multiple identified phases
    findPoolMultiPhaseDiff <- unique(
      findPoolMultiPhase[N_PHASE > 1][,
        MSG := paste0('Different phases identified for individual subjects in pool: ',
                      paste(unlist(list(.SD)), collapse = ', ')),
        by = c('STUDYID', 'POOLID', 'SEQ'),
        .SDcols='PHASE'][,PHASE := 'Uncertain'])[,N_PHASE := NULL]

    # Stack all finding rows
    #  - subject level rows
    #  - pool level rows with equal identified phase for for all subjects per
    #    pool and all related subject level attributes equal
    #  - pool level rows with equal identified phase for for all subjects per
    #    pool but differences in some related subject level attributes
    #  - pool level rows with different identified phases for subjects per pool
    findPhase <-
      data.table::rbindlist(list(findPhaseAll[is.na(POOLID)],
                                 findPhasePools[N == 1][,N := NULL],
                                 findPoolMultiPhaseOK,
                                 findPoolMultiPhaseDiff),
                            use.names = TRUE, fill=TRUE)[, c('STUDYID','USUBJID','POOLID','SEQ','PHASE','MSG')]

  }

  if (execFilter) {
    # Apply the given PHASE filter
    if (inclUncertain)
      # - include the uncertain rows, finalize UNCERTAIN_MSG
      findPhase <- findPhase[toupper(PHASE) %in% toupper(phaseFilter) | !is.na(MSG)][,
                           `:=` (UNCERTAIN_MSG = ifelse(is.na(MSG),
                                                        as.character(NA),
                                                        paste0('FindingsPhase: ',
                                                               MSG)))][,MSG := NULL]
    else
      # - exclude the uncertain rows and message column
      findPhase <- findPhase[is.na(MSG)
                           & toupper(PHASE) %in% toupper(phaseFilter)][,MSG := NULL]
  }
  else {
    # No PHASE filter - include all rows
    if (noFilterReportUncertain)
      # - finalize NOT_VALID_MSG
      findPhase[, `:=` (NOT_VALID_MSG = ifelse(is.na(MSG),
                                               as.character(NA),
                                               paste0('FindingsPhase: ',
                                                      MSG)))][,MSG := NULL]
    else
      # - exclude message column
      findPhase[,MSG := NULL]
  }
  # Rename the SEQ variable to the real --SEQ variable name
  data.table::setnames(findPhase, "SEQ",domainSEQ)



  # Merge the set of finding ids with identified (and potential filtered) phases
  # and related variables with the input set of findings to include all variables
  if ( ! pooledFindings)
    # Only subject level data
    foundFindings <-
      merge(findings, findPhase,
            by = c("STUDYID", "USUBJID",domainSEQ))
  else {
    # Join pooled level and subject level data and stack rows
    foundFindings <-
      data.table::rbindlist(list(
        merge(findings[!is.na(USUBJID), !c("POOLID")],
              findPhase[!is.na(USUBJID), !c("POOLID")],
              by = c("STUDYID", "USUBJID",domainSEQ)),
        merge(findings[!is.na(POOLID), !c("USUBJID")],
              findPhase[!is.na(POOLID), !c("USUBJID")],
              by = c("STUDYID", "POOLID",domainSEQ))),
        use.names = TRUE, fill = TRUE)
  }

  # Do final preparation of set of found animals and return
  prepareFinalResults(foundFindings,
                             names(findings),
                             c('PHASE'))
}

################################################################################
# Avoid  'no visible binding for global variable' notes from check of package:
