################################################################################
## The function getFindingsSubjAge.
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2021-02-09   Bo Larsen             Initial version
################################################################################

#' Add the subject age at finding time - and optionally extract the set of
#' findings within a specified range of age.
#'
#' Returns a data table with the set of findings rows included in the
#' \code{findings} where the age of subjects at finding time is within the
#' interval specified in \code{fromAge} to \code{fromAge}.\cr
#' If the \code{fromAge} and \code{fromAge} are empty (null, na or empty
#' string), all rows from \code{findings} are returned.
#'
#' In both situation, the subject age at finding time is calculated into an
#' additional column \code{AGEDAYS} for each row in \code{findings} combined
#' with the the additional input data.table \code{animalList} using this
#' algorithm:
#'   \itemize{
#'     \item Determine the number of study days between study start and findings
#'       \itemize{
#'         \item if \code{findings.[domain]DY} is populated
#'           \itemize{
#'             \item If \code{findings.[domain]DY > 0} then use
#'             \code{findings.[domain]DY - 1}
#'             \item Else use \code{findings.[domain]DY}
#'           }
#'         \item Else If \code{findings.[domain]DTC} is populated compute
#'         \code{animalList.RFSTDTC – findings.[domain]DTC} in days\cr where
#'         \code{animalList.RFSTDTC} is each subject's reference start date
#'         (\code{DM.RFSTDTC})
#'       }
#'     \item Animal age at time of finding is then calculated as
#'         \code{animalList.AGEDAYS + [study days between study start and
#'         findings]}\cr
#'         where \code{animalList.AGEDAYS} is the  subject age at reference
#'         start date(calculated during extraction of control subjects in
#'         \code{\link{getControlSubj}}.
#'     \item For pooled findings rows - i.e. POOLID is populated instead of
#'         USUBJID - the animal age at time of finding is calculated per animal
#'         included in the each pool and finding.
#'         \itemize{
#'            \item If all calculated ages are equal within a pool and finding,
#'               the calculated age is populated for this pool/finding.
#'            \item If all calculated ages are within the same time internal
#'               (2 days) within a pool and finding, the minimum calculated age
#'               plus 1 day is populated for this pool/finding.
#'        }
#'   }
#'
#' If both \code{fromAge} and \code{toAge} values are specified - all the rows
#' from the input table \code{findings} where value of the calculated
#' \code{AGEDYAS} is within the interval of the specified start/end age interval
#' are returned - including the values equal to the start/end age values.\cr
#' If only a \code{fromAge} value is specified - all the rows from the input
#' table \code{findings} where value of \code{AGEDYAS} equal to or greater than
#' the input age are returned.\cr
#' If only a \code{toAge} value is specified - all the rows from input table
#' \code{findings} where value of AGEDAYS is equal to or less than the input age
#' are extracted and returned.
#' The input age value(s) is/are converted to days before extraction of rows
#' from the input data tables using the input value(s) as filter - using this
#' conversion:
#'   \itemize{
#'     \item \code{DAYS}
#'     \item \code{WEEKS   : value * 7}
#'     \item \code{MONTHS  : value * 365/12}
#'     \item \code{YEARS   : value * 365}
#'   }
#'
#' If input parameter \code{inclUncertain=TRUE}, findings rows where the age at
#' finding time cannot be confidently identified are included in the output set.
#' These uncertain situations are identified and reported (in column
#' UNCERTAIN_MSG):
#'  \itemize{
#'    \item No age at reference time  has been calculated for subject
#'    (\code{animalList.AGEDAYS})
#'    \item Reference start time  is missing or contains invalid ISO8601 date
#'    value for subject (\code{animalList.RFSTDTC}).
#'    \item Missing \code{[domain]DY} value and missing or invalid ISO8601 date
#'    \code{[domain]DTC} value for finding
#'    \item For pooled findings:
#'      \itemize{
#'        \item More than two days between minimum and maximum of
#'        \code{animalList.AGEDAYS} for the set of animals in a pool.
#'        \item Different values in \code{animalList.RFSTDTC} for the set of
#'        animals in a pool.
#'      }
#'  }
#' The same checks are performed and reported in column \code{NOT_VALID_MSG} if
#' \code{fromAge} and \code{fromAge} are empty and \code{noFilterReportUncertain
#' = TRUE}.
#'
#' @param dbToken Mandatory\cr
#'   Token for the open database connection (see \code{\link{initEnvironment}}).
#' @param findings Mandatory, data.table.\cr
#'  A table with the set of input finding rows to process.\cr
#'  The table must include at least columns named
#'  \itemize{
#'     \item \code{STUDYID}
#'     \item \code{USUBJID}
#'     \item \code{DOMAIN}
#'     \item \code{[domain]SEQ}
#'     \item \code{[domain]DY}
#'     \item \code{[domain]DTC}
#'  }
#'  where \code{[domain]} is the name of the actual findings domain - e.g. \code{LBSEQ}, \code{LBDY}
#'  and \code{LBDTC}
#' @param animalList Mandatory, data.table.\cr
#'  A data with the set of animals included in the \code{findings} table
#'  (may contain more animals than included in \code{findings}).\cr
#'  The data set must contain at least these columns returned by the function [getControlSubj]
#'  \itemize{
#'     \item \code{STUDYID}
#'     \item \code{USUBJID}
#'     \item \code{RFSTDTC}
#'     \item \code{DM_AGEDAYS}
#'     \item \code{NO_AGE_MSG}
#'  }
#' @param fromAge Optional, character \cr
#'   The start of age interval to extract.\cr
#'   Must be in a string in this format:\cr
#'   \code{[value][age unit]} where \code{[age unit]} is one of
#'   \itemize{
#'      \item \code{d, day, days}
#'      \item \code{w, week, weeks}
#'      \item \code{m, month, months}
#'      \item \code{y, year, years}
#'  }
#'  The unit is case-insensitive, space(s) between age value and unit is
#'  allowed.
#' @param toAge Optional. character \cr
#'   The start of age interval to extract.\cr
#'   Must be in a string in in the same format as described for \code{fromAge}.
#' @param inclUncertain  Mandatory, boolean.\cr
#'  Only relevant if the \code{fromAge} and/or \code{toAge} is/are not empty.\cr
#'  Indicates whether finding rows for which the age at finding time cannot be
#'  confidently identified, shall be included or not in the output data table.
#' @param noFilterReportUncertain  Optional, boolean.\cr
#'  Only relevant if the \code{fromAge} and \code{toAge} are empty.\cr
#'  Indicates if the reason should be included if the age at finding time cannot
#'  be confidently decided for an animal.
#'
#' @return  The function returns a data.table with columns in this order:
#'   \itemize{
#'   \item All columns contained in the \code{findings} input table (original
#'   order except optional \code{UNCERTAIN_MSG} and \code{NOT_VALID_MSG})
#'   \item \code{AGEDAYS}        (character)\cr
#'   The subject age at finding time calculated in days. Is \code{NA} if thge age
#'   cannot be confidently calculated.
#'   \item \code{UNCERTAIN_MSG}  (character)\cr
#' Included when parameter \code{inclUncertain=TRUE}.\cr
#' In case the age at finding time cannot be confidently matched during the
#' filtering of data, the column contains an indication of the reason.\cr
#' If any uncertainties have been identified for individual subjects included in
#' pools for pooled finding rows, one  message for is reported per pool/finding.\cr
#' Is NA for rows where the age at finding time can be confidently matched.\cr
#' A non-empty \code{UNCERTAIN_MSG} value generated by this function is merged with
#' non-empty \code{UNCERTAIN_MSG} values which may exist in the input set of findings
#' specified in \code{findings} - separated by '|'.
#'   \item \code{NOT_VALID_MSG} (character)\cr
#' Included when parameter \code{noFilterReportUncertain=TRUE}.\cr
#' In case the age at finding time cannot be confidently calculated, the column
#' contains an indication of the reason.\cr
#' Is NA for rows where age at finding time can be confidently calculated.\cr
#' A non-empty \code{NOT_VALID_MSG} value generated by this function is merged with
#' non-empty \code{NOT_VALID_MSG} values which may exist in the input set of findings
#' \code{findings} - separated by '|'.
#'}

#' @export
#'
#' @examples
#' \dontrun{
#' # Extract LB rows for the animals at age between 8 and 12 weeks at finding
#' # time - include uncertain rows
#' getFindingsSubjAge(dbToken = db,
#'                    findings = lb,
#'                    animalList = animals,
#'                    fromAge = '8w',
#'                    toAge = '12w',
#'                    inclUncertain = TRUE)
#' # No filtering, just add AGEDAYS to FW rows - do not include messages when
#' # the AGEDAYS cannot be confidently identified
#' getFindingsSubjAge(dbToken = db, findings = fw,  animalList = animals,
#'                    noFilterReportUncertain = FALSE)
#' }
#'
getFindingsSubjAge<-function(dbToken,
                             findings,
                             animalList,
                             fromAge = NULL,
                             toAge = NULL,
                             inclUncertain = FALSE,
                             noFilterReportUncertain = TRUE)
{

  ## Evaluate input parameters

  if (!data.table::is.data.table(findings)) {
    stop('Input parameter findings must have assigned a data table ')
  }

  if (!data.table::is.data.table(animalList)) {
    stop('Input parameter animalList must have assigned a data table ')
  }

  if ((is.null(fromAge) | isTRUE(is.na(fromAge)) | isTRUE(fromAge==''))
        & (is.null(toAge) | isTRUE(is.na(toAge)) | isTRUE(toAge==''))) {
    execFilter <- FALSE
  } else
    execFilter <- TRUE

  if (!(inclUncertain %in% c(TRUE,FALSE)))
    stop("Parameter inclUncertain must be either TRUE or FALSE")

  if (!execFilter & !(noFilterReportUncertain %in% c(TRUE,FALSE)))
    stop("Parameter noFilterReportUncertain must be either TRUE or FALSE")

  if (execFilter) {
    ## Evaluate if the given filter ages are valid

    # Regular expression pattern for a valid from/to age input values
    validPattern <- "^\\s*(\\d+)\\s*(d|day|days|w|week|weeks|m|month|months|y|year|years){1}\\s*$"

    # Evaluate age input string and calculate age in days
    calcAgeDays <- function(ageStr) {
      if (!(is.null(ageStr) | isTRUE(is.na(ageStr)) | isTRUE(ageStr==''))) {
        if (grepl(validPattern, ageStr, ignore.case = TRUE )) {
          # Valid age string specified - convert to days and return value
          ageVal <- as.numeric(stringr::str_match(tolower(ageStr), validPattern)[,2])
          ageUnit <- substr(stringr::str_match(tolower(ageStr), validPattern)[,3],1,1)
          if (ageUnit == 'd') {ageVal}
          else if (ageUnit == 'w') {ageVal * 7}
          else if (ageUnit == 'm') {round(ageVal * 365/12)}
          else if (ageUnit == 'y') {ageVal * 365}
        }
        else {
          # Invalid age string specified - return NULL to indicate that
          NULL
        }
      }
      else {
        # No age string specified - return NA to indicate that
        fromAgeDays<-NA
      }
    }
    fromAgeDays <- calcAgeDays(fromAge)
    toAgeDays <- calcAgeDays(toAge)
    if (is.null(fromAgeDays) | is.null(fromAgeDays)) {
      stop('ERROR: Wrong format of specified start and/or end age')
    }
  }

  ## All input parameters are valid

  # Get the domain name of findings table from the first row of input data
  domain = utils::head(findings, n=1)$DOMAIN
  seqCol = paste0(domain,'SEQ')

  {
    ## Calculate age at finding time

        # Calculate the age in days of each subject/pool at time of finding
    # - if the age in days could be calculated for the subject/pool
    calcFindAge <- function(tab) {
      tab[,`:=` (AGEDAYS = ifelse(!is.na(NO_AGE_MSG),
                                  # No age in days calculated for subject:
                                  as.numeric(NA),
                                  # We have and DM age in days:
                                  ifelse(!is.na(dy),
                                         #  --DY is populated and used for calculation:
                                         DM_AGEDAYS + ifelse(dy>0, dy-1, dy),
                                         ifelse(!is.na(parsedate::parse_iso_8601(dtc)),
                                                #  --DTC is populated and used for calculation instead:
                                                ifelse(!is.na(parsedate::parse_iso_8601(RFSTDTC)),
                                                       DM_AGEDAYS + as.Date(parsedate::parse_iso_8601(dtc)) - as.Date(parsedate::parse_iso_8601(RFSTDTC)),
                                                       # No RFSTDTC has been populated
                                                       as.numeric(NA)),
                                                # Neither --DY nor --DTC is populated
                                                as.numeric(NA)))),
                 NO_AGE_MSG = ifelse(!is.na(NO_AGE_MSG),
                                     # No DM age in days calculated for animal
                                     # - just keep the message
                                     NO_AGE_MSG,
                                     # Tried to calculate findings age
                                     # - check if it was possible
                                     ifelse(is.na(dy),
                                            ifelse(is.na(parsedate::parse_iso_8601(dtc)),
                                                   'FindingsSubjAge: --DY is empty and --DTC is empty or contains invalid ISO8601 date',
                                                   ifelse(is.na(parsedate::parse_iso_8601(RFSTDTC)),
                                                          'FindingsSubjAge: RFSTDTC is empty or contains invalid ISO8601 date',
                                                          # All is fine - age could be calculated
                                                          as.character(NA))),
                                            # All is fine - age could be calculated
                                            as.character(NA))))][,`:=` (
                                              dy = NULL,
                                              dtc = NULL,
                                              DM_AGEDAYS = NULL,
                                              RFSTDTC = NULL
                                            )]
    }

    # Merge relevant subject level findings columns with set of identified
    # control animals to get RFSTDTC and age in days
    findingsSUBJ <-
      merge(findings[USUBJID != '',
                     list(STUDYID,
                          USUBJID,
                          seq=get(paste0(domain,'SEQ')),
                          dy=get(paste0(toupper(domain),'DY')),
                          dtc=get(paste0(toupper(domain),'DTC')))],
            animalList[,list(STUDYID,
                             USUBJID,
                             RFSTDTC,
                             DM_AGEDAYS,
                             NO_AGE_MSG)],
            by=c("STUDYID", "USUBJID"),
            all.x = TRUE)
    # Calculate the age of each subject at finding time
    calcFindAge(findingsSUBJ)
    # Merge with complete set of finding columns
    findingsAll <-
      merge(findings, findingsSUBJ,
            by.x = c('STUDYID','USUBJID', seqCol),
            by.y = c('STUDYID','USUBJID', 'seq'))

    # Check for pool level findings
    if ('POOLID' %in% names(findings)) {
      # Get list of pools included in input findings
      poolList <- unique(findings[nchar(POOLID) > 0, c('STUDYID', 'POOLID')])
      if (nrow(poolList) > 0) {
        # Input data contains pooled data

        # Get dm level ages for pools

        if(dbToken$dbType=='sqlite'){
dt_pool <- genericQuery(dbToken,
  "select STUDYID, POOLID, USUBJID
                           from pooldef
                          where studyid in (:1)",
  unique(poolList[, c("STUDYID")]))

        }else if(dbToken$dbType=='postgresql'){
          dt_pool <- DBI::dbGetQuery(dbToken$dbHandle,
                                     'select "STUDYID", "POOLID", "USUBJID"
                           from "POOLDEF"
                          where "STUDYID" in ($1)',
                          params=list(poolList$STUDYID))
          dt_pool <- data.table::as.data.table(dt_pool)
        }


        poolAges <-
          # Extract POOLDEF
          ((data.table::as.data.table(dt_pool) %>%
          # merge with pool list to get subject ids
          merge(poolList, by = c('STUDYID', 'POOLID')) %>%
          # merge with list of control animals to get age info
          merge(animalList[,list(STUDYID,
                                 USUBJID,
                                 RFSTDTC,
                                 DM_AGEDAYS)],
                by = c('STUDYID', 'USUBJID')))[,
                         # Calculate min and max subj age per pool
                         `:=` (MIN_AGE = min(.SD), MAX_AGE = max(.SD)),
                         by = c('STUDYID', 'POOLID'),
                         .SDcols='DM_AGEDAYS'][,`:=` (USUBJID = NULL, DM_AGEDAYS=NULL)] %>%
          # Reduce to pool/RFSTDTC level rows
          #  - for pools with different subject level RFSTDTC values, set value to NA
          unique()

          )[,# Get  number of distinct RFSTDTC per pool
                   `:=` (RFSTDTC = ifelse(.N > 1, as.character(NA), RFSTDTC)),
                   by=c('STUDYID', 'POOLID')] %>%
          # Reduce to pool level rows
          unique()
          # Decide the pool level 'animal' age af RFSTDTC
          #  - is more than 2 days between min and max subject level,
          #    then pool level age cannot be decided
          #  - set message if AGEDAYS cannot be decided
          #  - set message if RFSTDTC cannot be decided at pool level
          poolAges[,`:=` (DM_AGEDAYS =
                            ifelse(is.na(MIN_AGE),
                                           as.numeric(NA),
                                           ifelse(MIN_AGE == MAX_AGE,
                                                  MIN_AGE,
                                                  ifelse((MAX_AGE - MIN_AGE) > 2,
                                                         as.numeric(NA),
                                                         MIN_AGE + 1))),
                          MSG_AGEDAYS =
                            ifelse(is.na(MIN_AGE),
                                   'Missing age for one or more subjects',
                                   ifelse((MAX_AGE - MIN_AGE) > 2,
                                          'To big difference in subject level ages',
                                          as.character(NA))),
                          MSG_RFSTDTC = ifelse(is.na(RFSTDTC),
                                               'Cannot decide RFSTDTC',
                                               as.character(NA)))]
          poolAges[,NO_AGE_MSG :=
                     ifelse(is.na(MSG_AGEDAYS) & is.na(MSG_RFSTDTC),
                            as.character(NA),
                            paste0('FindingsSubjAge: ',
                                   ifelse(!is.na(MSG_AGEDAYS) & !is.na(MSG_RFSTDTC),
                                          paste0(MSG_AGEDAYS, ' & ', MSG_RFSTDTC),
                                          ifelse(!is.na(MSG_AGEDAYS),
                                                 MSG_AGEDAYS,
                                                 MSG_RFSTDTC))))][,`:=` (MIN_AGE = NULL,
                                                                         MAX_AGE = NULL,
                                                                         MSG_AGEDAYS = NULL,
                                                                         MSG_RFSTDTC = NULL)]

          # Merge pool level findings with the pool level DM data
          findingsPOOL <-
            merge(findings[nchar(POOLID) > 0,
                           list(STUDYID,
                                POOLID,
                                seq=get(paste0(domain,'SEQ')),
                                dy=get(paste0(toupper(domain),'DY')),
                                dtc=get(paste0(toupper(domain),'DTC')))],
                  poolAges,
                  by = c('STUDYID','POOLID'))
          # - calculate age af findings time
          calcFindAge(findingsPOOL)
          # Merge with complete set of finding columns
          #  - add to the subject level list of subject age at finding time
          findingsAll <-
            merge(findings, findingsPOOL,
                  by.x = c('STUDYID','POOLID', seqCol),
                  by.y = c('STUDYID','POOLID', 'seq')) %>%
            {data.table::rbindlist(list(findingsAll, .),
                                  use.names = TRUE, fill=TRUE)}
      }
    }
  }

  if (execFilter) {
    ## execute the filtering based on specified from/to age(s)

    # Construct filter to use for extraction
    ageFilter <- NA
    if (!(is.na(fromAgeDays))) {
      # The filter condition for the fromAge
      ageFilter <- 'AGEDAYS >= fromAgeDays'
    }
    if (!(is.na(toAgeDays))) {
      if (is.na(ageFilter)) {
        # only toAge filter part
        ageFilter <- 'AGEDAYS <= toAgeDays'
      }
      else {
        # Add this filter part to the fromAge filter part
        ageFilter <- paste0(ageFilter,' & AGEDAYS <= toAgeDays')
      }
    }
    if (inclUncertain)
      # Add inclusion of uncertain rows to filter
      ageFilter <- paste0('(', ageFilter, ') | !is.na(NO_AGE_MSG)')
    else
      # Add exclusion of uncertain rows to filter
      ageFilter <- paste0('(', ageFilter, ') & is.na(NO_AGE_MSG)')

    # execute the extraction
    findingsFound <- eval(parse(text=paste0('findingsAll[', ageFilter,']')))

  } else {
    ## Just add the age at finding time
    findingsFound <- findingsAll
  }

  if ((execFilter & inclUncertain) |
       (!execFilter & noFilterReportUncertain)) {
    # Rename NO_AGE_MSG column
    if (execFilter)
      msgCol = 'UNCERTAIN_MSG'
    else
      msgCol = 'NOT_VALID_MSG'

    if (msgCol %in% names(findingsFound))
      data.table::setnames(findingsFound,
                           c(msgCol,'NO_AGE_MSG'),
                           c(paste0(msgCol, '.x'), paste0(msgCol, '.y')))
    else
      data.table::setnames(findingsFound, 'NO_AGE_MSG' ,msgCol)
  } else {
      # Drop NO_AGE_MSG column
      findingsFound[,`:=` (NO_AGE_MSG = NULL)]
  }

  prepareFinalResults(findingsFound,
                             names(findings),
                             c('AGEDAYS'))
}

################################################################################
# Avoid  'no visible binding for global variable' notes from check of package:

