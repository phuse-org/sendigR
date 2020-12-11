################################################################################
## The function getStudiesSTSTDTC.
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2020-12-04   Bo Larsen             Initial version
################################################################################

#' Extract a list of SEND studies with study start date within a specified
#' interval.
#'
#' Returns a data table with the list of study ids from TS where the value of
#' TSVAL for the TSPARMCD 'STSTDTC' is within a a given date interval.
#'
#' Extracts the set of study ids from TS where the value of TSVAL for the
#' TSPARMCD 'STSTDTC' falls within a specified start/end date interval in IS8601
#' format (input parameters \code{fromDTC}/\code{toDTC}).\cr Both complete and
#' incomplete input start/end dates can be handled. If only a year is specified
#' - the date set to the first of January that year.\cr If only a year and month
#' is specified - the date set to the first day in that month.\cr
#' If a time part is included in a specified input start/end date, it is
#' ignored.\cr
#' If both a start and end input date are specified - all the STUDYID values
#' from TS where TSVAL for TSPARMCD 'STSTDTC' is with the interval of the
#' specified start/end date interval are extracted and returned - including the
#' values equal to the start/end dates. are included.\cr
#' If only a start input date is specified - all the STUDYID values from TS
#' where TSVAL for TSPARMCD 'STSTDTC' is equal to or later than the input date
#' are extracted and returned.\cr
#' If only an end date is specified - all the STUDYID values from TS where TSVAL
#' for TSPARMCD 'STSTDTC' is equal to or earlier than the are date are extracted
#' and returned.\cr
#'
#' If input \code{Uncertain} is TRUE, uncertain studies are included in the
#' output set. These uncertain situations are identified and reported (in column
#' UNCERTAIN_MSG):
#' \itemize{
#' \item TS contains now row for TSPARMCD='STSTDTC'
#' \item TSVAL contains an invalid ISO8601 date format for TSPARMCD='STSTDTC'
#' }
#'
#' @param dbToken Mandatory - token for the open database connection
#' @param studyList Optional.\cr
#'  A data.table with a list of studies to limit the output to be within this
#'  set of studies.\cr
#'  The table must include a column named 'STUDYID'.
#' @param fromDTC  Optional (either or both of \code{fromDTC} and \code{toDTC}
#'   must be filled). The start of the date interval to extract - must be in ISO8601 date format.
#' @param toDTC Optional (either or both of \code{fromDTC} and \code{toDTC} must be filled).\cr
#'   The end of the date interval to extract - must be in ISO8601 date format.
#' @param inclUncertain Optional, TRUE or FALSE, default: FALSE.\cr
#'   Indicates whether study ids with STSTDTC which are are missing or wrong
#'   shall be included or not in the output data table.
#'
#' @return The function return a data.table with columns:
#'   \itemize{
#'   \item STUDYID       (character)
#'   \item Additional columns contained in the \code{studyList} table (if such an input
#'   table is given)
#'   \item STSTDTC       (character - ISO8601 format)
#'   \item UNCERTAIN_MSG (character)
#' Only included when parameter \code{inclUncertain=TRUE}.\cr
#' Contains indication of whether STSTDTC is missing of has wrong
#' format - is NA for rows where STSTDTC is valid.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' GetStudyListSTSTDTC(myDbToken, allSTudies, '2018','2020')
#' }

getStudiesSTSTDTC<-function(dbToken,
                              studyList=NULL,
                              fromDTC=NULL,
                              toDTC=NULL,
                              inclUncertain=FALSE) {
  # Evaluate input parameters
  if ((is.null(fromDTC) | isTRUE(is.na(fromDTC)) | isTRUE(fromDTC=="")) & (is.null(toDTC) | isTRUE(is.na(toDTC)) | isTRUE(toDTC==""))) {
    stop("A start and/or an end date must be specified")
  }

  if ((!(is.null(fromDTC) | isTRUE(is.na(fromDTC)) | isTRUE(fromDTC=="")) & isTRUE(is.na(parsedate::parse_iso_8601(fromDTC)))) | (!(is.null(toDTC) | isTRUE(is.na(toDTC)) | isTRUE(toDTC=="")) & isTRUE(is.na(parsedate::parse_iso_8601(toDTC))))) {
    stop("The value(s) specified for fromDTC and/or toDTC is not a valid ISO8601 date")
  }

  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter Exclusively must be either TRUE or FALSE")
  }

  studyListIncl<-FALSE
  if (data.table::is.data.table(studyList)) {
    # An initial list of studies is included
    studyListIncl<-TRUE
  }

  # Extract TS parameter STSTDTC
  # - include a row for each for study which may miss a STSTDTC parameter
  tsSTSTDTC <-
    genericQuery(dbToken, "select ts0.studyid,  ts1.tsval as STSTDTC
                             from (select distinct STUDYID from ts) ts0
                             left join ts ts1
                               on ts0.studyid = ts1.studyid
                              and ts1.tsparmcd = 'STSTDTC'")

  if (studyListIncl) {
    # Limit to the set of studies given as input
    tsSTSTDTC<-data.table::merge.data.table(tsSTSTDTC, studyList[,c('STUDYID')], by='STUDYID')
  }

  # Construct the statement to apply the specified date interval
  dtcFilter<-NA
  if (!(is.null(fromDTC) | isTRUE(is.na(parsedate::parse_iso_8601(fromDTC))))) {
    # The filter condition for the fromDTC
    dtcFilter<-"as.Date(parsedate::parse_iso_8601(STSTDTC)) >= as.Date(parsedate::parse_iso_8601(fromDTC))"
  }
  if (!(is.null(toDTC) | isTRUE(is.na(parsedate::parse_iso_8601(toDTC))))) {
    # Check the granularity if the specified toDTC and a 1 year/month/day to end of the interval to extract
    if (nchar(toDTC)==4) {
      # Only year has been specified + add 1 year to the date
      toDTCdate<-DescTools::AddMonths(as.Date(parsedate::parse_iso_8601(toDTC)),12)
    }
    else if (nchar(toDTC)==7) {
      # Only year and month has been specified -  add 1 month to the date
      toDTCdate<-DescTools::AddMonths(as.Date(parsedate::parse_iso_8601(toDTC)),1)
    }
    else {
      # A full date has been specified - add one day
      toDTCdate<-as.Date(parsedate::parse_iso_8601(toDTC))+1
    }
    if (is.na(dtcFilter)) {
      # only toDTC filter part
      dtcFilter<-"as.Date(parsedate::parse_iso_8601(STSTDTC)) < toDTCdate"
    }
    else {
      # Add this filter part to the frodmDTC filter part
      dtcFilter<-paste(dtcFilter," & as.Date(parsedate::parse_iso_8601(STSTDTC)) < toDTCdate",sep="")
    }
  }
  if (inclUncertain) {
    ## Include uncertain studies

    # Include condition for rows with empty or wrong value of STSTDTC
    dtcFilter<-paste(paste("(", dtcFilter), ") | is.na(parsedate::parse_iso_8601(STSTDTC))")

    # Build the statement to execute - include column indication of missing or wrong value
    stmt=paste(paste("tsSTSTDTC[", dtcFilter, sep=""),
               ",.(STUDYID, STSTDTC, UNCERTAIN_MSG=ifelse(is.na(parsedate::parse_iso_8601(STSTDTC)),
                                                          ifelse(is.na(STSTDTC),
                                                                 'GetStudyListSTSTDTC: TS parameter STSTDTC is missing',
                                                                 'GetStudyListSTSTDTC: TS parameter STSTDTC has wrong format'),
                                                          NA))]",
               sep="")
    # Execute statement to extract studies fulfilling the condition(s) plus uncertain studies
    foundStudies<-eval(parse(text=stmt))

    if (studyListIncl) {
      # Merge the list of extracted studies with the input set of studies to keep
      # any additional columns from the input table
      foundStudies<-data.table::merge.data.table(studyList, foundStudies, by='STUDYID')
      if ("UNCERTAIN_MSG.y" %in% names(foundStudies)) {
        # The studyList table contains alread an UNCERTAIN_MSG column
        #  - merge the UNCERTAIN_MSG from each of the merged tables into one column
        #  - non-empty messages are separated by '|'
        #  - exclude the original studyList.UNCERTAIN_MSG after the merge
        foundStudies<-foundStudies[,`:=` (UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y),
                                                               paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep='|'),
                                                               DescTools::Coalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)))][, `:=` (UNCERTAIN_MSG.x=NULL,UNCERTAIN_MSG.y=NULL)]
      }
    }
    # Return the list of extracted studies
    return(foundStudies)
  }
  else {
    ## Do not include uncertain studies

    # Build the statement to extract studies fulfilling the condition(s) and execute
    foundStudies<-eval(parse(text=paste(paste("tsSTSTDTC[",
                                              dtcFilter,
                                              sep=""),
                                        ",.(STUDYID, STSTDTC)]",
                                        sep="")))

    if (studyListIncl) {
      # Return the list of extracted studies merged with the input set of studies to keep
      # any additional columns from the input table
      return(data.table::merge.data.table(studyList, foundStudies, by='STUDYID'))
    }
    else {
      # Return the list of extracted studies
      return(foundStudies)
    }
  }
}

################################################################################
# Avoid  'no visible binding for global variable' notes from check of package:
UNCERTAIN_MSG.x <- UNCERTAIN_MSG.y  <- NULL

