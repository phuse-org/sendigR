################################################################################
## The function getStudiesSDESIGN.
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2020-12-04   Bo Larsen             Initial version
################################################################################

#' Extract a list of SEND studies whith a specified study design.
#'
#' Returns a data table with the list of study ids from TS where the value of
#' TSVAL for the TSPARMCD 'SDESIGN' is equal to a given study design.
#'
#' Extracts the set of studies from TS where
#' the value of TSVAL for the TSPARMCD 'SDESIGN' is equal to a given
#' study design.\cr
#' The comparison of study design values are done case insensitive.\cr
#'
#' If input parameter \code{inclUncertain=TRUE}, uncertain rows are extracted too,
#' i.e. studies included in TS
#' \itemize{
#' \item without any row for TSPARMCD='SDESIGN' or
#' \item TSVAL doesn't contain a value included in the  CDISC CT list
#'      'DESIGN' for TSPARMCD='SDESIGN'
#' }
#' are included in the returned set of data - including a message telling the
#' reason of uncertainty.
#'
#' @param dbToken Mandatory - token for the open database connection
#' @param studyList Optional.\cr
#'  A data.table with a list of studies to limit the output to be within this
#'  set of studies.\cr
#'  The table must include a column named 'STUDYID'
#' @param studyDesignFilter Mandatory, character. The study design to use as
#'   criterion for filtering of the study id values. It can be a single string,
#'   a vector or a list of multiple strings.
#' @param exclusively Optional.
#'   \itemize{
#'     \item TRUE: Include studies only for studies with no other study design
#'   then included in studyDesignFilter.
#'     \item FALSE: Include animals for all studies with route matching
#'   studyDesignFilter.
#'   }
#' @param inclUncertain Optional, TRUE or FALSE, default: FALSE.\cr
#' Indicates whether study ids with SDESIGN value which are is missing or wrong
#' shall be included or not in the output data table.
#'
#' @return The function return a data.table with columns:
#'   \itemize{
#'   \item STUDYID       (character)
#'   \item Additional columns contained in the \code{studyList} table (if such an input
#'   table is given)
#'   \item SDESIGN       (character)
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
#' GetStudyListSDESIGN(myDbToken, 'PARALLEL')
#' }


getStudiesSDESIGN<-function(dbToken,
                              studyList=NULL,
                              studyDesignFilter=NULL,
                              exclusively=TRUE,
                              inclUncertain=FALSE) {

  if ((is.null(studyDesignFilter) | isTRUE(is.na(studyDesignFilter)) | isTRUE(studyDesignFilter==""))) {
    stop('Input parameter studyDesignFilter must have assigned a non-empty value')
  }

  if (!(exclusively %in% c(TRUE,FALSE))) {
    stop("Parameter Exclusively must be either TRUE or FALSE")
  }

  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter inclUncertain must be either TRUE or FALSE")
  }

  studyListIncl<-FALSE
  if (data.table::is.data.table(studyList)) {
    # An initial list of studies is included
    studyListIncl<-TRUE
  }

  # Extract TS parameter 'SDESIGN'
  # - include a row for each for study which may miss a SDESIGN parameter
  tsSDESIGN <-
    genericQuery(dbToken, "select ts0.studyid,  ts1.tsval as SDESIGN
                             from (select distinct STUDYID from ts) ts0
                             left join ts ts1
                               on ts0.studyid = ts1.studyid
                              and ts1.tsparmcd = 'SDESIGN'")

  if (studyListIncl) {
    # Limit to the set of studies given as input
    tsSDESIGN<-data.table::merge.data.table(tsSDESIGN, studyList[,list(STUDYID)], by='STUDYID')
  }

  # Add variable with count of distinct study designs specified per study
  tsSDESIGN[, `:=` (NUM_SDESIGN = .N), by = STUDYID]

  # Construct the statement to apply the specified design
  designFilter<-'toupper(SDESIGN) %in% toupper(trimws(studyDesignFilter))'
  if (exclusively) {
    designFilter<-paste(designFilter, ' & NUM_SDESIGN==1', sep='')
  }

  if (inclUncertain) {
    ## Include uncertain studies

    # Get values of codelist DESIGN from CDISC CT
    ctDESIGN<-getCTCodListValues(dbToken, "DESIGN")

    # Include condition for rows with empty or wrong value of SDESIGN
    designFilter<-paste(paste("(", designFilter), ") | ! (toupper(SDESIGN) %in% ctDESIGN)")

    # Build the statement to execute - include column indication of missing or wrong value
    stmt=paste(paste("tsSDESIGN[", designFilter, sep=""),
               ",list(STUDYID, SDESIGN, UNCERTAIN_MSG=ifelse( ! (toupper(SDESIGN) %in% ctDESIGN),
                                                          ifelse(is.na(SDESIGN),
                                                                 'GetStudyListSDESIGN: TS parameter SDESIGN is missing',
                                                                 'GetStudyListSDESIGN: TS parameter SDESIGN does not contain a valid CT value'),
                                                          NA))]",
               sep="")
    # Execute statement to extract studies fulfilling the condition(s) plus uncertain studies
    foundStudies<-eval(parse(text=stmt))

    if (studyListIncl) {
      # Merge the list of extracted studies with the input set of studies to keep
      # any additional columns from the input table
      foundStudies<-data.table::merge.data.table(foundStudies, studyList, by='STUDYID')
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
  else  {
    ## Do not include uncertain studies

    # Build the statement to extract studies fulfilling the condition(s) and execute
    foundStudies<-eval(parse(text=paste(paste("tsSDESIGN[",
                                              designFilter,
                                              sep=""),
                                        ",list(STUDYID, SDESIGN)]",
                                        sep="")))

    if (studyListIncl) {
      # Return the list of extracted studies merged with the input set of studies to keep
      # any additional columns from the input table
      return(data.table::merge.data.table(foundStudies, studyList, by='STUDYID'))
    }
    else {
      # Return the list of extracted studies
      return(foundStudies)
    }
  }
}

################################################################################
# Avoid  'no visible binding for global variable' notes from check of package:
UNCERTAIN_MSG.x <- UNCERTAIN_MSG.y <- STUDYID <- NULL



