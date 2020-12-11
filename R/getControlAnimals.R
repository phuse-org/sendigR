# ADD calculation of animal age

################################################################################
## The function getControlAnimals
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2020-12-04   Bo Larsen             Initial version
################################################################################

#' Extract a list of control animals for a list of studies
#'
#' Returns a data table with a list of animals belonging to the groups for
#' negative control in the given list of studies.\cr
#'
#' The set of animals contains all animals from DM where the SETCD
#' is associated with a TX parameter 'TCNTRL'. Negative control animals are
#' further defined by
#' \itemize{
#'   \item  either containing a word from a set of words, to
#'    automatically distinguish it as a negative control:
#'    \itemize{
#'      \item \['placebo', 'untreated', 'sham'\]
#'    }
#'   \item or containing a combination of a word  from of two lists:
#'     \enumerate{
#'      \item \['negative', 'saline', 'peg', 'vehicle', 'citrate', 'dextrose', 'water', 'air'\]
#'      \item \['item', 'control', 'article'\]
#'    }
#' }
#' Animals are in all cases excluded (i.e. whether \code{inclUncertain=TRUE} or
#' \code{inclUncertain=FALSE}) from the output set, when they are identified as
#' psoitive control animals - i.e they are associated with a TX parameter
#' 'TCNTRL' containing a word from this set of words:
#' \itemize{
#'   \item \['positive','reference'\]
#' }
#'
#' If input parameter \code{inclUncertain=TRUE}, uncertain animals are included
#' in the output set. These uncertain situations are identified and reported (in
#' column UNCERTAIN_MSG):
#' \itemize{
#' \item TX parameter 'TCNTRL' is missing
#' \item TXVAL for TX parameter 'TCNTRL' cannot be identified as Negative or
#' Positive control according to the algorithm described above
#' }
#'
#' @param dbToken Mandatory - token for the open database connection
#' @param studyList Mandatory.\cr
#'  A data.table with a list of studies to limit the output to be within this
#'  set of studies.\cr
#'  The table must include a column named 'STUDYID'.
#' @param inclUncertain Optional, TRUE or FALSE, default: FALSE.\cr
#'   Indicates whether animals which cannot be identified as neither negative
#'   nor positive control (i.e. uncertain animals) shall be included or not in
#'   the output data table.
#'
#' @return The function return a data.table with columns:
#'   \itemize{
#'   \item STUDYID       (character)
#'   \item Additional columns contained in the \code{studyList} table (if such an input
#'   table is given)
#' The value of the TX parameter TCNTRL which is used for identification of
#' whether its a negative control group or not
#'   \item TCNTRL        (character)
#'   \item USUBJID       (character)
#'   \item UNCERTAIN_MSG (character)
#' Included when parameter \code{inclUncertain=TRUE}.\cr
#' Contains the reason for an uncertain animal is NA for rows for confident
#' identified negative control animals.
#'   \item NOT_VALID_MSG (character)
#' Included if the column is included in data table specified in \code{studyList},
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' controlAnimals <- getControlAnimals(myDbToken, allSTudies)
#' }
getControlAnimals<-function(dbToken,
                            studyList,
                            inclUncertain=FALSE) {

  # Definition of the search words for negative control terms
  negStandAlonesWords <- c('placebo', 'untreated', 'sham')
  negModifiers <- c('negative', 'saline', 'peg', 'vehicle', 'citrate',
                    'dextrose', 'water', 'air')

  # Definition of the search words for negative control terms
  posModifiers <- c('positive','reference')

  # Definition of the search words for common for negative/positive control terms
  allCntrlNouns <- c('item', 'control', 'article')

  ###################################################################################
  # Check whether the input string setNames contains a value indicating it's a
  # negative or positive control group.
  # Default input parameter values set to identify negative or positive control groups
  ###################################################################################
  HasControlTerms<-function(setName, standAlonesWords=negStandAlonesWords,
                                     controlMods=negModifiers,
                                     controlNouns=allCntrlNouns) {
    # trim white space and convert to lowercase
    setName <- trimws(tolower(setName))

    # prepare strings in regex format
    if (! is.null(standAlonesWords)) {
      standAlongString <- paste(standAlonesWords, collapse = '|')
    }
    controlModsString <- paste(controlMods, collapse = '|')
    controlNounsString <- paste(controlNouns, collapse = '|')

    # if the string matches any of  the words
    # in the controlMods vector exactly
    # it should be treated as a relevant control
    if (setName %in% controlMods) {
      return(TRUE)
    }

    # the logic in the code below first
    # checks if the string contains any standalone words
    # to delineate it  as a relevant control group
    # the second if block check if it contains
    # words from both the 'controlNouns' and
    # 'controlMods'

    if (! is.null(standAlonesWords) & grepl(standAlongString, setName)) {
      return(TRUE)
    }
    if ((grepl(controlModsString, setName)) & (grepl(controlNounsString, setName))) {
      return(TRUE)
    }
    return(FALSE)
  }
  ###################################################################################

  # Verify input parameter
  if (!data.table::is.data.table(studyList)) {
    stop('Input parameter studyList must have assigned a data table ')
  }
  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter inclUncertain must be either TRUE or FALSE")
  }

  # Extract  TX parameter 'TCNTRL' for the studies included in studyList
  # - include a row for each for study which may miss a 'TCNTRL' parameter
  # - include the animals from DM belonging to the identified trial sets
  txDmCtrlSet <- genericQuery(dbToken,
                            "select ts.studyid
                                   ,tx.txval as TCNTRL
                                   ,tx.setcd
                                   ,dm.usubjid
                             from (select distinct STUDYID
                                   from ts
                                   where studyid in (:1)) ts
                             left join tx
                               on ts.studyid = tx.studyid
                              and tx.txparmcd = 'TCNTRL'
                             left join dm
                               on dm.studyid = ts.studyid
                              and ((tx.setcd is not null
                                    and dm.setcd = tx.setcd)
                                  or
                                   (tx.setcd is null))",
                            studyList[,c('STUDYID')])


  # Extract the unique set of trial sets
  txCtrlSet <- unique(txDmCtrlSet[!is.na(TCNTRL),c('TCNTRL')])


  # Get set of control groups identified as negative
  foundCtrlSet <- txCtrlSet[FALSE]
  if (txCtrlSet[,.N] > 0)
    foundCtrlSet <- txCtrlSet[sapply(txCtrlSet$TCNTRL, HasControlTerms)]

  if (inclUncertain) {
    ## Include uncertain studies/animals

    # Get the control groups not identified as negative
    notNegCtrlSet <- data.table::fsetdiff(txCtrlSet, foundCtrlSet[])
    # Get the uncertain control groups  - i.e. control set not identified as negative or positive
    uncertainCtrlSet <- notNegCtrlSet[FALSE]
    if (notNegCtrlSet[,.N] > 0)
      uncertainCtrlSet <-
        data.table::fsetdiff(notNegCtrlSet,
                             # Set of control groups identified as positive
                             notNegCtrlSet[sapply(notNegCtrlSet$TCNTRL, HasControlTerms,NA,posModifiers)])

    # add empty UNCERTAIN_MSG to confidently identified control groups
    foundCtrlSet[,`:=` (UNCERTAIN_MSG = as.character(NA))]

    if (uncertainCtrlSet[,.N] > 0) {
      # Uncertain groups have been identified

      # Set the UNCERTAIN_MSG for the control groups identified as uncertain
      uncertainCtrlSet[, `:=` (UNCERTAIN_MSG = 'ControlAnimals: Cannot decide type of control group from TXVAL in TX parameter TCNTRL')]

      # Combine set of negative and uncertain control groups
      foundCtrlSet<-data.table::rbindlist(list(foundCtrlSet, uncertainCtrlSet))
    }

    # Get the list of animals belonging to identified plus uncertain control groups
    dmCtrlSet <- data.table::merge.data.table(txDmCtrlSet[, c('STUDYID', 'TCNTRL', 'USUBJID')],
                                              foundCtrlSet, by=c('TCNTRL'),
                                              all.y=TRUE)[,c('STUDYID', 'TCNTRL', 'USUBJID','UNCERTAIN_MSG')]
    # Add the list of animals which may belong to studies with no TX.TCNTRL defined
    dmCtrlSet <- data.table::rbindlist(list(dmCtrlSet,
                                            txDmCtrlSet[is.na(TCNTRL), list(STUDYID, TCNTRL, USUBJID,
                                                                         UNCERTAIN_MSG = 'GetControlAnimals: TX parameter TCNTRL is missing')]))

    # Merge with the studyList to include all study level attributes and accumulate eventual UNCERTAIN_MSG texts
    dmCtrlSet <- merge(studyList, dmCtrlSet, by='STUDYID')
  }
  else {
    # Get the list of animals belong to the identified control groups
    dmCtrlSet <- data.table::merge.data.table(txDmCtrlSet[, c('STUDYID', 'TCNTRL', 'USUBJID')],
                                              foundCtrlSet, by=c('TCNTRL'),
                                              all.y=TRUE)[,c('STUDYID', 'TCNTRL', 'USUBJID')]
    # Merge with the studyList to include all study level attributes
    dmCtrlSet <- data.table::merge.data.table(studyList, dmCtrlSet, by='STUDYID')
  }

  # Do final preparation of set of found animals and return
  return(prepareFinalResults(dmCtrlSet, names(studyList), c('TCNTRL', 'USUBJID')))
}

################################################################################
# Avoid  'no visible binding for global variable' notes from check of package:
UNCERTAIN_MSG.x <- UNCERTAIN_MSG.y  <- NULL
TCNTRL <- NULL
USUBJID <- NULL

