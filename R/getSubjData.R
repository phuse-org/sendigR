################################################################################
## The function getSubjData.
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2021-01-28   Bo Larsen             Initial version
################################################################################

#' Extract data from a subject level domain.
#'
#' Extracts and returns all rows from the specified \code{domain} for the set
#' of subjects included in \code{animalList}.\cr
#'
#' @param dbToken Mandatory\cr
#'   Token for the open database connection (see \code{\link{initEnvironment}}).
#' @param animalList  Mandatory, data.table.\cr
#'   A table with the list of animals to be included in the output data.\cr
#'   The table must include at least columns named 'STUDYID' and 'USUBJID'.
#' @param domain Mandatory, character, not case sensitive.\cr
#'  The name of the domain table to extract data from.\cr
#'  The name must be a subject level domain - i.e. a table including a 'USUBJID'
#'  column.
#' @param colList Optional, character, not case sensitive.\cr
#'  The list of columns to be extracted from the specified domain table.\cr
#'  It can be a single string, a vector or a list of multiple strings.
#'
#' @return The function returns a data.table with all the rows for the animals
#'   included in \code{animalList}.\cr
#'   If no columns have been specified in \code{colList}, all the columns in
#'   the table \code{colList} are included.\cr
#'   If a list of columns have been specified in \code{colList}, these are
#'   included. In addition, a set of columns are always included, whether they
#'   are included in \code{colList} or not:
#'   \itemize{
#'     \item To ensure each row can be uniquely identified:
#'     \itemize{
#'        \item DOMAIN
#'        \item STUDYID
#'        \item USUBJID
#'        \item POOLID (if it exists)
#'        \item domainSEQ (if it exists)
#'     }
#'     \item For finding tables - to support age calculation and evaluation of
#'     study phase:
#'     \itemize{
#'        \item domainDTC
#'        \item domainDY
#'     }
#'   }
#'   The order of the columns are as they are defined for the domain in the
#'   SEND IG.\cr
#'   The data table contains both
#'   \itemize{
#'      \item subject level data - i.e. rows where USUBJID is not empty
#'      \item if applicable for the \code{domain}, pool level data - i.e. rows
#'      where POOLID is not empty.\cr
#'      In this case, all pools, which includes any of the subjects included in
#'      \code{animalList}, are included
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract all columns from DM:
#' getSubjData(myDbToken, myControlAnimals, 'dm')
#'
#' # Extract selected columns from LB:
#' getSubjData(myDbToken, myControlAnimals, 'LB',
#'             list('LBTESTCD', 'LBCAT',
#'                  'LBSTRESC', 'LBSTRESN', 'LBSTRESU',
#'                  'LBSTAT', 'LBREASND',
#'                  'LBTPT'))
#' }
#'
getSubjData <- function(dbToken,
                      animalList,
                      domain,
                      colList = NULL) {
  if (is.null(domain) | isTRUE(is.na(domain)) | isTRUE(domain=='')) {
    stop('Input parameter domain must have assigned a domain name ')
  }

  if (!data.table::is.data.table(animalList)) {
    stop('Input parameter animalList must have assigned a data table ')
  }

  # Verify that domain exists
  domain <- toupper(trimws(domain))
  if (!dbExistsTable(dbToken, domain))
    stop(sprintf("A table with name %s doesn't exist in the database", domain))
  # ... and contains a USUBJID col
  colListAll <- dbListFields(dbToken, domain)
  if ( ! 'USUBJID' %in% colListAll)
    stop(sprintf("Table %s doesn't contain a USUBJID column", domain))

  # Check list of columns to include in extraction
  if (isTRUE(length(colList) > 0)) {
    # A lists of columns has been specified
    colList <- toupper(trimws(colList))
    # - check if all columns a valid
    colListInvalid <- setdiff(colList, colListAll)
    if (length(colListInvalid) > 0)
      stop(sprintf("Table %s doesn't contain column(s): %s",
                   domain,
                   paste0(colListInvalid, collapse = ',')))

    # Ensure that all unique key columns are included:
    #  - DOMAIN, STUDYID, USUBJID
    colList <- as.character(colList)
    colList <- c(colList, 'DOMAIN', 'STUDYID', 'USUBJID')
    #  - and if exists: POOLID, <domain>SEQ
    if ('POOLID' %in% colListAll)
      colList <- c(colList, 'POOLID')
    if (paste0(domain,'SEQ') %in% colListAll)
      colList <- c(colList, paste0(domain,'SEQ'))

    # For findings tables - ensure the columns
    #  - <domain>DTC, <domain>DY
    # are included to support age calculation and evaluation of study phase
    if (paste0(domain,'TESTCD') %in% colListAll)
      colList <- c(colList, paste0(domain,'DTC'), paste0(domain,'DY'))

    # Limit set of extracted columns to the specified list
    # - ensure they are listed in the same order as defined in the table
    # - collapse list into a comma seperated string
    colListSelect <- paste0('"', colListAll[colListAll %in% colList],
                           collapse = ',',
                           '"')
  }
  else
    # Include all columns from data table
    colListSelect <- '*'

  # List of relevant studyid values
  studyList <- unique(animalList[,c('STUDYID')])

  # Extract subset of findings rows from db for relevant studies
  if(dbToken$dbType=='sqlite'){

  allData <- genericQuery(dbToken,
                          sprintf('select %s from "%s" where "STUDYID" in (?)',
                                  colListSelect, domain),
                          studyList)
  }else if(dbToken$dbType=='postgresql'){

  ## allData <- genericQuery(dbToken,
  query <- sprintf('select %s from "%s" where "STUDYID" in ($1)',
                                  colListSelect, domain)
    ## studyList)

    data <- RPostgres::dbGetQuery(dbToken$dbHandle,query,params=list(studyList$STUDYID))
    allData <- data.table::as.data.table(data)


  }

  # Extract subject level data for the input list of animals
  foundData <-
    data.table::merge.data.table(allData[!(is.null(USUBJID) | USUBJID == '')],
                                 animalList[,c('STUDYID', 'USUBJID')],
                                 by=c('STUDYID', 'USUBJID'))

  # Find pooled data if relevant
  if (dbExistsTable(dbToken, 'POOLDEF') &&
      'POOLID' %in% names(allData) &&
      length(allData[isTRUE(nchar(POOLID) > 0), c('POOLID')]) > 0) {
    # Extract pool level data for the input list of animals and add to
    # subject level set of found data
    if(dbToken$dbType=='sqlite'){
    foundData<-
      # extract subset of POOLDEF from db for relevant studies
      (genericQuery(dbToken,
                   "select studyid  as STUDYID
                          ,poolid   as POOLID
                          ,usubjid  as USUBJID
                      from pooldef
                     where studyid in (?)",
                  studyList) %>%
      # input list of animals is joined to POOLDEF to get the related POOLID values
      # - delete col USUBJID after join
      data.table::merge.data.table(animalList[,c('STUDYID', 'USUBJID')],
                                   by=c('STUDYID', 'USUBJID')))[,!"USUBJID"] %>%
      # get unique list of pools
      unique() %>%
      # extract pool level data from fetched data
      data.table::merge.data.table(allData[!(is.null(POOLID) | POOLID == '')],
                                   by=c('STUDYID', 'POOLID')) %>%
      # add rows to the extracted subject level data
      {data.table::rbindlist(list(foundData, .), use.names=TRUE, fill=TRUE)}


    }else if(dbToken$dbType=='postgresql'){

   foundData<-
      (DBI::dbGetQuery(dbToken$dbHandle,
                   'select "STUDYID"  as "STUDYID"
                          ,"POOLID"   as "POOLID"
                          ,"USUBJID"  as "USUBJID"
                      from "POOLDEF"
                     where "STUDYID" in ($1)',
                   params=list(studyList$STUDYID)) %>%
       data.table::as.data.table() %>%
      # input list of animals is joined to POOLDEF to get the related POOLID values
      # - delete col USUBJID after join
      data.table::merge.data.table(animalList[,c('STUDYID', 'USUBJID')],
                                   by=c('STUDYID', 'USUBJID')))[,!"USUBJID"] %>%
      # get unique list of pools
      unique() %>%
      # extract pool level data from fetched data
      data.table::merge.data.table(allData[!(is.null(POOLID) | POOLID == '')],
                                   by=c('STUDYID', 'POOLID')) %>%
      # add rows to the extracted subject level data
      {data.table::rbindlist(list(foundData, .), use.names=TRUE, fill=TRUE)}


    }
   }

  # Return the found rows with columns order as define in SEND IG
  data.table::setorderv(sendIGcolumns[TABLE_NAME == domain &
                                        COLUMN_NAME %in% names(foundData)],
                        "SEQ")$COLUMN_NAME %>%
    data.table::setcolorder(foundData, .)
}


