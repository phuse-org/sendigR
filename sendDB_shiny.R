################################################################################
## A set of internal functions for extraction and manipulation of data for the
## Shiny app
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2021-04-28   Yousuf Ali,           Initial version
##              Daniel Russo
##              Bo Larsen
################################################################################


GetAnimalList <- function(design, species) {
  # helper function tjust used for troubleshooting
  # to get a quick list of animals.
  species <- sendigR::genericQuery(.sendigRenv$dbToken,
                                   sprintf('SELECT STUDYID
                                            FROM TS
                                            WHERE TSPARMCD == "SPECIES"
                                            AND TSVAL == "%s" ', species))
  design <- sendigR::genericQuery(.sendigRenv$dbToken,
                                  sprintf('SELECT STUDYID
                                           FROM TS
                                           WHERE TSPARMCD == "SDESIGN"
                                           AND TSVAL == "%s" ', design))

  studies <- merge(species, design, by='STUDYID')

  controls <- sendigR::getControlSubj(.sendigRenv$dbToken, studyList = studies)
  animals <- merge(studies, controls, by='STUDYID')
  return(animals)
}

MiFindings <- function(animalList, mispec) {
  # given a set of USUBJIDs and and target organ
  # will return the frequency counts of counts
  # of the findings

  # query MI and remove findings not in
  # our target animals. Convert
  # all findings to uppercase for
  # counting.
  findings <- sendigR::genericQuery(.sendigRenv$dbToken,
                                    sprintf('SELECT STUDYID, USUBJID, MISTRESC
                                            FROM MI
                                            WHERE MISPEC == "%s"', mispec))
  finalFindings <- merge(animalList, findings,
                         by=c('STUDYID', 'USUBJID'))
  finalFindings$MISTRESC <- toupper(finalFindings$MISTRESC)

  # Count findings using dplyr
  findingsCount <- finalFindings %>%
    dplyr::distinct(STUDYID, USUBJID, MISTRESC) %>% # only one organ, finding per animal (input errors cause duplications)
    dplyr::count(MISTRESC) %>%
    dplyr::arrange(-n)

  # findings = n / total animals * 100
  # round to 2 decimal places and
  # sort by descending.
  findingsCount$Incidence <- (findingsCount$n / length(unique(finalFindings$USUBJID))) * 100
  findingsCount$Incidence <- paste0(round(findingsCount$Incidence, 2), '%')
  findingsCount <- dplyr::select(findingsCount, -n)
  return(findingsCount)
}


LiverFindings <- function(animalList, lbtestcd, how='max') {
  # given a set of USUBJIDs and and target LBTESTCD
  # will return either the max or mean responses
  # for all the animals.

  # set function to either
  # max or mean
  if (tolower(how) == 'max') {
    fx = max
  } else {
    fx = mean
  }

  # use a list to filter
  # valid test units per
  # each LBTESTCD
  LAB_TEST_UNITS <- list(
    ALP = c('U/L', 'IU/L'),
    ALT = c('U/L', 'IU/L'),
    AST = c('U/L', 'IU/L'),
    BILEAC = c('umol/L', 'mmol/L'),
    BILI = c('umol/L', 'mg/dL'),
    GGT = c('U/L'),
    GLDH = c('U/L'),
    SDH = c('U/L')
  )

  # only get valid units
  validUnits <- LAB_TEST_UNITS[[lbtestcd]]

  # parameters vary per lbtestcd
  # need to get specific number
  questionMarks <- paste(rep('?', length(validUnits)), collapse=', ')
  queryString <- sprintf('SELECT STUDYID, USUBJID, LBSTRESC, LBSTRESU
                          FROM LB
                          WHERE LBTESTCD == ?
                            and LBSTRESU IN (%s)', questionMarks)

  params <- c(lbtestcd, validUnits)

  results <- sendigR::genericQuery(.sendigRenv$dbToken, queryString, params)
  results <- merge(results, animalList, by=c('STUDYID', 'USUBJID'))

  # strip and remove equality signs
  results$LBSTRESC <- as.numeric(gsub("[^0-9.-]", "", results$LBSTRESC))
  results <- results[!is.na(results$LBSTRESC),]

  # convert BILI to from umol/L
  # to ml/dL
  if ((lbtestcd == 'BILI') & (any(results$LBSTRESU == 'mg/dL'))) {
    results[results$LBSTRESU == 'mg/dL',]$LBSTRESC <-
                          results[results$LBSTRESU == 'mg/dL',]$LBSTRESC * 17.1
    results[results$LBSTRESU == 'mg/dL',]$LBSTRESU <- 'umol/L'
  }

  # use dplyr to take max
  # or mean and return results
  finalResults <- results %>%
    dplyr::group_by(STUDYID, USUBJID) %>%
    dplyr::mutate(LBSTRESC_TRANS = fx(as.numeric(LBSTRESC))) %>%
    unique()
  return(finalResults)

}

# TODO: find a better analysis for
# BW or remove.
BodyWeight <- function(animalList) {

  results <-
    sendigR::genericQuery(.sendigRenv$dbToken,
                          'SELECT STUDYID, USUBJID, BWSTRESN, BWSTRESU
                           FROM BW')


  results <- merge(results, animalList, by=c('STUDYID', 'USUBJID'))
  results$BWSTRESN[results$BWSTRESU == 'kg']  <-
                        results$BWSTRESN[results$BWSTRESU == 'kg'] * 1000
  results$BWSTRESU[results$BWSTRESU == 'kg']  <- 'g'

  results <- results %>%
    dplyr::group_by(STUDYID, USUBJID) %>%
    dplyr::mutate(days = 1:dplyr::n() / dplyr::n())
  return(results)

}


# Get the minimum study start date in ts
getMinStudyStartDate <- function() {
  min(parsedate::parse_iso_8601(sendigR::genericQuery(.sendigRenv$dbToken,
                                                      "select distinct tsval
                                                       from ts
                                                       where upper(tsparmcd) = 'STSTDTC'")$TSVAL),
      na.rm = TRUE)
}

# series of functions to query the
# database to find unique elements.
GetUniqueDesign <- function() {
  uniqueDesigns <- toupper(sendigR::genericQuery(.sendigRenv$dbToken,
                                                 'SELECT DISTINCT TSVAL
                                                  FROM TS
                                                  WHERE upper(TSPARMCD) = "SDESIGN"')$TSVAL)
  return(unique(uniqueDesigns))
}


GetUniqueSpecies <- function() {
  toupper(sendigR::genericQuery(.sendigRenv$dbToken, "select distinct tsval as SPECIES
                          from ts
                         where upper(tsparmcd) = 'SPECIES'
                        union
                        select distinct txval as SPECIES
                          from tx
                         where upper(txparmcd) = 'SPECIES'
                        union
                        select distinct SPECIES
                          from dm
                         order by SPECIES")$SPECIES)
}

GetUniqueStrains <- function(species) {
  if (length(species) == 1) {
    # 1 species selected
    # - select strain values without any prefixes
    selectStrTS <- "ts1.tsval"
    selectStrTX <- "tx1.txval"
    selectStrDM <- "strain"
  }
  else {
    # Multiple species selected
    # - select strain values prefixed with respective species value
    selectStrTS <- "ts2.tsval || ': ' || ts1.tsval"
    selectStrTX <- "tx2.txval || ': ' || tx1.txval"
    selectStrDM <- "species || ': ' || strain"
  }

  return(sort(toupper(
    sendigR::genericQuery(.sendigRenv$dbToken, sprintf("select distinct %s as STRAIN
                            from ts ts1
                            join ts ts2
                              on upper(ts2.tsparmcd) = 'SPECIES'
                             and upper(ts2.tsval) in (:1)
                             and ts1.tsgrpid = ts2.tsgrpid
                             and ts1.studyid = ts2.studyid
                           where upper(ts1.tsparmcd) = 'STRAIN'
                          union
                         select distinct trim(%s) as STRAIN
                            from tx tx1
                            join tx tx2
                              on upper(tx2.txparmcd) = 'SPECIES'
                             and upper(tx2.txval) in (:1)
                             and tx1.setcd = tx2.setcd
                             and tx1.studyid = tx2.studyid
                           where upper(tx1.txparmcd) = 'STRAIN'
                         union
                         select distinct trim(%s)
                           from dm
                          where species in (:1)",
                         selectStrTS,
                         selectStrTX,
                         selectStrDM),
                 species)$STRAIN)))
}


GetUniqueRoutes <- function() {
  toupper(sendigR::genericQuery(.sendigRenv$dbToken, "select distinct tsval as ROUTE
                           from ts
                          where upper(tsparmcd) = 'ROUTE'
                         union
                         select distinct exroute as ROUTE
                           from ex
                          order by ROUTE")$ROUTE)
}

GetUniqueOrgans <- function() {
  uniqueOrgans <- toupper(sendigR::genericQuery(.sendigRenv$dbToken, 'SELECT DISTINCT MISPEC
                                                          FROM MI')$MISPEC)
  return(unique(uniqueOrgans))
}

GetUniqueLBTESTCD <- function(cat) {
  uniqueLBTESTCD <- toupper(sendigR::genericQuery(.sendigRenv$dbToken,
                                                  'SELECT DISTINCT LBTESTCD
                                                   FROM LB
                                                   WHERE LBCAT = ?', c(cat))$LBTESTCD)
  return(unique(uniqueLBTESTCD))
}

GetAvailableStudies <- function() {
  uniqueStudies <- sendigR::genericQuery(.sendigRenv$dbToken,
                                         'SELECT DISTINCT STUDYID
                                          FROM TS')$STUDYID
  return(uniqueStudies)
}

GetStudyTS <- function(studyid) {
  studyInfo <- sendigR::genericQuery(.sendigRenv$dbToken,
                                     'SELECT *
                                      FROM TS
                                      WHERE STUDYID = :1', c(studyid))
  return(studyInfo)
}

GetAnimalGroupsStudy <- function(studyid) {
  studyAnimals <- sendigR::genericQuery(.sendigRenv$dbToken,
                                        'SELECT TX.STUDYID, USUBJID, TX.SETCD, "SET"
                                         FROM TX
                                         INNER JOIN DM
                                            on DM.SETCD = TX.SETCD
                                           AND DM.STUDYID = TX.STUDYID
                                         WHERE DM.STUDYID = ?', c(studyid))
  return(studyAnimals)
}

aggDomain <- function(domainData, grpByCols, includeUncertain=TRUE) {
  # creates an aggregate table from domainData
  # domainData: should be a data.table that with
  # both domain data (e.g., MI) merged with animal
  # meta data.  In the case of the R Shiny App
  # this is the result of the animalList() reactive
  # groByCols is a vector of column names in
  # domainData for which to summarize stats.

  # group by counts and get
  # the number of incidence per
  # group
  aggData <- domainData %>%
    dplyr::group_by_at(grpByCols) %>%
    dplyr::summarize(N = dplyr::n())

  # if include uncertain is not
  # selected, we dont need to calc.
  # the differences in non confident
  # matches.

  if (!includeUncertain) {
    return(aggData)
  }

  # do the same for non confident
  # matches, which are rows with
  # no UNCERTAIN_MSG
  aggDataNonConf <- domainData %>%
    dplyr::filter(!is.na(UNCERTAIN_MSG)) %>%
    dplyr::group_by_at(grpByCols) %>%
    dplyr::summarize(Uncertain.Matches = dplyr::n())

  # if no UNCERTAIN_MSG, that is
  # a confident match.

  aggDataConf <- domainData %>%
    dplyr::filter(is.na(UNCERTAIN_MSG)) %>%
    dplyr::group_by_at(grpByCols) %>%
    dplyr::summarize(Certain.Matches = dplyr::n())

  # some groups by have no or all confidence
  # matches.  Need to to an outer join and
  # replace these as 0s
  df <- merge(aggData, aggDataConf, by=grpByCols, all=TRUE)
  df <- merge(df, aggDataNonConf, by=grpByCols, all=TRUE)
  for(j in seq_along(df)){
    data.table::set(df, i = which(is.na(df[[j]]) & is.numeric(df[[j]])), j = j, value = 0)
  }

  return(df)

}

# function for Aggregate BW and LB domain
# control animal list and domain subject data merged to create doaminData
#domain should be "lb" or "bw"
aggDomain_bw_lb <- function(domainData, domain, includeUncertain=F) {
  
  domain <- tolower(domain)
  
  if (domain=='bw') {
    grpByCols <- c("AGEDAYS_BW_AGE","SEX","BWORRESU")
    result <- 'BWSTRESN'
    result_unit <- 'BWORRESU'
    
  } else if (domain=='lb') {
    grpByCols <- c("LBSPEC","LBTESTCD", "LBTEST","SEX","LBSTRESU")
    result <- 'LBSTRESN'
    result_unit <- 'LBSTRESU'
    
  }
  mean_result <- paste0('Mean_',result)
  sd_result <- paste0('SD_',result)
  
  if (includeUncertain==F) {
    
    agg_tb_certain <- domainData%>%
      dplyr::group_by_at(grpByCols) %>% 
      dplyr::summarize(!!mean_result := mean(get(result)),
                       !!sd_result := sd(get(result)),
                       N = n())
    agg_tb_certain <- dplyr::relocate(agg_tb_certain,{{result_unit}}, .after = (!!sd_result))
    
    return(agg_tb_certain)
  } else if (includeUncertain==T) {
    
    agg_tb_uncer <- domainData%>%
      dplyr::group_by_at(grpByCols) %>% 
      dplyr::summarize(!!mean_result := mean(get(result)),
                       !!sd_result := sd(get(result)),
                       N = n())
    
    aggDataNonConf <- domainData%>% 
      dplyr::filter(!is.na(UNCERTAIN_MSG)) %>% 
      dplyr::group_by_at(grpByCols) %>%
      dplyr::summarize(Uncertain.Matches = n())
    
    aggDataConf <- domainData%>% 
      dplyr::filter(is.na(UNCERTAIN_MSG)) %>%
      dplyr::group_by_at(grpByCols) %>%
      dplyr::summarize(Certain.Matches = n()) 
    
    df <- merge(agg_tb_uncer, aggDataConf, by=grpByCols, all=TRUE)
    df <- merge(df, aggDataNonConf, by=grpByCols, all=TRUE)
    df <- dplyr::relocate(df,{{result_unit}}, .after = {{sd_result}})
    
    for(j in seq_along(df)){
      data.table::set(df, i = which(is.na(df[[j]]) & is.numeric(df[[j]])), j = j, value = 0)
    }
    
    return(df)
    
  }
}

################################################################################
# Avoid  'no visible binding for global variable' notes from check of package:
MISTRESC <- LBSTRESC <- NULL
n <- NULL
