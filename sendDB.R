# Following R style conventions:
#     https://google.github.io/styleguide/Rguide.html
#     http://adv-r.had.co.nz/Style.html
# 
# 
# This module contains a bunch of
# functions to support some SEND db
# data analysis and manipulation. 


library(dplyr)
library(data.table)
library(parsedate)
library(ini)


## Initiate SEND package functions and DB connection

# Import parameter file from same folder as program is located
iniParmsFile <- 'sendDashboard.ini'
if (! file.exists(iniParmsFile))
  stop(sprintf('The init file %s cannot be found in %s',iniParmsFile, getwd()))
       
iniParms<-read.ini(iniParmsFile)
iniDbType <- iniParms$`database`$`dbType`   ## add check for valid dbType
iniDbPath <- iniParms$`database`$`dbPath`
iniDbSchema <- iniParms$`database`$`dbSchema`
iniCtFile <- iniParms$`ct`$`ctFile`
if (is.null(iniDbType) | is.null(iniDbPath) | is.null(iniCtFile))
  stop(sprintf('All of these parameters in %s/%s must have assigned a value: dbType,dbPath, ctFile', 
               getwd(),iniParmsFile))

# Execute the SEND function initiation
# TODO: Is is okay to have an open DB connection
# throughout the app?  Is there a way to ensure
# it gets closed?
dbToken <- sendigR::initEnvironment(dbType = iniDbType, 
                dbPath = iniDbPath, 
                ##!! To do: add handling of getting username/password for relevant database types..
                # dbUser = <user>, dbPwd= <pwd>, 
                dbSchema = iniDbSchema,
                ctFile = iniCtFile)

# exhaustive list of domains outlined in 
# IG 3.1
domains <- c(
  'DM', # demographics
  'SE', # subject elements
  'CO', # comments
  'EX', # exposure
  'DS', # disposition
  'BW', # body weight
  'BG', # body weight gain
  'CL', # clinical observations
  'DD', # death diagnosis and detaisl
  'FW', # food water and consumption
  'LB', # laboratory test results
  'MA', # macroscopic findings
  'MI', # microscopic findings
  'OM', # organ measurements
  'PM', # palpable masses
  'PC', # pharmacokinetics concentrations
  'PP', # pharmacokinetics parameters
  'SC', # subjects characteristics
  'TF', # tumor findings
  'VS', # vital signs
  'EG', # ECG test results
  'CV', # cardiovascular test results
  'RE', # respiratory test results
  'TE', # trial elements
  'TX', # trial sets
  'TA', # trial arms
  'TS', # trial summary
  'RELREC', # related records
  'POOLDEF' # pool definition
)

supp_domains <- paste("SUPP", domains, sep="")
all_domains <- c(domains, supp_domains)


GetAnimalList <- function(design, species) {
  # helper function tjust used for troubleshooting
  # to get a quick list of animals. 
  species = data.table(sendigR::genericQuery(dbToken, sprintf('SELECT STUDYID FROM TS WHERE TSPARMCD == "SPECIES" AND TSVAL == "%s" ', species)))
  design = data.table(sendigR::genericQuery(dbToken, sprintf('SELECT STUDYID FROM TS WHERE TSPARMCD == "SDESIGN" AND TSVAL == "%s" ', design)))
  
  studies <- merge(species, design, by='STUDYID')
  
  controls <- GetControlAnimals(studyList = studies)
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
  findings <- data.table(sendigR::genericQuery(dbToken, sprintf('SELECT STUDYID, USUBJID, MISTRESC FROM MI WHERE MISPEC == "%s"', mispec)))
  finalFindings <- merge(animalList, findings, by=c('STUDYID', 'USUBJID'))
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
  # will return either the max or mean respones
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
  queryString <- sprintf('SELECT STUDYID, USUBJID, LBSTRESC, LBSTRESU FROM LB WHERE LBTESTCD == ? and LBSTRESU IN (%s)', questionMarks)
  
  params <- c(lbtestcd, validUnits)
  
  results <- sendigR::genericQuery(dbToken, queryString, params)
  results <- merge(results, animalList, by=c('STUDYID', 'USUBJID'))
  
  # strip and remove equality signs
  results$LBSTRESC <- as.numeric(gsub("[^0-9.-]", "", results$LBSTRESC))
  results <- results[!is.na(results$LBSTRESC),]
  
  # convert BILI to from umol/L 
  # to ml/dL 
  if ((lbtestcd == 'BILI') & (any(results$LBSTRESU == 'mg/dL'))) {
    results[results$LBSTRESU == 'mg/dL',]$LBSTRESC <- results[results$LBSTRESU == 'mg/dL',]$LBSTRESC * 17.1
    results[results$LBSTRESU == 'mg/dL',]$LBSTRESU <- 'umol/L'
  }
  
  # use dplyr to take max
  # or mean and return results
  finalResults <- results %>%
    group_by(STUDYID, USUBJID) %>% 
    mutate(LBSTRESC_TRANS = fx(as.numeric(LBSTRESC))) %>%
    unique()
  return(finalResults)
  
}

# TODO: find a better analysis for 
# BW or remove. 
BodyWeight <- function(animalList) {

  results <- data.table(sendigR::genericQuery(dbToken, 'SELECT STUDYID, USUBJID, BWSTRESN, BWSTRESU FROM BW'))
  
  
  results <- merge(results, animalList, by=c('STUDYID', 'USUBJID'))
  results$BWSTRESN[results$BWSTRESU == 'kg']  <- results$BWSTRESN[results$BWSTRESU == 'kg'] * 1000
  results$BWSTRESU[results$BWSTRESU == 'kg']  <- 'g'
  
  results <- results %>%
    group_by(STUDYID, USUBJID) %>%
    mutate(days = 1:n() / n())
  return(results)
  
}


# Get the minimum study start date in ts
getMinStudyStartDate<-function() {
  min(parse_iso_8601(sendigR::genericQuery(dbToken, "select distinct tsval
                                                from ts
                                               where upper(tsparmcd) = 'STSTDTC'")$TSVAL),
      na.rm = TRUE)
}

# series of functions to query the 
# database to find unique elements. 
GetUniqueDesign <- function() {
  uniqueDesigns <- toupper(sendigR::genericQuery(dbToken, 'SELECT DISTINCT TSVAL FROM TS WHERE upper(TSPARMCD) = "SDESIGN"')$TSVAL)
  return(unique(uniqueDesigns))
}


GetUniqueSpecies <- function() {
  toupper(sendigR::genericQuery(dbToken, "select distinct tsval as SPECIES
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
    sendigR::genericQuery(dbToken, sprintf("select distinct %s as STRAIN
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
  toupper(sendigR::genericQuery(dbToken, "select distinct tsval as ROUTE
                           from ts 
                          where upper(tsparmcd) = 'ROUTE'
                         union 
                         select distinct exroute as ROUTE
                           from ex
                          order by ROUTE")$ROUTE)
}

GetUniqueOrgans <- function() {
  uniqueOrgans <- toupper(sendigR::genericQuery(dbToken, 'SELECT DISTINCT MISPEC FROM MI')$MISPEC)
  return(unique(uniqueOrgans))
}

GetUniqueLBTESTCD <- function(cat) {
  uniqueLBTESTCD <- toupper(sendigR::genericQuery(dbToken, 'SELECT DISTINCT LBTESTCD FROM LB WHERE LBCAT = ?', c(cat))$LBTESTCD)
  return(unique(uniqueLBTESTCD))
}

GetAvailableStudies <- function() {
  uniqueStudies <- sendigR::genericQuery(dbToken, 'SELECT DISTINCT STUDYID FROM TS')$STUDYID
  return(uniqueStudies)
}

GetStudyTS <- function(studyid) {
  studyInfo <- sendigR::genericQuery(dbToken, 'SELECT * FROM TS WHERE STUDYID = :1', c(studyid))
  return(studyInfo)
}

GetAnimalGroupsStudy <- function(studyid) {
  studyAnimals <- sendigR::genericQuery(dbToken, 'SELECT TX.STUDYID, USUBJID, TX.SETCD, "SET" FROM 
                               TX  
                               INNER JOIN DM 
                               on DM.SETCD = TX.SETCD AND DM.STUDYID = TX.STUDYID   
                               WHERE DM.STUDYID = ?', c(studyid))
  return(studyAnimals)
}

aggDomain <- function(domainData, grpByCols) {
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
    group_by_at(grpByCols) %>%
    summarize(N = n())
  
  # do the same for non confident
  # matches, which are rows with 
  # no UNCERTAIN_MSG
  aggDataNonConf <- domainData %>% 
    filter(!is.na(UNCERTAIN_MSG)) %>%
    group_by_at(grpByCols) %>%
    summarize(NonConF = n())
  
  # if no UNCERTAIN_MSG, that is
  # a confident match. 
  
  aggDataConf <- domainData %>% 
    filter(is.na(UNCERTAIN_MSG)) %>%
    group_by_at(grpByCols) %>%
    summarize(ConfN = n())  
  
  # some groups by have no or all confidence
  # mataches.  Need to to an outer join and 
  # replace these as 0s
  df <- merge(aggData, aggDataConf, by=grpByCols, all=TRUE)
  df <- merge(df, aggDataNonConf, by=grpByCols, all=TRUE)
  for(j in seq_along(df)){
    set(df, i = which(is.na(df[[j]]) & is.numeric(df[[j]])), j = j, value = 0)
  }
  
  return(df)
  
}
