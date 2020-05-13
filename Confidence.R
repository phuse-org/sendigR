# Script for computing 
# the confidence values
# Confidence values for Table X.B in the 
# manuscript.
#confidence is defined as 
# conf = 100 * (confidently-matching records + confidently non-matching records) / total number of records 
#
# Each query block should be able to run 
# so long as the code up until the first
# header is run before.  
#
source("sysParameters.R")
source("studyListStudyStartDate.R")
source("importSENDDomains.R")
source("studyListStudyDesign.R")
source("animalListControl.R")
source("addFindingsAnimalAge.R")

library(dplyr)

# control terminology list need for 
# a variety of different query blocks
cont_terms <- readxl::read_excel('data/SEND Terminology 2019-09-27.xls', 
                                 sheet = "SEND Terminology 2019-09-27")[['CDISC Submission Value']]

# query parameters for our question

design <- "PARALLEL"
route1 <- "ORAL"
route2 <- "ORAL GAVAGE"
sex <- "M"
species <- "DOG"
strain <- "BEAGLE"
startdate <- "2017"
enddate <- "2020"
sex <- "M"
lowerBoundAge <- '12m'
upperBoundAge <- '18m'
phase <- "Treatment"



#########################################
#
#  QUERY BLOCK -- STUDY START DATE
#
#########################################

db <- dbConnect(RSQLite::SQLite(), dbFullName)
ts_date <- data.table(unique(dbGetQuery(db, 'SELECT STUDYID, TSPARMCD, TSVAL FROM TS WHERE TSPARMCD = "STSTDTC"')))
dbDisconnect(db)


# confidently matching is
# just the number of studies
# in the specified date interval
conf_matching_start <- unique(GetStudyListSTSTDTC(fromDTC=startdate, toDTC=enddate))

# any studies not found by 
# our algorithm are nonmatching
non_matching_start <- ts_date[(!(ts_date$STUDYID %in% conf_matching_start$STUDYID)),]
conf_non_matching <- sum(!is.na(parse_iso_8601(non_matching_start$TSVAL)))

# 94 values are confidently non-matching
# 1490 records are confidently matching
# 1590 records have a STSTDTC


#########################################
#
# QUERY BLOCK -- Route of Administration 
#
#########################################


# Animals for which a route of administration 
# can be determined from either the TS parameter
# ROUTE or from the EXROUTE variable in the EX domain, 
# but only if the route matches a term on the
# controlled terminology list

# to count the number of animals with that have 
# a valid route, we need to to replace all the 
# missing values in Ex, with those from TS
# in order to do this, we create a second data
# of animals with their routes specified in TS
# however, we can't do

db <- dbConnect(RSQLite::SQLite(), dbFullName)
ex <- unique(data.table(dbGetQuery(db, "SELECT STUDYID, USUBJID, EXROUTE FROM EX")))
ts_route <- data.table(unique(dbGetQuery(db, 'SELECT STUDYID, TSPARMCD, TSVAL FROM TS WHERE TSPARMCD = "ROUTE"')))
dbDisconnect(db)

ts_route <- ts_route[,.(STUDYID, EXROUTE=toupper(TSVAL))]
ex$EXROUTE <- toupper(ex$EXROUTE)

ts_route <- ts_route %>%
              dplyr::group_by(STUDYID) %>%
              dplyr::mutate(NUM_ROUTE = n()) %>%
              dplyr::ungroup()


ts_route$EXROUTE[ts_route$NUM_ROUTE > 1] <- NA              
ts_route <- unique(ts_route)
ts_animals <- merge(ts_route[,c("STUDYID", "EXROUTE")], ex[,c("STUDYID", "USUBJID")])

merged <- merge(ex, ts_animals, by=c('STUDYID', 'USUBJID'))

final_route <- merged %>%
        mutate(ROUTE = toupper(coalesce(EXROUTE.x, EXROUTE.y))) %>%
        select(-EXROUTE.x, -EXROUTE.y) %>%
        unique()

conf_matching_route <- final_route[final_route$ROUTE == route1 | final_route$ROUTE == route2,]
not_matching_route <- final_route[!final_route$USUBJID %in% conf_matching_route$USUBJID,]
not_matching_conf <- not_matching_route[not_matching_route$ROUTE %in% cont_terms,]

#########################################
#
# QUERY BLOCK -- Study Design 
#
#########################################

# confidence is defined as records matching
# the controlled terms.  confidence not
# matching, if more than one design, they
# all have to by matching controlled term


db <- dbConnect(RSQLite::SQLite(), dbFullName)
ts_design <- data.table(unique(dbGetQuery(db, 'SELECT STUDYID, TSPARMCD, TSVAL FROM TS WHERE TSPARMCD = "SDESIGN"')))
dbDisconnect(db)

conf_matching_design <- GetStudyListSDESIGN(design)
non_matching_design <- ts_design[(!(ts_design$STUDYID %in% conf_matching_design$STUDYID)), .(STUDYID, SDESIGN = TSVAL)]
non_matching_design$is_controlled <- toupper(non_matching_design$SDESIGN) %in% cont_terms

non_matching_design_unique <- non_matching_design %>%
  dplyr::group_by(STUDYID) %>%
  dplyr::mutate(all_true = dplyr::if_else(sum(is_controlled) == dplyr::n(), TRUE, FALSE)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(STUDYID, all_true)

conf_non_matching_design <- non_matching_design_unique[non_matching_design_unique$all_true,]


#########################################
#
# QUERY BLOCK -- Species/Strain 
#
#########################################


# Animals for which a route of administration 
# can be determined from either the TS parameter
# ROUTE or from the EXROUTE variable in the EX domain, 
# but only if the route matches a term on the
# controlled terminology list

# TX is not used, because in our database 
# all animals have species info in TS or DM
db <- dbConnect(RSQLite::SQLite(), dbFullName)
dm <- dbGetQuery(db, "SELECT STUDYID, USUBJID, ARMCD, SETCD, SPECIES, STRAIN, SEX FROM DM")
animals <- unique(dbGetQuery(db, 'SELECT STUDYID, TSPARMCD, TSVAL FROM TS WHERE TSPARMCD = "SPECIES" or TSPARMCD = "STRAIN"'))
dbDisconnect(db)
# dcast requires every row->col pair to be unique
# in order to cast.  however, some studies have
# multiple strains.  In order to cast, need to 
# create a unique (STUDYID, ID) combination 
animals['ID'] <- rowidv(animals, cols=c("STUDYID", "TSPARMCD"))
animals <- reshape2::dcast(animals, STUDYID+ID~TSPARMCD, value.var='TSVAL')

# merging dm and the results from 
# ts creates a new dataframe with 
# two columns corresponding to SPECIES 
# and STRAIN. SPECIES.x and STRAIN.x
# come from the dm domain and SPECIES.y
# STRAIN.y come from the TS domain

merged_dm_ts <- merge(dm, animals, by='STUDYID')

# replace emtpy strings with NA values
# this allows the coalesce function to 
# replace missing values from the left
# SPECIES/STRAIN columns (those from 
# DM) and impute them with values from
# the right SPECIES/STRAIN columns 

merged_dm_ts[merged_dm_ts == ""] <- NA
imputed_dm <- merged_dm_ts %>%
                dplyr::mutate(SPECIES = dplyr::coalesce(SPECIES.x, SPECIES.y)) %>% # fill missing values in SPECIES.x with SPECIES.y in new column called SPECIES
                dplyr::mutate(STRAIN = dplyr::coalesce(STRAIN.x, STRAIN.y)) %>%
                dplyr::select(-SPECIES.x, -SPECIES.y, -STRAIN.x, -STRAIN.y)


# studies that have multiple strains
# will only have the species populated
# from TS in one of the studyid, which 
# because of the pipe above will result
# in a frame like so.....
# STUDYID SPECIES STRAIN
#       1     DOG BEAGLE
#       2     RAT SPRAGUE
#       2     NA  WISTAR
#
# so we need to fill these to extend
# imputed these by grouping by study

imputed_dm <- imputed_dm %>%
                dplyr::group_by(STUDYID) %>%
                tidyr::fill(SPECIES) %>% #default direction down
                tidyr::fill(SPECIES, .direction = "up") %>%
                dplyr::ungroup() 
imputed_dm <- imputed_dm[!duplicated(imputed_dm[c('STUDYID', 'USUBJID')]),]

imputed_dm$SPECIES <- toupper(imputed_dm$SPECIES)
imputed_dm$STRAIN <- toupper(imputed_dm$STRAIN)

conf_match_species <- imputed_dm[imputed_dm$SPECIES == species,]
not_matching_species <- imputed_dm[imputed_dm$SPECIES != species,]
conf_not_matching_species <- not_matching_species[not_matching_species$SPECIES %in% cont_terms,]

conf_match_strain <- imputed_dm[imputed_dm$STRAIN == strain,]
not_matching_strain <- imputed_dm[imputed_dm$STRAIN != strain,]
conf_not_matching_strain <- not_matching_strain[not_matching_strain$STRAIN %in% cont_terms,]

#########################################
#
# QUERY BLOCK -- Control Animals 
#
#########################################


# Control animals are considered confidently
# matching if they fall in a set considered
# by our aglorithm as likely a negative 
# control.  Confidently non matching are 
# only those that fall into a set labeled 
# explicitly as positive controls.

db <- dbConnect(RSQLite::SQLite(), dbFullName)
dm <- unique(dbGetQuery(db, "SELECT STUDYID, USUBJID, SETCD FROM DM"))
tx <- unique(dbGetQuery(db, "SELECT STUDYID, SETCD, TXVAL FROM TX WHERE TXPARMCD == 'TCNTRL'"))
dbDisconnect(db)

dm_with_setcd <- merge(dm, tx)

# find animals not idenified as 
# negative control but have
# POSITIVE CONTROL specified as 
# their tcntrl value
conf_matching_control <- GetControlAnimals()
not_matching_control <- dm_with_setcd[!(dm_with_setcd$USUBJID %in% conf_matching_control$STUDYID),]
not_matching_control$TXVAL <- toupper(not_matching_control$TXVAL)
conf_not_matching_control <- not_matching_control[not_matching_control$TXVAL ==  'POSITIVE CONTROL',]


#########################################
#
# QUERY BLOCK -- 11 Sex
#
#########################################


db <- dbConnect(RSQLite::SQLite(), dbFullName)
dm <- unique(dbGetQuery(db, "SELECT STUDYID, USUBJID, SEX FROM DM"))
dbDisconnect(db)

# gives us frequency counts of 
# sex in DM
table(dm$SEX, useNA='always')


#########################################
#
# QUERY BLOCK -- findings records AGE
#
#########################################

# confident matches, are those that have 
# an age being able to be calculated by 
# our algorith, condient non matches are
# those with a valid age, but not in the 
# interval we are looking for.  non-confident
# are those where an age could no be calcuate
# ie., NA values 


db <- dbConnect(RSQLite::SQLite(), dbFullName)
mi <- data.table(unique(dbGetQuery(db, "SELECT * FROM MI")))
dbDisconnect(db)

mi_with_age <- unique(addFindingsAnimalAge('mi', mi))

# our query asks for findings between 
# 12 months and 18 months at time of
# finding
lower_age <- 12*30
upper_age <- 18*30

conf_matches_age <- mi_with_age[((mi_with_age$AGE >= lower_age) & (mi_with_age$AGE <= upper_age)),]
conf_not_matching_age <- mi_with_age[((mi_with_age$AGE < lower_age) | (mi_with_age$AGE > upper_age)),]



#########################################
#
# QUERY BLOCK -- 3.1 terminology MI Findings
#
#########################################


# we dont have a specific query for 
# findings, so just suppose the case we 
# were looking for the most populated finding
# at the end of query block, which would be
# cysts in the pituitary gland.  For this, 
# confidently matching would be those matching
# this orgam/finding pair.  Confidently non-matching 
# would be those 

finding <- 'CYST'
organ <- 'GLAND, PITUITARY'

# Confident matches are
db <- dbConnect(RSQLite::SQLite(), dbFullName)
mifindings <- unique(dbGetQuery(db, "SELECT * FROM MI"))
tsfindings <- dbGetQuery(db, "SELECT STUDYID, TSPARMCD, TSVAL FROM TS WHERE TSPARMCD = 'SNDIGVER' ")

dbDisconnect(db)

studies_31 <-  tsfindings$STUDYID[grepl("3.1", tsfindings$TSVAL)]
findings_31 <- mifindings[mifindings$STUDYID %in% studies_31,]

findings_31$MISTRESC <- toupper(findings_31$MISTRESC)
findings_31$MISPEC <- toupper(findings_31$MISPEC)

conf_matches_findings <- findings_31[((findings_31$MISPEC == organ) & (findings_31$MISTRESC == finding)),]
not_matching_findings <- findings_31[!((findings_31$MISPEC == organ) & (findings_31$MISTRESC == finding)),]

# both MISPEC and MISTRESC should be 
# in controlled terms...

conf_non_matches_findings <- not_matching_findings[((not_matching_findings$MISPEC %in% cont_terms) & 
                                                      (not_matching_findings$MISTRESC %in% cont_terms)),]

