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
source("importSENDDomains.R")
source("studyListStudyDesign.R")
source("filterStudyAnimalSpeciesStrain.R")
source("filterStudyAnimalRoute.R")
source("studyListStudyStartDate.R")
source("animalListControl.R")
source("filterAnimalsSex.R")
source("subjDataExtract.R")
source("filterFindingsPhase.R")
source("addFindingsAnimalAge.R")
source("filterFindingsAnimalAge.R")
source("dbManager.R")

library(dplyr)

# control terminology list need for 
# a variety of different query blocks
cont_terms <- readxl::read_excel('data/SEND Terminology 2019-09-27.xls', 
                                 sheet = "SEND Terminology 2019-09-27")

# query parameters for our question
###################################################################################
# Input parameter values
pStudyDesign      <-  "PARALLEL"         # CT: DESIGN
#pSpecies          <-  "RAT"              # CT: SPECIES (extensible)
#pStrain           <-  "SPRAGUE-DAWLEY"   # CT: STRAIN  (extensible)
#pStrain           <-  "WISTAR"
pSpecies          <-  "DOG"
#pStrain          <-  "WISTAR"
pStrain           <- c("BEAGLE")
#pRoute           <-  c("SUBCUTANEOUS")     # CT: ROUTE   (extensible)
#pRoute           <- c("INTRAPERITONEAL")
pRoute            <- c("ORAL", "ORAL GAVAGE")
pFromDTC          <-  "2017"
pToDTC            <-  "2020"
pSex              <-  "M"                # CT: SEX
pStudyPhase       <-  "Treatment"        # Valid: "Screening", "Treatment", "Recovery"
pStudyPhaseInclUncertain <- FALSE    # Valid: TRUE, FALSE
pFindingsFromAge  <-  "12m"
pFindingsToAge    <-  "18m"
###################################################################################



#########################################
#
#  QUERY BLOCK -- STUDY START DATE
#
#########################################


recordsInTS <- GenericQuery('SELECT DISTINCT STUDYID FROM TS')

nRecordsInTS <- nrow(recordsInTS)

TSdate <- GenericQuery('SELECT STUDYID, TSVAL FROM TS WHERE TSPARMCD = "STSTDTC"')

nStudiesWithDate <- nrow(TSdate)

# confidently matching is
# just the number of studies
# in the specified date interval
confMatchingStart <- GetStudyListSTSTDTC(fromDTC=startdate, toDTC=enddate)

# any studies not found by 
# our algorithm are nonmatching
nonMatchingStart <- TSdate[(!(TSdate$STUDYID %in% confMatchingStart$STUDYID)),]
confNonMatchingStart <- nonMatchingStart[(!is.na(parse_iso_8601(nonMatchingStart$TSVAL))),]

# studies not confidently matching, or not confidently non matchings
ambiguousStart <- recordsInTS[(!recordsInTS$STUDYID %in% confNonMatchingStart$STUDYID) & (!recordsInTS$STUDYID %in% confMatchingStart$STUDYID),] 

print(sprintf("Num conf matches: %s", nrow(confMatchingStart)))
print(sprintf("Num conf non-matches: %s", nrow(confNonMatchingStart)))
print(sprintf("Denominator: %s", nRecordsInTS))

print(sprintf("Confidence: %.2f", ((nrow(confMatchingStart) + nrow(confNonMatchingStart)) / nRecordsInTS) * 100))


#########################################
#
# QUERY BLOCK -- Study Design 
#
#########################################

# confidence is defined as records matching
# the controlled terms.  confidence not
# matching, if more than one design, they
# all have to by matching controlled term


recordsInTS <- GenericQuery('SELECT DISTINCT STUDYID FROM TS')
nRecordsInTS <- nrow(recordsInTS)

TSdesign <- GenericQuery('SELECT STUDYID, TSPARMCD, TSVAL FROM TS WHERE TSPARMCD = "SDESIGN"')
nStudiesWithDate <- nrow(TSdesign)


# only use study design controled terms
studyDesignCodelist <- 'C89967'
sdesignControlledTerms <- cont_terms[cont_terms$`Codelist Code` == studyDesignCodelist,]$`CDISC Submission Value`


confMatchingDesign <- GetStudyListSDESIGN(design)
nonMatchingDesign <- TSdesign[(!(TSdesign$STUDYID %in% confMatchDesign$STUDYID)),]
nonMatchingDesign$is_controlled <- toupper(gsub("[\r\n]", "", nonMatchingDesign$TSVAL)) %in% sdesignControlledTerms


# make sure multiple studies all match controled term
nonMatchingDesignUnique <- nonMatchingDesign %>%
                                dplyr::group_by(STUDYID) %>%
                                dplyr::mutate(all_true = dplyr::if_else(sum(is_controlled) == dplyr::n(), TRUE, FALSE)) %>%
                                dplyr::ungroup() %>%
                                dplyr::distinct(STUDYID, all_true)

confNonMatchingDesign <- nonMatchingDesignUnique[nonMatchingDesignUnique$all_true,]


ambiguousDesign <- recordsInTS[(!recordsInTS$STUDYID %in% confNonMatchingDesign$STUDYID) & (!recordsInTS$STUDYID %in% confMatchingDesign$STUDYID),] 

print(sprintf("Num conf matches: %s", nrow(confMatchingDesign)))
print(sprintf("Num conf non-matches: %s", nrow(confNonMatchingDesign)))
print(sprintf("Denominator: %s", nRecordsInTS))

print(sprintf("Confidence: %.2f", ((nrow(confMatchingDesign) + nrow(confNonMatchingDesign)) / nRecordsInTS) * 100))


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


dmWithSetcd <- GenericQuery("SELECT STUDYID, USUBJID, SETCD FROM DM")
txCntrl <- GenericQuery("SELECT STUDYID, SETCD, TXVAL FROM TX WHERE TXPARMCD == 'TCNTRL'")


dmCntrls <- merge(dmWithSetcd, txCntrl, by=c('STUDYID', 'SETCD'))

nCntrlAnimals <- nrow(dmCntrls)

# find animals not idenified as 
# negative control but have
# POSITIVE CONTROL specified as 
# their tcntrl value
confMatchingControl <- GetControlAnimals()
nonMatchingControl <- dmCntrls[!(dmCntrls$USUBJID %in% confMatchingControl$USUBJID),]
nonMatchingControl$TXVAL <- toupper(nonMatchingControl$TXVAL)
confNonMatchingControl <- nonMatchingControl[nonMatchingControl$TXVAL ==  'POSITIVE CONTROL',]


ambiguousControls <- dmCntrls[(!dmCntrls$USUBJID %in% confNonMatchingControl$USUBJID) & (!dmCntrls$USUBJID %in% confMatchingControl$USUBJID),] 



print(sprintf("Num conf matches: %s", nrow(confMatchingControl)))
print(sprintf("Num conf non-matches: %s", nrow(confNonMatchingControl)))
print(sprintf("Denominator: %s", nCntrlAnimals))

print(sprintf("Confidence: %.2f", ((nrow(confMatchingControl) + nrow(confNonMatchingControl)) / nCntrlAnimals) * 100))

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


exRoute <- GenericQuery("SELECT STUDYID, USUBJID, EXROUTE FROM EX")
dmAnimals <- GenericQuery("SELECT STUDYID, USUBJID FROM DM")
tsRoute <- data.table(GenericQuery('SELECT STUDYID, TSPARMCD, TSVAL FROM TS WHERE TSPARMCD = "ROUTE"'))

mergedDmEx <- merge(dmAnimals, unique(exRoute), all=TRUE)


nRecords <- nrow(unique(mergedDmEx[,c('STUDYID', 'USUBJID')]))


tsRoute <- tsRoute[,.(STUDYID, EXROUTE=toupper(TSVAL))]
mergedDmEx$EXROUTE <- toupper(mergedDmEx$EXROUTE)

tsRoute <- tsRoute %>%
              dplyr::group_by(STUDYID) %>%
              dplyr::mutate(NUM_ROUTE = n()) %>%
              dplyr::ungroup()

tsRoute$EXROUTE[tsRoute$NUM_ROUTE > 1] <- NA

tsAnimals <- merge(tsRoute[,c("STUDYID", "EXROUTE")], mergedDmEx[,c("STUDYID", "USUBJID")])

mergedDmExTs <- merge(mergedDmEx, tsAnimals, by=c('STUDYID', 'USUBJID'))

finalRoute <- mergedDmExTs %>%
                mutate(ROUTE = toupper(coalesce(EXROUTE.x, EXROUTE.y))) %>%
                select(-EXROUTE.x, -EXROUTE.y) %>%
                unique()


roaCodelist <- 'C66729'
roaControlledTerms <- cont_terms[cont_terms$`Codelist Code` == roaCodelist,]$`CDISC Submission Value`


confMatchingRoute <- finalRoute[finalRoute$ROUTE %in% pRoute,]
notMatchingRoute <- finalRoute[!finalRoute$USUBJID %in% confMatchingRoute$USUBJID,]


notMatchingRoute$is_controlled <- notMatchingRoute$ROUTE %in% roaControlledTerms

# make sure animals with multiple routes all match controled term
nonMatchingRouteUnique <- notMatchingRoute %>%
                            dplyr::group_by(STUDYID, USUBJID) %>%
                            dplyr::mutate(all_true = dplyr::if_else(sum(is_controlled) == dplyr::n(), TRUE, FALSE)) %>%
                            dplyr::ungroup() %>%
                            dplyr::distinct(STUDYID, USUBJID, all_true)

confNonMatchingRoute <- nonMatchingRouteUnique[nonMatchingRouteUnique$all_true,]

animals <- unique(mergedDmEx[,c('STUDYID', 'USUBJID')])

ambigiousRecords <- animals[(!animals$USUBJID %in% confNonMatchingRoute$USUBJID) & (!animals$USUBJID %in% confMatchingRoute$USUBJID),]


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
dm <- GenericQuery("SELECT STUDYID, USUBJID, ARMCD, SETCD, SPECIES, STRAIN, SEX FROM DM")
animals <- GenericQuery('SELECT STUDYID, TSPARMCD, TSVAL FROM TS WHERE TSPARMCD = "SPECIES" or TSPARMCD = "STRAIN"')

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

speciesCodelist <- 'C77808'
speciesControlledTerms <- cont_terms[cont_terms$`Codelist Code` == speciesCodelist,]$`CDISC Submission Value`

strainCodelist <- 'C77530'
strainControlledTerms <- cont_terms[cont_terms$`Codelist Code` == strainCodelist,]$`CDISC Submission Value`

confMatchingSpecies <- imputed_dm[imputed_dm$SPECIES == pSpecies,]
nonMatchingSpecies <- imputed_dm[imputed_dm$SPECIES != pSpecies,]
confNonMatchingSpecies <- nonMatchingSpecies[nonMatchingSpecies$SPECIES %in% speciesControlledTerms,]

ambigiousSpecies <- imputed_dm[(!imputed_dm$USUBJID %in% confNonMatchingSpecies$USUBJID) & (!imputed_dm$USUBJID %in% confMatchingSpecies$USUBJID),]

print(sprintf("Num conf matches: %s", nrow(confMatchingSpecies)))
print(sprintf("Num conf non-matches: %s", nrow(confNonMatchingSpecies)))
print(sprintf("Denominator: %s", nrow(imputed_dm)))

print(sprintf("Confidence: %.2f", ((nrow(confMatchingSpecies) + nrow(confNonMatchingSpecies)) / nrow(imputed_dm)) * 100))



confMatchingStrain <- imputed_dm[imputed_dm$STRAIN == pStrain,]
nonMatchingStrain <- imputed_dm[imputed_dm$STRAIN != pStrain,]
confNonMatchingStrain <- nonMatchingStrain[nonMatchingStrain$STRAIN %in% strainControlledTerms,]

ambigiousStrain <- imputed_dm[(!imputed_dm$USUBJID %in% confNonMatchingStrain$USUBJID) & (!imputed_dm$USUBJID %in% confMatchingStrain$USUBJID),]

print(sprintf("Num conf matches: %s", nrow(confMatchingStrain)))
print(sprintf("Num conf non-matches: %s", nrow(confNonMatchingStrain)))
print(sprintf("Denominator: %s", nrow(imputed_dm)))

print(sprintf("Confidence: %.2f", ((nrow(confMatchingStrain) + nrow(confNonMatchingStrain)) / nrow(imputed_dm)) * 100))

#########################################
#
# QUERY BLOCK -- 11 Sex
#
#########################################



dm <- GenericQuery("SELECT STUDYID, USUBJID, SEX FROM DM")


# gives us frequency counts of 
# sex in DM
table(dm$SEX, useNA='always')


#########################################
#
# QUERY BLOCK -- findings records phase
#
#########################################



mi <- data.table(GenericQuery('SELECT * FROM MI'))

treatment <- FilterFindingsPhase('MI', mi, 'Treatment', TRUE)
dosing <- FilterFindingsPhase('MI', mi, 'Dosing', FALSE)
screening <- FilterFindingsPhase('MI', mi, 'Screening', FALSE)


ambiguousAge <- treatment[treatment$PHASE == 'Uncertain',]
confMatchingPhase <- treatment[treatment$PHASE != 'Uncertain',]
confNotMatchingPhase <- rbind(dosing, screening)


print(sprintf("Num conf matches: %s", nrow(confMatchingPhase)))
print(sprintf("Num conf non-matches: %s", nrow(confNotMatchingPhase)))
print(sprintf("Denominator: %s", nrow(mi)))

print(sprintf("Confidence: %.2f", ((nrow(confMatchingPhase) + nrow(confNotMatchingPhase)) / nrow(mi)) * 100))




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


mi <- GenericQuery('SELECT * FROM MI')

mi_with_age <- addFindingsAnimalAge('mi', data.table(mi))

# our query asks for findings between 
# 12 months and 18 months at time of
# finding
lower_age <- 12*30
upper_age <- 18*30

confMatchingAge <- mi_with_age[((mi_with_age$AGE >= lower_age) & (mi_with_age$AGE <= upper_age)),]
confNotMatchingAge <- mi_with_age[((mi_with_age$AGE < lower_age) | (mi_with_age$AGE > upper_age)),]
ambiguousAge <- mi_with_age[is.na(mi_with_age$AGE),]

print(sprintf("Num conf matches: %s", nrow(confMatchingAge)))
print(sprintf("Num conf non-matches: %s", nrow(confNotMatchingAge)))
print(sprintf("Denominator: %s", nrow(mi)))

print(sprintf("Confidence: %.2f", ((nrow(confMatchingAge) + nrow(confNotMatchingAge)) / nrow(mi)) * 100))



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

# Confident matches are

mifindings <- GenericQuery("SELECT * FROM MI")
tsfindings <- GenericQuery("SELECT STUDYID, TSPARMCD, TSVAL FROM TS WHERE TSPARMCD = 'SNDIGVER' ")


studies_31 <-  tsfindings$STUDYID[grepl("3.1", tsfindings$TSVAL)]
findings_31 <- mifindings[mifindings$STUDYID %in% studies_31,]

findings_31$MISTRESC <- toupper(findings_31$MISTRESC)
findings_31$MISPEC <- toupper(findings_31$MISPEC)


miCodelists <- c('C120531', 'C88025', 'C132321')
findingsControlledTerms <- cont_terms[cont_terms$`Codelist Code` %in% miCodelists,]$`CDISC Submission Value`

confFindings <- findings_31[findings_31$MISTRESC %in% findingsControlledTerms,]

print(sprintf("Num conf : %s", nrow(confFindings)))
print(sprintf("Denominator: %s", nrow(findings_31)))

print(sprintf("Confidence: %.2f", ((nrow(confFindings) /  nrow(findings_31)) * 100)))

