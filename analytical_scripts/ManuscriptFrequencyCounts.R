# Note: working directory needs
# to be set one level above 
# the current directory i.e, 
# the parent directory 


# This script will generate all the
# for the SEND Manuscript 2.

source("importSENDDomains.R")
source("extractTSParam.R")
source("addFindingsAnimalAge.R")

require(dplyr)


##### Table 2 ######

# will import the TS Domain
importSENDDomains('TS')
uniqueStudiesTS <- length(unique(TS$STUDYID))

# 1763 unique studies
tsRoute <- TS[TS$TSPARMCD == 'ROUTE']

nrow(tsRoute)
table(tsRoute$TSVAL, useNA = 'always')

# no NA so 100% of the time? Manuscript now says 1588/1591

##### Table 2 ######

tsSpecies <- TS[TS$TSPARMCD == 'SPECIES']

nrow(tsSpecies)
# 1764 species, 1763 unique studies

nrow(unique(tsSpecies))
# 1763/1763 100% -> 99.81% in manuscript


tsSpecies <- TS[TS$TSPARMCD == 'SPECIES']

nrow(tsSpecies)
# 1764 species, 1763 unique studies

nrow(unique(tsSpecies))
# 1763/1763 100% -> 99.81% in manuscript

##### Table 3 ######

importSENDDomains('TX')

uniqueStudiesTX <- unique(TX$STUDYID)

length(uniqueStudiesTX)

# should this be done, by studyid/setcd level???
txSpecies <- TX[TX$TXPARMCD == 'SPECIES']

length(unique(txSpecies$STUDYID))

# 11/1763 ???? -> many deemed invalid studies due to missing domains, will needed to look at which domains
# are missing

txStrain <- TX[TX$TXPARMCD == 'STRAIN']

length(unique(txStrain$STUDYID))
# 14/1763, probably the same deal

importSENDDomains('DM')

uniqueStudiesDM <- unique(DM$STUDYID)

length(uniqueStudiesDM)

# 1763

dmSpecies <- DM[,c('STUDYID', 'SPECIES')] 

# count number of unique studies where dm is nan
length(unique(dmSpecies$STUDYID[((!is.na(dmSpecies$SPECIES)) & (dmSpecies$SPECIES != ''))]))

# 110/1763  35% vs 44% in manuscript.  --- am i doing this right??


#### Table # ####

txControl <- TX[TX$TXPARMCD == 'TCNTRL']

length(unique(txControl$STUDYID))
# 1611 / 1763 91.37% vs 91.76% in manuscript

# find the most common entries expressed for Control type

freqCntrls <- txControl %>% dplyr::count(TXVAL) %>% dplyr::arrange(dplyr::desc(n))


#### Table # ####

tsSplrnam <- TS[TS$TSPARMCD == 'SPLRNAM']
length(unique(tsSplrnam$STUDYID))

# 1763/1763 manuscript >99% 

tsSplrloc <- TS[TS$TSPARMCD == 'SPLRLOC']
length(unique(tsSplrloc$STUDYID))

# 977/1763 55.04 % vs 53.04 % in manuscript

# animal age at time of Finding

importSENDDomains('MI')
miFindingsAge <- addFindingsAnimalAge('MI', MI)
nFindings <- nrow(miFindingsAge)

findingsWithAge <- sum(!(is.na(miFindingsAge$AGE)))
findingsWithAge / nFindings

# 98.12 vs vs 97.93 in manuscript

##### 3.3.2 MI Findings #####

tsIG <- TS[TS$TSPARMCD == 'SNDIGVER']

studies_30 <-  tsIG$STUDYID[grepl("3.0", tsIG$TSVAL)]
studies_31 <-  tsIG$STUDYID[grepl("3.1", tsIG$TSVAL)]

findings_30 <- MI[MI$STUDYID %in% studies_30]
findings_31 <- MI[MI$STUDYID %in% studies_31]

# frequencies 

sum(findings_30$MISTRESC != "") / length(findings_30$MISTRESC)
# 98.57 vs 98.5

sum(findings_31$MISTRESC != "") / length(findings_31$MISTRESC)
# 98.7 vs 99.4

cont_terms <- readxl::read_excel('data/SEND Terminology 2019-09-27.xls', sheet = "SEND Terminology 2019-09-27")[['CDISC Submission Value']]


sum(findings_30$MISTRESC %in% cont_terms) / length(findings_30$MISTRESC)

sum(findings_31$MISTRESC %in% cont_terms) / length(findings_31$MISTRESC)
# 94.30 vs 95.6

frequencyMI30 <- findings_30 %>%
                  count(MISTRESC) %>%
                  arrange(desc(n))


write.csv(frequencyMI30, 'data/freqMIFindings30.csv')

          