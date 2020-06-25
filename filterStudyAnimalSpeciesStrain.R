###################################################################################
# Script name   : filterStudyAnimalSpeciesStrain.R
# Date Created  : 16-Jan-2020
# Documentation : Specification - R script - studyListSpeciesStrain.docx
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Extract studies and animals which fulfills a specified species
#                 value and optional strain value from a pooled SEND data store.
#
# Description   : Two function are defined 
#                   - GetStudyListSPECIES_STRAIN
#                   - FilterAnimalListSpeciesStrain
#                 Each of these function are described below
#
# Input         : See description for each function below
#
# Output        : See description for each function below
#
# Parameters    : See description for each function below
# 
# MISSING: 
#   - Full validation of input parameters (strainFilter only allowed if single speciesFulter value)
###################################################################################

###################################################################################
# Function:     : GetStudyListSPECIES_STRAIN
#
# Description   : Returns a data table with a list of studyid, species and strain values
#                 extracted from TS (where TSPARMCD in ('SPECIES', 'STRAIN') where
#                 the TSVAL for species is equal to a given species or within a list of 
#                 species given as input and the TSVAL for strain is equal to a given 
#                 strain or within a list of strains given as input value.
#                 The comparisons of species and strain values are done case insensitive.
#                 The TSVAL values for species and strains are joined together per 
#                 STUDYID and TSGRPID and renamed to SPECIES and STRAIN respectively.
#                 There may be specified multiple value of species and/or strain per study
#                 in TS. To indicate whether this is the case for the studies included in 
#                 the output table, two extra variables are added:
#                   - the number of distinct species values per STUDYID in TS (if greater 
#                     than 1, multiple species are specified for that study in TS even though 
#                     only  one species has been extracted for that study) - relevant if only 
#                     a species has been given as input
#                   - the number of distinct combinations of species and strain values per 
#                     STUDYID in TS (if greater than 1, multiple species and strain combinations
#                     are specified for that study in TS even though only one species/strain 
#                     combination has been extracted for that study) - relevant if both
#                     a species and a strain has been given as input
#
# Input         : The Ts domain - is imported from the pooled SEND data store if 
#                 it doesn't exist in workspace.
#
# Output        : The function GetStudyListSPECIES_STRAIN returns a data table with these columns:
#                     STUDYID             (character)
#                     SPECIES             (character) (<-TS.TSVAL where TSPARMCD='SPECIES')
#                     STRAIN              (character) (<-TS.TSVAL where TSPARMCD='STRAIN')
#                     NUM_SPECIES         (character) 
#                       - the number of distinct species values per STUDYID.
#                     NUM_SPECIES_STRAIN  (character) 
#                       - the number of distinct species/strain values per STUDYID
#
# Parameters    : speciesFilter:  mandatory, the  species(s) to use as criterion for 
#                                 filtering of the input data table.
#                                 It can be a single string, a vector or a list of 
#                                 multiple strings.
#                 strainFilter:   optional, the  strains(s) to use as criterion for 
#                                 filtering of the input data table.
#                                 Only allowed when a single value is specified for 
#                                 speciesFilter.
#                                 It can be a single string, a vector or a list of 
#                                 multiple strings.
#                 studyList:      optional, a data table with a list of studies to 
#                                 limit the output to be within this set of studies
#
###################################################################################


###################################################################################
# Function:     : FilterAnimalListSpeciesStrain
#
# Description   : Returns a data table with a set of animals extratced from the table of  
#                 animals given as input where the animals fits the species value(s) given 
#                 as input (a list of multiple species values may be 
#                 given as input) and also fits the strain value(s) if given as input (optional).
#                 Animals from the input set, which are included in studies where one of  
#                 of the input species values - and optionally also strain values - is 
#                 registered in TS (and TS have only this species or species/strain 
#                 value included) are included in the output set.
#                 Animals from the input set, belonging to studies 
#                   - with no species/strain value registered  or  
#                   - with more than one species/strain value registered
#                 in TS are included in the output set if they exists in DM with matching 
#                 DM.SPECIES or DM.SPECIES/STRAIN values or are included in TX in a trial set 
#                 with matching TX.SPECIES or TX.SPECIES/STRAIN (i.e. TXVAL where TXPARMCD is 
#                 SPECIES' or 'STRAIN')
#                 The comparisons of the species and strain values are done case 
#                 insensitive.
#                 
#                 If the input parameter inclUncertain flag is enabled, uncertain animals
#                 are included in the output set.
#                 These uncertain situations are identified and reported (in column UNCERTAIN_MSG):
#                  - TS parameter is missing or invalid (not CT value - CDISC code list ROUTE) 
#                    and EXROUTE is missing or invalid (not CT value - CDISC code list ROUTE)
#                  - Multiple TS parameter ROUTE values are registered for study but EXROUTE is missing 
#                    or invalid (not CT value)
#                  Non-empty UNCERTAIN_MSG values are merged with non-empty UNCERTAIN_MSG values 
#                  which may exist in the input set of animals (animalList).
#
# Input         : The TX and DM domains - are imported from the pooled SEND data store if 
#                 the don't exist in workspace.
#                 Data tables specified in the input parameters:
#                   - animalList
#                     The list a animals to be filtered.
#                     Must contain these character variables:
#                       STUDYID
#                       USUBJID
#                     other variables may be included 
#                 CDISC CT code lists SPECIES and STRAIN
#                   
# Output        : A data table with the character columns:
#                   STUDYID
#                   USUBJID
#                   SPECIES
#                   STRAIN (if strainFilter has been specified)
#                   UNCERTAIN_MSG - if input parameter inclUncertain flag is enabled
#                 plus any additional columns which may be included in the input data animalList
#
# Parameters    : animalList:     mandatory, data table (see Input).
#                 speciesFilter:  mandatory, the  species(s) to use as criterion for 
#                                 filtering of the input data table.
#                                 It can be a single string, a vector or a list of 
#                                 multiple strings.
#                 strainFilter:   optional, the  strains(s) to use as criterion for 
#                                 filtering of the input data table.
#                                 Only allowed when a single value is specified for 
#                                 speciesFilter.
#                                 It can be a single string, a vector or a list of 
#                                 multiple strings.
#                 inclUncertain:  optional, Include uncertain rows or not
#                   
###################################################################################

library(data.table)

FilterAnimalsSpeciesStrain<-function(animalList=NULL, speciesFilter=NULL, strainFilter=NULL, inclUncertain=FALSE) {
  
  if (is.null(animalList) | isTRUE(is.na(animalList)) | isTRUE(animalList=="")) {
    stop("animalList must be be specified with a data table")
  } 
  
  if (is.null(speciesFilter) | isTRUE(is.na(speciesFilter)) | isTRUE(speciesFilter=="")) {
    stop("speciesFilter must be specified")
  } 
  
  InclStrainFilter<-FALSE
  if (!(is.null(strainFilter) | isTRUE(is.na(strainFilter)) | isTRUE(strainFilter==""))) {
    # check if strainFilter is allowed...
    if (length(speciesFilter) > 1) {
      stop("Not possible to handle strainFilter when speciesFilter contains multiple values")
    }
    InclStrainFilter<-TRUE
  }
  
  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter inclUncertain must be either TRUE or FALSE")
  }
  
  # List of studyid values included in the input table of animals
  animalStudies<-unique(animalList[,.(STUDYID)])
  
  # import TS, TX and DM if they not already exists
  if (!exists("TS")) {
    importSENDDomains(c("TS"))
  }
  if (!exists("TX")) {
    importSENDDomains(c("TX"), animalStudies)
  }
  if (!exists("DM")) {
    importSENDDomains(c("DM"), animalStudies)
  }
  
  # 1. First we look into the SPECIES level (whether or not strainFilter has been specified)
   
  # 1.1 Study level extraction - SPECIES
 
  # Get values of code list SPECIES from CDISC CT
  ctSPECIES<-getCTCodListValues("SPECIES")
    
  # Extract all TS rows for parameter SPECIES, rename TSVAL to SPECIES 
  # - remove duplicates
  # - limit to the set of studies for input set of animals
  tsSPECIESall<-merge(unique(TS[TSPARMCD == 'SPECIES', .(STUDYID,SPECIES=toupper(trimws(TSVAL)))]), animalStudies, by='STUDYID')
  # Add variable with count of number of distinct SPECIESs per study
  # - extract set or rows matching speciesFilter
  tsSPECIESall[, `:=` (NUM_SPECIES = .N), by = STUDYID]
  tsSPECIES<-tsSPECIESall[SPECIES %in% toupper(trimws(speciesFilter))]

  # Get list of uncertain SPECIES data at TS level - combined by 
  #  - list of studies with no TS parameter SPECIES
  #  - list of studies with non-CT value of TS parameter SPECIES
  #  - include a study level uncertain message
  tsSPECIESUncertain<-
      rbindlist(list(fsetdiff(animalStudies, 
                              tsSPECIESall[,.(STUDYID)])[!is.na(STUDYID),.(STUDYID, UNCERTAIN_MSG='TS parameter SPECIES is missing')],
                     tsSPECIESall[!is.na(STUDYID) & !(SPECIES %in% ctSPECIES),
                              .(STUDYID, UNCERTAIN_MSG='TS parameter SPECIES does not contain a valid CT value')]))
  
  # Extract list of all animals for studies with only one SPECIES specified in TS 
  # - for these studies we can extract all relevant control animals matching the speciesFilter
  tsLvlAnimalsSPECIES<-merge(tsSPECIES[NUM_SPECIES==1, .(STUDYID, SPECIES)], 
                             animalList[,.(STUDYID, USUBJID)], by='STUDYID')[,.(STUDYID, USUBJID, SPECIES)]
  
  # 1.2 Trial set level extraction - SPECIES
  
  # Extract all TX rows for parameter SPECIES, rename TXVAL to SPECIES 
  # - remove duplicates
  # - limit  to studies with no species value registered in TS or with more than one species value registered in TS
  txSPECIESall<-merge(unique(TX[TXPARMCD == 'SPECIES', .(STUDYID, SETCD, SPECIES=toupper(trimws(TXVAL)))]), 
                      rbindlist(list(tsSPECIES[NUM_SPECIES > 1, .(STUDYID)], tsSPECIESUncertain[,.(STUDYID)])), 
                      by='STUDYID')
  
  # Extract trial sets of matching speciesFilter
  txSPECIES<-txSPECIESall[SPECIES %in% toupper(trimws(speciesFilter))]
  
  # Get list of uncertain SPECIES data at TX level from list of studies with no species value registered in TS 
  # or with more than one species value registered in TS - combined by 
  #  - list of trials sets with no TX parameter SPECIES
  #  - list of trials sets with non-CT value of TX parameter SPECIES
  #  - include a trial set level uncertain message
  txSPECIESUncertain<-
    rbindlist(list(fsetdiff(merge(TX[TXPARMCD=='TCNTRL',.(STUDYID,SETCD)],
                                  rbindlist(list(tsSPECIES[NUM_SPECIES > 1, .(STUDYID)], tsSPECIESUncertain[,.(STUDYID)])),
                                  by='STUDYID', all.y = TRUE), 
                            txSPECIESall[,.(STUDYID, SETCD)])[!is.na(STUDYID),.(STUDYID, SETCD, UNCERTAIN_MSG='TX parameter SPECIES is missing')],
                   txSPECIESall[!is.na(STUDYID) & !(SPECIES %in% ctSPECIES), .(STUDYID, SETCD, UNCERTAIN_MSG='TX parameter SPECIES does not contain a valid CT value')]))
  
  # Extract list of all animals for studies with only one SPECIES specified in TS 
  # - for these studies we can extract all relevant control animals matching the speciesFilter
  txLvlAnimalsSPECIES<-merge(txSPECIES[, .(STUDYID, SETCD, SPECIES)], 
                              animalList[,.(STUDYID, USUBJID)], by='STUDYID')[,.(STUDYID, USUBJID, SPECIES)]
  
  # 1.3 Animal level extraction - SPECIES
  
  # Extract list of animals belonging to trial sets with 
  #   - no valid species value registered in TS or with more than one species value registered in TS 
  #   - and with no valid species value registered in TX.
  # - Include animals where 
  #   - DM.SPECIES matches the speciesFilter
  #   - or DM.SPECIES does not contain a valid CDISC CT value - and an uncertain message is defined for these animals 
  dmLvlAnimalsSPECIES<-
    merge(merge(DM[,.(STUDYID, SETCD, USUBJID, SPECIES=toupper(trimws(SPECIES)))],
                      txSPECIESUncertain[,.(STUDYID, SETCD)],
                      by=c('STUDYID', 'SETCD')),
                animalList[,.(STUDYID,USUBJID)], 
                by=c('STUDYID', 'USUBJID'))[SPECIES %in% speciesFilter | !(SPECIES %in% ctSPECIES), 
                                            .(STUDYID, SETCD, USUBJID, SPECIES, UNCERTAIN_MSG=fifelse(!(SPECIES %in% ctSPECIES),
                                                                                                     'DM.SPECIES does not contain a valid CT value',
                                                                                                     as.character(NA))) ]
  if (inclUncertain) {
    # Include uncertain rows
    # Merge the animal level set of animals with the lists of uncertain studies and trial sets
    #  - If a SPECIES has been identified at animal level, not uncertain message is included
    #  - else an uncertain message is included 
    #       - if uncertain messages exists at multiple levels, these are merged separated by ' & '
    dmLvlAnimalsSPECIES<-
      merge(merge(dmLvlAnimalsSPECIES, 
                  txSPECIESUncertain, 
                  by=c('STUDYID', 'SETCD'), 
                  all.x=TRUE) [,.(STUDYID, USUBJID, SPECIES, 
                                  UNCERTAIN_MSG=fifelse(!is.na(UNCERTAIN_MSG.x),
                                                        fifelse(!is.na(UNCERTAIN_MSG.y), 
                                                                paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep=' & '),
                                                                UNCERTAIN_MSG.x),
                                                       as.character(NA)))],
            tsSPECIESUncertain, 
            by=c('STUDYID'),
            all.x=TRUE)[,.(STUDYID, USUBJID, SPECIES, 
                           UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x),
                                                 fifelse(!is.na(UNCERTAIN_MSG.y), 
                                                         paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep=' & '),
                                                         UNCERTAIN_MSG.x),
                                                as.character(NA)))]
  }
  else
    # Exclude uncertain rows
    dmLvlAnimalsSPECIES<-dmLvlAnimalsSPECIES[is.na(UNCERTAIN_MSG),.(STUDYID, USUBJID, SPECIES)]
  
  # 1.4 Combine study, trial set and animal level sets of animals into one set - SPECIES
  
  # Union of all three sets of animals
  foundAnimalsSPECIES<-rbindlist(list(tsLvlAnimalsSPECIES, txLvlAnimalsSPECIES, dmLvlAnimalsSPECIES), use.names=TRUE, fill=TRUE)
 
  
  ##########################################################################################################################
  
  if (!InclStrainFilter)
    # No strainFilter has been specified
    
    # 1.5 The complete set of animals to return
    foundAnimals<-foundAnimalsSPECIES
  else {
    # 2. strainFilter has been specified - limit set of animals extracted at SPECIES level to specified STRAIN(s)
  
    # 2.1 Study level extraction - STRAIN
    
    # Get values of code list STRAIN from CDISC CT
    ctSPECIES<-getCTCodListValues("STRAIN")
    
    # List of studyid values included in the list of animals extracted at SPECIES level
    speciesStudies<-unique(foundAnimalsSPECIES[,.(STUDYID)])
    
    # Extract all TS rows for parameter STRAIN, rename TSVAL to STRAIN 
    # - remove duplicates
    # - limit to the set of studies for input set of animals
    tsSTRAINall<-merge(unique(TS[TSPARMCD == 'STRAIN', .(STUDYID,STRAIN=toupper(trimws(TSVAL)))]), speciesStudies, by='STUDYID')
    # Add variable with count of number of distinct STRAINs per study
    # - extract set or rows matching strainFilter
    tsSTRAINall[, `:=` (NUM_STRAIN = .N), by = STUDYID]
    tsSTRAIN<-tsSTRAINall[STRAIN %in% toupper(trimws(strainFilter))]
    
    # Get list of uncertain STRAIN data at TS level - combined by 
    #  - list of studies with no TS parameter STRAIN
    #  - list of studies with non-CT value of TS parameter STRAIN
    #  - include a study level uncertain message
    tsSTRAINUncertain<-
      rbindlist(list(fsetdiff(speciesStudies, 
                              tsSTRAINall[,.(STUDYID)])[!is.na(STUDYID),.(STUDYID, UNCERTAIN_MSG='TS parameter STRAIN is missing')],
                     tsSTRAINall[!is.na(STUDYID) & !(STRAIN %in% ctSTRAIN),
                                  .(STUDYID, UNCERTAIN_MSG='TS parameter STRAIN does not contain a valid CT value')]))
    
    # Extract list of all animals for studies with only one STRAIN specified in TS 
    # - for these studies we can extract all relevant control animals matching the strainFilter
    tsLvlAnimalsSTRAIN<-merge(tsSTRAIN[NUM_STRAIN==1, .(STUDYID, STRAIN)], 
                              foundAnimalsSPECIES[,.(STUDYID, USUBJID)], by='STUDYID')[,.(STUDYID, USUBJID, STRAIN)]
    
    
    # 2.2 Trial set level extraction - STRAIN
    
    # Extract all TX rows for parameter STRAIN, rename TXVAL to STRAIN 
    # - remove duplicates
    # - limit  to studies with no strain value registered in TS or with more than one strain value registered in TS
    txSTRAINall<-merge(unique(TX[TXPARMCD == 'STRAIN', .(STUDYID, SETCD, STRAIN=toupper(trimws(TXVAL)))]), 
                        rbindlist(list(tsSTRAIN[NUM_STRAIN > 1, .(STUDYID)], tsSTRAINUncertain[,.(STUDYID)])), 
                        by='STUDYID')
    
    # Extract trial sets of matching strainFilter
    txSTRAIN<-txSTRAINall[STRAIN %in% toupper(trimws(strainFilter))]
    
    # Get list of uncertain STRAIN data at TX level from list of studies with no strain value registered in TS 
    # or with more than one strain value registered in TS - combined by 
    #  - list of trials sets with no TX parameter STRAIN
    #  - list of trials sets with non-CT value of TX parameter STRAIN
    #  - include a trial set level uncertain message
    txSTRAINUncertain<-
      rbindlist(list(fsetdiff(merge(TX[TXPARMCD=='TCNTRL',.(STUDYID,SETCD)],
                                    rbindlist(list(tsSTRAIN[NUM_STRAIN > 1, .(STUDYID)], tsSTRAINUncertain[,.(STUDYID)])),
                                    by='STUDYID', all.y = TRUE), 
                              txSTRAINall[,.(STUDYID, SETCD)])[!is.na(STUDYID),.(STUDYID, SETCD, UNCERTAIN_MSG='TX parameter STRAIN is missing')],
                     txSTRAINall[!is.na(STUDYID) & !(STRAIN %in% ctSTRAIN), .(STUDYID, SETCD, UNCERTAIN_MSG='TX parameter STRAIN does not contain a valid CT value')]))
    
    # Extract list of all animals for studies with only one STRAIN specified in TS 
    # - for these studies we can extract all relevant control animals matching the strainFilter
    txLvlAnimalsSTRAIN<-merge(txSTRAIN[, .(STUDYID, SETCD, STRAIN)], 
                              foundAnimalsSPECIES[,.(STUDYID, USUBJID)], by='STUDYID')[,.(STUDYID, USUBJID, STRAIN)]
    
    
    # 2.3 Animal level extraction - STRAIN
    
    # Extract list of animals belonging to trial sets with 
    #   - no valid strain value registered in TS or with more than one strain value registered in TS 
    #   - and with no valid strain value registered in TX.
    # - Include animals where 
    #   - DM.STRAIN matches the strainFilter
    #   - or DM.STRAIN does not contain a valid CDISC CT value - and an uncertain message is defined for these animals 
    dmLvlAnimalsSTRAIN<-
      merge(merge(DM[,.(STUDYID, SETCD, USUBJID, STRAIN=toupper(trimws(STRAIN)))],
                  txSTRAINUncertain[,.(STUDYID, SETCD)],
                  by=c('STUDYID', 'SETCD')),
            foundAnimalsSPECIES[,.(STUDYID,USUBJID)], 
            by=c('STUDYID', 'USUBJID'))[STRAIN %in% strainFilter | !(STRAIN %in% ctSTRAIN), 
                                        .(STUDYID, SETCD, USUBJID, STRAIN, UNCERTAIN_MSG=fifelse(!(STRAIN %in% ctSTRAIN),
                                                                                                 'DM.STRAIN does not contain a valid CT value',
                                                                                                 as.character(NA))) ]
    if (inclUncertain) {
      # Include uncertain rows
      # Merge the animal level set of animals with the lists of uncertainty studies and trial sets
      #  - If a STRAIN has been identified at animal level, not uncertainty message is included
      #  - else an uncertainty message is included 
      #       - if uncertainty messages exists at multiple levels, these are merged separated by ' & '
      dmLvlAnimalsSTRAIN<-
        merge(merge(dmLvlAnimalsSTRAIN, 
                    txSTRAINUncertain, 
                    by=c('STUDYID', 'SETCD'), 
                    all.x=TRUE) [,.(STUDYID, USUBJID, STRAIN, 
                                    UNCERTAIN_MSG=fifelse(!is.na(UNCERTAIN_MSG.x),
                                                          fifelse(!is.na(UNCERTAIN_MSG.y), 
                                                                  paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep=' & '),
                                                                  UNCERTAIN_MSG.x),
                                                          as.character(NA)))],
              tsSTRAINUncertain, 
              by=c('STUDYID'),
              all.x=TRUE)[,.(STUDYID, USUBJID, STRAIN, 
                             UNCERTAIN_MSG=fifelse(!is.na(UNCERTAIN_MSG.x),
                                                   fifelse(!is.na(UNCERTAIN_MSG.y), 
                                                           paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep=' & '),
                                                           UNCERTAIN_MSG.x),
                                                   as.character(NA)))]
    }
    else
      # Exclude uncertain rows
      dmLvlAnimalsSTRAIN<-dmLvlAnimalsSTRAIN[is.na(UNCERTAIN_MSG),.(STUDYID, USUBJID, STRAIN)]
    
    # 2.4 Combine study, trial set and animal level sets of animals into one set - STRAIN
    
    # Union of all three sets of animals
    foundAnimalsSTRAIN<-rbindlist(list(tsLvlAnimalsSPECIES, txLvlAnimalsSPECIES, dmLvlAnimalsSPECIES), use.names=TRUE, fill=TRUE)
    
    # 2.5 Generate the complete set of animals to return
    
    # Merge the final set of animals set with the set of animals listed at SPECIES level to include 
    # the SPECIES and (the potential) UNCERTAIN_MSG varialble
    foundAnimals<-merge(foundAnimalsSPECIES, foundAnimalsSTRAIN, by=c('STUDYID', 'USUBJID'))
    
    if (inclUncertain) {
      # Combine the uncertainty messages at SPECIES and STRAIN level
      # - if uncertainty messages exists at both levels, these are merged separated by ' & '
      foundAnimals[,`:=` (UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y), 
                                                paste(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y, sep=' & '),
                                                fcoalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)))][, `:=` (UNCERTAIN_MSG.x=NULL,UNCERTAIN_MSG.y=NULL)]
    }
    
    
  }
  
  if (inclUncertain)
    # Add function name in front of the generated uncertain messages
    foundAnimals[,`:=` (UNCERTAIN_MSG=fifelse(!is.na(UNCERTAIN_MSG), 
                                              paste('FilterAnimalsSpeciesStrain: ', UNCERTAIN_MSG, sep=''),
                                              as.character(NA)))]
  
  # Merge the list of extracted animals with the input set of animals to keep
  # any additional columns from the input table 
  foundAnimals<-merge(foundAnimals, animalList, by=c('STUDYID', 'USUBJID'))
  if ("UNCERTAIN_MSG.y" %in% names(foundAnimals)) {
    # An UNCERTAIN_MSG column is included in both input and found list of animals
    #  - merge the UNCERTAIN_MSG from each of the merged tables into one column
    #  - non-empty messages are separated by '|'
    #  - exclude the original UNCERTAIN_MSG columns after the merge  
    foundAnimals<-foundAnimals[,`:=` (UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y), 
                                                            paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep='|'),
                                                            fcoalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)))][, `:=` (UNCERTAIN_MSG.x=NULL,UNCERTAIN_MSG.y=NULL)]
  }
  # Return list of found animals  
  return(foundAnimals)
  
}
