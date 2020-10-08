###################################################################################
# Script name   : filterStudyAnimalSpeciesStrain.R
# Date Created  : 16-Jan-2020
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
# Description   : Function FilterAnimalsSpeciesStrain:
# 
#                 Returns a data table with a set of animals extracted from the table of  
#                 animals given as input where the animals fits the species value(s) given 
#                 as input (a list of multiple species values may be given as input) 
#                 and also fits the strain value(s) if given as input (optional).
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
#                 These uncertain situations are identified and reported for SPECIES and STRAIN respectively 
#                 (in column UNCERTAIN_MSG):
#                  - TS parameter SPECIES/STRAIN is missing or invalid (not CT value - CDISC code list SPECIES/STRAIN) 
#                    and TX parameter SPECIES/STRAIN is missing or invalid (not CT value) and DM.SPECIES/STRAIN is 
#                    missing or invalid (not CT value)
#                  - Different values of SPECIES/STRAIN across TS, TX and DM for studies where no or only one 
#                    TS parameter SPECIES/STRAIN is registered
#                  - Multiple TS parameter SPECIES/STRAIN values are registered for study and TX parameter 
#                    SPECIES/STRAIN and/or DM.SPECIES/STRAIN do not match any of the TS values.
#                  - Multiple TS parameter SPECIES/STRAIN values are registered for study and TX parameter 
#                    SPECIES/STRAIN and DM.SPECIES/STRAIN are unequal.
#                  Non-empty UNCERTAIN_MSG values are merged with non-empty UNCERTAIN_MSG values 
#                  which may exist in the input set of animals (animalList).
#
# Input         : - The TS, TX and DM domains - are imported from the pooled SEND data store if 
#                   the don't exist in workspace.
#                 - A data tables specified in the input parameters animalList:
#                   It contains the list of animals to filter for specified species and strain value(s)
#                   - must contain these character variables:
#                       STUDYID
#                       USUBJID
#                     other variables may be included 
#                 - CDISC CT code lists SPECIES and STRAIN imported from a CDISC CT file.
#                   
# Output        : A data table with the character columns:
#                   STUDYID
#                   USUBJID
#                   SPECIES
#                   STRAIN (if strainFilter has been specified)
#                   UNCERTAIN_MSG - if input parameter inclUncertain flag is enabled
#                 plus any additional columns which may be included in the input data animalList
#
# Parameters    : animalList:     Mandatory, data table (see Input).
#                 speciesFilter:  Mandatory, character.
#                                   The species value(s) to use as criterion for filtering of the input data table.
#                                   It can be a single string, a vector or a list of multiple strings.
#                 strainFilter:   Optional, character.
#                                   The species value(s) to use as criterion for filtering of the input data table.
#                                   It can be a single string, a vector or a list of multiple strings.
#                                   Only allowed when a single value is specified for speciesFilter.
#                 inclUncertain:  Optional, Include uncertain rows or not
#                   
###################################################################################

library(data.table)

FilterAnimalsSpeciesStrain<-function(animalList=NULL, speciesFilter=NULL, strainFilter=NULL, inclUncertain=FALSE, exclusively=FALSE) {
  
  ##################################################################################################################
  
  #### Function to identify uncertain animals at species level
  identifyUncertainSPECIES<-function(SPECIES, SPECIES_TS, SPECIES_TX, SPECIES_DM,  ALL_SPECIES_TS, NUM_SPECIES_TS, NUM_ANIMALS) {
    msgArr<-c()
    if (is.na(SPECIES))
      msgArr<-c(msgArr, 'TS and TX parameters SPECIES and DM.SPECIES are all missing')
    else {
      if (NUM_ANIMALS > NUM_SPECIES_TS)
        msgArr<-c(msgArr, 'TX parameter SPECIES included multiple times for the SET')
      else {
        if (! SPECIES %in% ctSPECIES) {
          if (!is.na(SPECIES_DM) & ! SPECIES_DM %in% ctSPECIES)
            msgArr<-c(msgArr, 'DM.SPECIES does not contain a valid CT value')
          else if (!is.na(SPECIES_TX) & ! SPECIES_TX %in% ctSPECIES)
            msgArr<-c(msgArr, 'TX parameter SPECIES does not contain a valid CT value')
          else if (!is.na(SPECIES_TS) & ! SPECIES_TS %in% ctSPECIES)
            msgArr<-c(msgArr, 'TS parameter SPECIES does not contain a valid CT value')
        }
        
        if (NUM_SPECIES_TS == 1 & length(unique(na.omit(c(SPECIES_TS, SPECIES_TX, SPECIES_DM)))) > 1)
          msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters SPECIES and/or DM.SPECIES')
        else
          if (NUM_SPECIES_TS > 1 & ((!SPECIES %in% ALL_SPECIES_TS) | (!is.na(SPECIES_TX) & !is.na(SPECIES_DM) & SPECIES_TX != SPECIES_DM))) 
            msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters SPECIES and/or DM.SPECIES')
      }
    }
    msg<-paste(msgArr, collapse = ' & ')
    return(ifelse(msg=="", as.character(NA), msg))
  }
    
  #### Function to identify uncertain animals at strain level
  identifyUncertainSTRAIN<-function(STRAIN, STRAIN_TS, STRAIN_TX, STRAIN_DM,  ALL_STRAIN_TS, NUM_STRAIN_TS, NUM_ANIMALS) {
    msgArr<-c()
    if (is.na(STRAIN))
      msgArr<-c(msgArr, 'TS and TX parameters STRAIN and DM.STRAIN are all missing')
    else {
      if (NUM_ANIMALS > NUM_STRAIN_TS)
        msgArr<-c(msgArr, 'TX parameter STRAIN included multiple times for the SET')
      else {
        if (! STRAIN %in% ctSTRAIN) {
          if (!is.na(STRAIN_DM) & ! STRAIN_DM %in% ctSTRAIN)
            msgArr<-c(msgArr, 'DM.STRAIN does not contain a valid CT value')
          else if (!is.na(STRAIN_TX) & ! STRAIN_TX %in% ctSTRAIN)
            msgArr<-c(msgArr, 'TX parameter STRAIN does not contain a valid CT value')
          else if (!is.na(STRAIN_TS) & ! STRAIN_TS %in% ctSTRAIN)
            msgArr<-c(msgArr, 'TS parameter STRAIN does not contain a valid CT value')
        }
        
        if (NUM_STRAIN_TS == 1 & length(unique(na.omit(c(STRAIN_TS, STRAIN_TX, STRAIN_DM)))) > 1)
          msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters STRAIN and/or DM.STRAIN')
        else
          if (NUM_STRAIN_TS > 1 & ((!STRAIN %in% ALL_STRAIN_TS) | (!is.na(STRAIN_TX) & !is.na(STRAIN_DM) & STRAIN_TX != STRAIN_DM))) 
            msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters STRAIN and/or DM.STRAIN')
      }
    } 
    msg<-paste(msgArr, collapse = ' & ')
    return(ifelse(msg=="", as.character(NA), msg))
  }
  
  ##################################################################################################################
  
  if (!is.data.table(animalList)) {
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
  
  if (!(exclusively %in% c(TRUE,FALSE))) {
    stop("Parameter Exclusively must be either TRUE or FALSE")
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
  
  ##################################################################################################################
  
  # 1. First we look into the SPECIES level (whether or not strainFilter has been specified)
   
  # Get values of code list SPECIES from CDISC CT
  ctSPECIES<-getCTCodListValues("SPECIES")

  # Extract all TS rows for parameter SPECIES, rename TSVAL to SPECIES_TS 
  # - remove duplicates
  # - limit to the set of studies for input set of animals
  tsSPECIESall<-merge(unique(TS[TSPARMCD == 'SPECIES', .(STUDYID,SPECIES_TS=toupper(trimws(TSVAL)))]), animalStudies, by='STUDYID')
  # Add studies with no TS parameter SPECIES from the set of studies for input set of animals
  tsSPECIESall<-
    rbindlist(list(tsSPECIESall,
                   fsetdiff(animalStudies, 
                            tsSPECIESall[,.(STUDYID)])[!is.na(STUDYID),.(STUDYID, SPECIES_TS=as.character(NA))]),
              use.names=TRUE, fill=TRUE)
  
  # Add variables with 
  #  - count of number of distinct SPECIES per study
  #  - concatenation of all species per study (for studies with one species this is equal to SPECIES_TS)
  tsSPECIESall[, `:=` (ALL_SPECIES = unique(SPECIES_TS), NUM_SPECIES_TS = .N), by = STUDYID]
  tsSPECIESall[,`:=`(ALL_SPECIES_TS = c(.SD)), by = STUDYID, .SDcols='SPECIES_TS']
  
  # JOin the list of studies/species with DM to get all animal level SPECIES 
  #  - the list will contain STUDYID/USUBJID duplicates for studies with multiple SPECIES registered in TS               
  animalSPECIESall<-               
    merge(merge(tsSPECIESall,
                animalList[,.(STUDYID, USUBJID)],
                by='STUDYID', allow.cartesian = TRUE),
          DM[,.(STUDYID, USUBJID, SETCD, SPECIES_DM=ifelse(SPECIES=="" ,as.character(NA),toupper(trimws(SPECIES))))],
          by=c('STUDYID','USUBJID'), allow.cartesian = TRUE)
  
  # Join the list of studies/animals/species with TX to get all set level SPECIES
  #  - add variable SPECIES with the first non-empty species value from DM, TX or TS
  #  - add variable with count of unique USUBJID per study (there is expected to be one usubjid per studyid per TSPARMCD 'SPECIES' )
  animalSPECIESall<-
    merge(animalSPECIESall, 
          unique(TX[TXPARMCD=='SPECIES',.(STUDYID,SETCD,SPECIES_TX=toupper(trimws(TXVAL)))]), 
          by=c('STUDYID','SETCD'), all.x=TRUE )[,`:=` (SPECIES=fcoalesce(as.character(SPECIES_DM),SPECIES_TX,SPECIES_TS))]
  animalSPECIESall[, `:=` (NUM_ANIMALS = .N), by = .(STUDYID, USUBJID)]
  
  # Identify uncertain animals - add variable UNCERTAIN_MSG
  animalSPECIESall[,`:=` (UNCERTAIN_MSG=mapply(identifyUncertainSPECIES, SPECIES, SPECIES_TS, SPECIES_TX, SPECIES_DM,  ALL_SPECIES_TS, NUM_SPECIES_TS, NUM_ANIMALS ))]
  
  # Extract unique set of animals and related species - exclude uncertain animals
  animalSPECIESallUniq<-unique(animalSPECIESall[is.na(UNCERTAIN_MSG), .(STUDYID, USUBJID, SPECIES)])
  
  # Extract animals match 
  foundAnimalsSPECIES<-unique(animalSPECIESallUniq[SPECIES %in% speciesFilter, .(STUDYID, USUBJID, SPECIES)])
  
  if (exclusively) {
  # Find studies with animals having other SPECIES than the requested
    foundAnimalsSPECIES<-
      merge(foundAnimalsSPECIES,
            # Set of studies to keep:
            fsetdiff(unique(foundAnimalsSPECIES[,.(STUDYID)]),
                     # Set of studies (included in the found set of animals with matching SPECIES values) with possible 
                     # SPECIES values not included in the speciesFilter:  
                     unique(fsetdiff(merge(# Set of possible SPECIES values per study in the input set of animals:
                                           unique(animalSPECIESallUniq[,.(STUDYID, SPECIES)]),
                                           unique(foundAnimalsSPECIES[,.(STUDYID)]), by='STUDYID'),
                            unique(foundAnimalsSPECIES[,.(STUDYID, SPECIES)]))[,.(STUDYID)])),
            by='STUDYID')
  
  }
  
  
  if (!InclStrainFilter) {
    # No strainFilter has been specified
    
    # The complete set of animals to return - add the uncertain animals
    if (inclUncertain)
      # Add the uncertain animals
      foundAnimals<-rbindlist(list(foundAnimalsSPECIES, 
                                   unique(animalSPECIESall[!is.na(UNCERTAIN_MSG), .(STUDYID, USUBJID, SPECIES, UNCERTAIN_MSG)])),
                              use.names=TRUE, fill=TRUE)
    else 
      foundAnimals<-foundAnimalsSPECIES
  } 
  else {
    
  ##########################################################################################################################
  
    # 2. strainFilter has been specified - limit set of animals extracted at SPECIES level to specified STRAIN(s)
  
    # Get values of code list STRAIN from CDISC CT
    ctSTRAIN<-getCTCodListValues("STRAIN")
    
    # List of studyid values included in the list of animals extracted at SPECIES level
    animalSPECIESStudies<-unique(foundAnimalsSPECIES[,.(STUDYID)])
    
    # Extract all TS rows for parameter STRAIN, rename TSVAL to STRAIN_TS 
    # - remove duplicates
    # - limit to the set of studies for input set of animals
    tsSTRAINall<-merge(unique(TS[TSPARMCD == 'STRAIN', .(STUDYID,STRAIN_TS=toupper(trimws(TSVAL)))]), animalSPECIESStudies, by='STUDYID')
    # Add studies with no TS parameter STRAIN from the set of studies for input set of animals
    tsSTRAINall<-
      rbindlist(list(tsSTRAINall,
                     fsetdiff(animalSPECIESStudies, 
                              tsSTRAINall[,.(STUDYID)])[!is.na(STUDYID),.(STUDYID, STRAIN_TS=as.character(NA))]),
                use.names=TRUE, fill=TRUE)
    
    # Add variables with 
    #  - count of number of distinct STRAIN per study
    #  - concatenation of all strain per study (for studies with one strain this is equal to STRAIN_TS)
    tsSTRAINall[, `:=` (ALL_STRAIN = unique(STRAIN_TS), NUM_STRAIN_TS = .N), by = STUDYID]
    tsSTRAINall[,`:=`(ALL_STRAIN_TS = c(.SD)), by = STUDYID, .SDcols='STRAIN_TS']
    
    # JOin the set of studies/strain with DM to get all animal level STRAIN  limited to the found set of animals at species level
    #  - the set will contain STUDYID/USUBJID duplicates for studies with multiple STRAIN registered in TS               
    animalSTRAINall<-               
      merge(merge(tsSTRAINall,
                  animalList[,.(STUDYID, USUBJID)],
                  by='STUDYID'),
            merge(foundAnimalsSPECIES[,.(STUDYID,USUBJID)], 
                  DM[,.(STUDYID, USUBJID, SETCD, STRAIN_DM=ifelse(STRAIN=="",as.character(NA),toupper(trimws(STRAIN))))],
                  by=c('STUDYID','USUBJID')),
            by=c('STUDYID','USUBJID'))
    
    # Join the list of studies/animals/strain with TX to get all set level STRAIN
    #  - add variable STRAIN with the first non-empty strain value from DM, TX or TS
    #  - add variable with count of unique USUBJID per study (there is expected to be one usubjid per studyid per TSPARMCD 'STRAIN' )
    animalSTRAINall<-
      merge(animalSTRAINall, 
            unique(TX[TXPARMCD=='STRAIN',.(STUDYID,SETCD,STRAIN_TX=toupper(trimws(TXVAL)))]), 
            by=c('STUDYID','SETCD'), all.x=TRUE )[,`:=` (STRAIN=fcoalesce(as.character(STRAIN_DM),as.character(STRAIN_TX),as.character(STRAIN_TS)))]
    animalSTRAINall[, `:=` (NUM_ANIMALS = .N), by = .(STUDYID, USUBJID)]
    
    # Identify uncertain animals - add variable UNCERTAIN_MGS
    animalSTRAINall[,`:=` (UNCERTAIN_MSG=mapply(identifyUncertainSTRAIN, STRAIN, STRAIN_TS, STRAIN_TX, STRAIN_DM,  ALL_STRAIN_TS, NUM_STRAIN_TS, NUM_ANIMALS ))]
    
    # Extract unique set of animals and related strain - exclude uncertain animals
    animalSTRAINallUniq<-unique(animalSTRAINall[is.na(UNCERTAIN_MSG), .(STUDYID, USUBJID, STRAIN)])
    
    # Extract animals match 
    foundAnimalsSTRAIN<-unique(animalSTRAINallUniq[STRAIN %in% strainFilter, .(STUDYID, USUBJID, STRAIN)])
    
    if (exclusively) {
      # Find studies with animals having other STRAIN than the requested
      foundAnimalsSTRAIN<-
        merge(foundAnimalsSTRAIN,
              # Set of studies to keep:
              fsetdiff(unique(foundAnimalsSTRAIN[,.(STUDYID)]),
                       # Set of studies (included in the found set of animals with matching STRAIN values) with possible 
                       # STRAIN values not included in the strainFilter:  
                       unique(fsetdiff(merge(# Set of possible STRAIN values per study in the input set of animals:
                                             unique(animalSTRAINallUniq[,.(STUDYID, STRAIN)]),
                                             unique(foundAnimalsSTRAIN[,.(STUDYID)]), by='STUDYID'),
                                       unique(foundAnimalsSTRAIN[,.(STUDYID, STRAIN)]))[,.(STUDYID)])),
              by='STUDYID')
      
    }
  
    # Merge set of animals with the set of animals extracted at SPECIES level to get the SPECIES values
    foundAnimals<-merge(foundAnimalsSPECIES,
                        foundAnimalsSTRAIN,
                        by=c('STUDYID','USUBJID'))
    
    if (inclUncertain) {
        # Extract and merge the sets of uncertain animals identified at species and/or strain level
        uncertainAnimals<-
          merge(unique(animalSPECIESall[!is.na(UNCERTAIN_MSG), .(STUDYID, USUBJID, SPECIES, UNCERTAIN_MSG)]),
                unique(animalSTRAINall[!is.na(UNCERTAIN_MSG), .(STUDYID, USUBJID, STRAIN, UNCERTAIN_MSG)]),
                by=c('STUDYID','USUBJID'), all = TRUE)
        #  Merge the UNCERTAIN_MSG from each tables into one column
        #  - non-empty messages are separated by ' & '
        #  - The function name is included as first part of the combined texts
        #  - exclude the original UNCERTAIN_MSG columns after the merge  
        uncertainAnimals[,`:=` (UNCERTAIN_MSG = ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y), 
                                                       paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep=' & '),
                                                       fcoalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)))][, `:=` (UNCERTAIN_MSG.x=NULL,UNCERTAIN_MSG.y=NULL)]
        # Add the set of uncertain animals to the set of found animals
        foundAnimals<-rbindlist(list(foundAnimals,uncertainAnimals), use.names=TRUE, fill=TRUE)
    }
  }
  
  ##################################################################################################################
  
  # Handling of the final set of animals to return
  
  # Merge the set of extracted animals with the input set of animals to keep
  # any additional columns from the input table 
  foundAnimals<-merge(foundAnimals, animalList, by=c('STUDYID', 'USUBJID'))
  if (inclUncertain)
    if ("UNCERTAIN_MSG.y" %in% names(foundAnimals)) {
      # An UNCERTAIN_MSG column is included in both input and found list of animals
      #  - merge the UNCERTAIN_MSG from each of the merged tables into one column
      #  - non-empty messages are separated by '|'
      #  - exclude the original UNCERTAIN_MSG columns after the merge  
      foundAnimals<-foundAnimals[,`:=` (UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y), 
                                                              paste(UNCERTAIN_MSG.y, paste0('FilterAnimalsSpeciesStrain: ',UNCERTAIN_MSG.x), sep='|'),
                                                              fcoalesce(ifelse(!is.na(UNCERTAIN_MSG.x), 
                                                                               paste0('FilterAnimalsSpeciesStrain: ',UNCERTAIN_MSG.x), NA ), 
                                                                        UNCERTAIN_MSG.y)))][, `:=` (UNCERTAIN_MSG.x=NULL,UNCERTAIN_MSG.y=NULL)]
  }
  # Return list of found animals  
  return(foundAnimals)
  
}
