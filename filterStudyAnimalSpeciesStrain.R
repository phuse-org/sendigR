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
# Purpose       : Extract a list of SEND study ids which fulfills a specified species
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
library(data.table)
#library(DescTools)

###################################################################################
# Function:     : GetStudyListSPECIES_STRAIN
#
# Description   : Returns a data table with a list of studyid, species and strain values
#                 extracted from TS (where TSPARMCD in ('SPECIES', 'STRAIN') where
#                 the TSVAL for species is equal to a given species input value and the  
#                 TSVAL for strain is equal to a given strain input value (it,s optional 
#                 to specify a strain input value).
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
# Parameters    : speciesFilter:  mandatory, the species to use as criterion for filtering of the
#                                 study id values.
#                                 It can be a single string, a vector or a list of multiple strings.
#                 strainFilter:   optional, the strain to use as criterion for filtering of the
#                                 study id values.
#                                 It can be a single string, a vector or a list of multiple strings.
#                 studyList:      optional, a data table with a list of studies to 
#                                 limit the output to be within this set of studies
#
###################################################################################
GetStudyListSPECIES_STRAIN<-function(speciesFilter=NULL, strainFilter=NULL, studyList=NULL) {

  if (is.null(speciesFilter) | isTRUE(is.na(speciesFilter)) | isTRUE(speciesFilter=="")) {
    stop("SpeciesFilter must be specified")
  } 
  
  if (!exists("TS")) {
    # import TS if it's not already exists
    importSENDDomains(c("TS"))
  }
  
  # Extract all TS rows for parameters SPECIES and STRAIN
  #  - rename TSVAL to SPECIES and STRAIN respectively
  #  - merge into one table
  #  - remove duplicates
  TSAllSPECIES_STRAIN<-unique(merge(TS[TSPARMCD=='SPECIES', .(STUDYID,TSGRPID, SPECIES=toupper(TSVAL))], 
                                    TS[TSPARMCD=='STRAIN', .(STUDYID,TSGRPID, STRAIN=toupper(TSVAL))],
                                    by=c('STUDYID','TSGRPID'))[,.(STUDYID,SPECIES,STRAIN)])
    
  # Add variable with count of of distinct
  # - species/strain combinations 
  # - species
  # per study.
  TSAllSPECIES_STRAIN[, `:=` (NUM_SPECIES = uniqueN(SPECIES), NUM_SPECIES_STRAIN = .N), by = STUDYID]

  # species part of filter condition
  where<-"SPECIES %in% toupper(trimws(speciesFilter))"
  
  if (!(is.null(strainFilter) | isTRUE(is.na(strainFilter)) | isTRUE(strainFilter==""))) {
    # a strain has been specified - add this part to filter condition
    where<-paste(where, " & STRAIN %in% toupper(trimws(strainFilter))", sep="")
  }
  
  # Construct complete filtering command, execute and return resulting data table
  expr<-paste('subset(TSAllSPECIES_STRAIN, ', where, ')', sep="")
  return(eval(str2lang(expr)))
}


###################################################################################
# Function:     : FilterAnimalListSpeciesStrain
#
# Description   : Returns a data table with a set of animals extratced from the table of  
#                 animals given as input where the animals fits the species vaæue(s) given 
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
#                 DM.SPECIES or DM.SPECIES/STRAIN values or are included in TX in rial set 
#                 with match TX.SPECIES or TX.SPECIES/STRAIN (i.e. TXVAL where TXPARMCD is 
#                 SPECIES' or 'STRAIN')
#                 The comparisons of the species and strain values are done case 
#                 insensitive.
#
# Input         : The TX and DM domains - are imported from the pooled SEND data store if 
#                 the doen't exist in workspace.
#                 Data tables specified in the input parameters:
#                   - animalList
#                     The list a animals to be filtered.
#                     Must contain these character variables:
#                       STUDYID
#                       USUBJID
#                     other variables may be included
#                   
#
# Output        : A data table with two character columns:
#                   STUDYID
#                   USUBJID
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
#                   
###################################################################################
FilterAnimalsSpeciesStrain<-function(animalList=NULL, speciesFilter=NULL, strainFilter=NULL) {
  
  if (is.null(animalList) | isTRUE(is.na(animalList)) | isTRUE(animalList=="")) {
    stop("Input parameter animalList must have assigned a data table ")
  } 
  if (is.null(speciesFilter) | isTRUE(is.na(speciesFilter)) | isTRUE(speciesFilter=="")) {
    stop("SpeciesFilter must be specified")
  } 
  
  # List of studyid values included in the input table of animals
  animalStudies<-unique(animalList[,.(STUDYID)])
  
  # import TX and DM if they  not already exists
  if (!exists("TX")) {
    importSENDDomains(c("TX"), animalStudies)
  }
  if (!exists("DM")) {
    importSENDDomains(c("DM"), animalStudies)
  }

  # Get list of studies with SPECIES/STRAIN parameter value(s) included in the speciesFIlter/strainFilter
  tsSPECIES_STRAIN<-GetStudyListSPECIES_STRAIN(speciesFilter, strainFilter, animalStudies)
  
  if ((is.null(strainFilter) | isTRUE(is.na(strainFilter)) | isTRUE(strainFilter==""))) {
    # Only speciesFilter has been specified:
    
    # Extract list of animals for studies with only one SPECIES specified in TS
    studyLvlAnimals<-merge(tsSPECIES_STRAIN[NUM_SPECIES==1, .(STUDYID, SPECIES, STRAIN)], 
                           animalList[,.(STUDYID, USUBJID)], by='STUDYID')[,.(STUDYID, USUBJID, SPECIES, STRAIN)]
    
    # Extract list of animals belonging to studies with no species value registered or with more 
    # than one species value registered in TS:
    #  - extract list of studies included in animalList which are not included in studyLvlAnimals
    #  - join with DM to get all rows for these studies 
    #  - outer join with TX to get TXPARMCD 'SPECIES' for all related trial sets 
    #  - set SPECIES value to DM.SPECIES if it contains a value, else TX.SPECIES
    #  - include only rows where SPECIES fits the speciesFIlter
    #  - join with unique set of studyid/usubjid from animalList to extract the animals to include in the output table
    TxDmLvlAnimals<-
      merge(merge(merge(DM[,.(STUDYID, SETCD, USUBJID, dmSPECIES=toupper(SPECIES))],
                        fsetdiff(animalStudies, studyLvlAnimals[,.(STUDYID)]),
                        by='STUDYID'),
                  TX[TXPARMCD=='SPECIES',.(STUDYID, SETCD, txSPECIES=toupper(TXVAL))],
                  by=c('STUDYID','SETCD'),all.x = TRUE)[,.(STUDYID, USUBJID, SPECIES = fcoalesce(dmSPECIES, txSPECIES))][SPECIES %in% speciesFilter],
            unique(animalList[,.(STUDYID,USUBJID)]), 
            by=c('STUDYID', 'USUBJID'))
  } else {
    # Both species and strain have been specified:
    
    # Extract list of animals for studies with only one SPECIES and STRAIN specified in TS
    studyLvlAnimals<-merge(tsSPECIES_STRAIN[NUM_SPECIES_STRAIN==1, .(STUDYID, SPECIES, STRAIN)], 
                           animalList[,.(STUDYID, USUBJID)], by='STUDYID')[,.(STUDYID, USUBJID, SPECIES, STRAIN)]
    
    # Extract list of animals belonging to studies with no species/strain value registered or with more 
    # than one species/strain values registered in TS:
    #  - extract list of studies included in animalList which are not included in studyLvlAnimals
    #  - join with DM to get all rows for these studies 
    #  - outer join with TX two times to get TXPARMCD 'SPECIES' and 'STRAIN' respectively for all related trial sets
    #  - set SPECIES/STRAIN values to DM.SPECIES/DM.STRAIN if they contain values, else TX.SPECIES/TX.STRAIN
    #  - include only rows where SPECIES and STRAIN fits the speciesFilter and strainFilter respectively
    #  - join with unique set of studyid/usubjid from animalList to extract the animals to include in the output table
    TxDmLvlAnimals<-
      merge(merge(merge(DM[,.(STUDYID, SETCD, USUBJID, dmSPECIES=toupper(SPECIES), dmSTRAIN=toupper(STRAIN))],
                        fsetdiff(animalStudies, studyLvlAnimals[,.(STUDYID)]),
                        by='STUDYID'),
                  merge(TX[TXPARMCD=='SPECIES',.(STUDYID, SETCD, txSPECIES=toupper(TXVAL))],
                        TX[TXPARMCD=='STRAIN',.(STUDYID, SETCD, txSTRAIN=toupper(TXVAL))],
                        by=c('STUDYID','SETCD'),all = TRUE),
                  by=c('STUDYID','SETCD'),all.x = TRUE)[,.(STUDYID, USUBJID, SPECIES = fcoalesce(dmSPECIES, txSPECIES), STRAIN = fcoalesce(dmSTRAIN, txSTRAIN))][SPECIES %in% speciesFilter & STRAIN %in% strainFilter],
            unique(animalList[,.(STUDYID,USUBJID)]), 
            by=c('STUDYID', 'USUBJID'))
  }
  # Return the list of extracted animals merged with the input set of animals to keep
  # any additional columns from the input table 
  return(merge(animalList,
               rbindlist(list(studyLvlAnimals, TxDmLvlAnimals),use.names=TRUE, fill=TRUE),
               by=c('STUDYID', 'USUBJID'))) 
}
