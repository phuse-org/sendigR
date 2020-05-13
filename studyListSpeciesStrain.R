###################################################################################
# Script name   : studyListSpeciesStrain.R
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
# Description   : The function GetStudyListSPECIES_STRAIN is defined:
#                 Returns a data table with a list of studyid, species and strain values
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
# Parameters    : The function GetStudyListSPECIES_STRAIN are defined with two input 
#                 parameters:
#                   species:  mandatory, the species to use as criterion for filtering of the
#                             study id values.
#                   strain:   optional, the strain to use as criterion for filtering of the
#                             study id values.
#
# Usage notes   : Source the script.
#                 Execute the function GetStudyListSPECIES_STRAIN as many times as needed to extract 
#                 list(s) of studies for the wanted species/strains.
#                 Examples:
#                   GetStudyListSPECIES_STRAIN("monkey")
#                   GetStudyListSPECIES_STRAIN("RAT")
#                   GetStudyListSPECIES_STRAIN("rat","WISTAR HAN")
# 
#######################################################################################################################################################################

library(data.table)
library(DescTools)

GetStudyListSPECIES_STRAIN<-function(species=NULL, strain=NULL) {
  
  ########################################################################
  # Generate a global data table with all studies and related species and
  # and strians included in the TS domain. 
  # Calculate and add two column with the number of distinct 
  #  - species
  #  - combined species/sttain
  # values for each study
  # 
  prepareTS_SPECIES_STRAIN<-function() {
    if (!exists("TS")) {
      # import TS if it's not already exists
      importSENDDomains(c("TS"))
    }
    
    # Extract species and strains from TS and join together per studyid/tsgrpid - remove duplicates
    #setnames(ts_spec<-subset(TS, TSPARMCD=='SPECIES',c("STUDYID","TSGRPID", "TSVAL") ), "TSVAL", "SPECIES")
    #setnames(ts_strain<-subset(TS, TSPARMCD=='STRAIN',c("STUDYID","TSGRPID", "TSVAL") ), "TSVAL", "STRAIN")
    ts_spec<-TS[TSPARMCD=='SPECIES', .(STUDYID,TSGRPID, SPECIES=TSVAL)]
    ts_strain<-TS[TSPARMCD=='STRAIN', .(STUDYID,TSGRPID, STRAIN=TSVAL)]
    assign("TSAllSPECIES_STRAIN", unique(subset(merge(x=ts_spec, y=ts_strain), TRUE, c("STUDYID","SPECIES","STRAIN"))), envir=.GlobalEnv)
    
    # Add variable with count of of distinct
    # - species/strain combinations 
    # - species
    # per study.
    TSAllSPECIES_STRAIN[, `:=` (NUM_SPECIES = uniqueN(SPECIES), NUM_SPECIES_STRAIN = .N), by = STUDYID]
  }
  ## End of prepareTS_SPECIES_STRAIN #####################################
  
  if (!exists("TSAllSPECIES_STRAIN")) {
    # Initial extraction  of TS species/strain data - executed once.
    prepareTS_SPECIES_STRAIN()
  }
  
  if (is.null(species)) {
    print("ERROR: Species must be specified")
  } 
  else {
    # species part of filter condition
    where<-paste("toupper(SPECIES)=='", toupper(StrTrim(species)), "'", sep="")
    
    if (!is.null(strain)) {
      # add strain part of filter condition
      where<-paste(where, " & toupper(STRAIN)=='", toupper(StrTrim(strain)), "'", sep="")
    }
    # Construct complete filtering command, execute and return resulting data table
    expr<-paste('subset(TSAllSPECIES_STRAIN, ', where, ')', sep="")
    eval(str2lang(expr))
  }
}





