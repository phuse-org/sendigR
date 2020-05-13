###################################################################################
# Script name   : <script name>.R
# Date Created  : dd-Mon-2020
# Documentation : <if relevant, reference to specification document>
# Programmer    : <programmer name>
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : <overall prupose of script>
#
# Description   : <detailed description of the algorithm(s) implemented in the script>
#
# Input         : <list of input data - i.e. SEND domains and other potiential input 
#                 files and/or data frames etc.>
#
# Output        : <list of output data - files, data frames or other kind of generated 
#                  output>
#
# Parameters    : <if relevant - list of user specified input parameters to be applied
#                 - e.g. for a function created by the script 
#
# Usage notes   : <how to use the script - e.g. the preconditions to be fulfilled 
#                 before script execution>
#
###################################################################################

library(data.table)

# define the path to R scripts to actual script location
dummyuseCaseQuestionMiFindings<-function() {
    # dummy function only to be used to get this script's location
}
setwd(getSrcDirectory(dummyuseCaseQuestionMiFindings))

source("importSENDDomains.R")
source("studyListStudyDesign.R")
source("filterStudyAnimalSpeciesStrain.R")
source("filterStudyAnimalRoute.R")
source("animalListControl.R")
source("subjDataExtract.R")
source("filterFindingsPhase.R")
source("addFindingsAnimalAge.R")

# Extract list of studies per condition
studiesSDESIGN<-GetStudyListSDESIGN("PARALLEL")
studiesSPECIES_STRAIN<- rbindlist(list(GetStudyListSPECIES_STRAIN("RAT", "SPRAGUE-DAWLEY"),GetStudyListSPECIES_STRAIN("MONKEY")))
studiesROUTE<-GetStudyListROUTE("SUBCUTANEOUS")

# Get the combined list of all studies to look into 
#  - inclusive information of potentiel species/strain and/or route values to be 
# used for extraction of subset of animals
setkey(studiesSDESIGN, "STUDYID")
setkey(studiesSPECIES_STRAIN, "STUDYID")
setkey(studiesROUTE, "STUDYID")
studiesAll<-merge(merge(studiesSDESIGN,studiesSPECIES_STRAIN),studiesROUTE)


# Get a unique list of the study ud values
studiesAllID<-unique(studiesAll[ ,.(STUDYID)])

# Get a unique list of study id and species to use for extraction of subsets of animals.
# To be used if only a species is used as input to GetStudyListSPECIES_STRAIN
studiesSubsetSpecies<-unique(studiesAll[NUM_SPECIES>1 ,.(STUDYID, SPECIES)])

# Get a unique list of study id and species/strain to use for extraction of subsets of animals
# To be used if both a species and a strain are used as input to GetStudyListSPECIES_STRAIN
studiesSubsetSpeciesStrain<-unique(studiesAll[NUM_SPECIES_STRAIN>1 ,.(STUDYID, SPECIES, STRAIN)])

# Get a unique list of study id and route to use for extraction of subsets of animals
studiesSubsetRoute<-unique(studiesAll[NUM_ROUTE>1 ,.(STUDYID, ROUTE)])

# Extract subset of of all control animals for the selected  studies 
controlAnimals<-GetControlAnimals(studiesAllID)

# If any studies are identified with multiple routes specied at study level
# - filter the list of control animals to remove the animals which are include in EX with another route than the specified route(s).
if (studiesSubsetRoute[,.N] != 0) {
  controlAnimals<-FilterAnimalListRoute(controlAnimals, studiesSubsetRoute)
}

# Extract all MI findings for the control animals
allMI<-ExtractSubjData("MI", controlAnimals)

# Filter MI findings - save Dosing findings (and included rows where phase cannot be identified )
dosingMI<-FilterFindingsPhase('MI', allMI,'Treatment',inclUncertain = TRUE)

# Add animal age of finding column
dosingMI<-addFindingsAnimalAge('mi', dosingMI)
