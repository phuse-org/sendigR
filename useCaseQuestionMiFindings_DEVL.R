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
# Usage notes   : Used packages:
#                   data.table
#                   parsedate
#                   stringr
#                   stringi
#                   RSQLite
#                   varhandle
#                   
#
###################################################################################

start<-Sys.time()

###################################################################################
# Input parameter values
pStudyDesign      <-  "PARALLEL"         # CT: DESIGN
#pSpecies          <-  "RAT"              # CT: SPECIES (extensible)
#pStrain           <-  "SPRAGUE-DAWLEY"   # CT: STRAIN  (extensible)
#pStrain           <-  "WISTAR"
pSpecies          <-  "RAT"
#pStrain          <-  "WISTAR"
pStrain           <- c("WISTAR","SPRAGUE-DAWLEY")
#pRoute           <-  c("SUBCUTANEOUS")     # CT: ROUTE   (extensible)
#pRoute           <- c("INTRAPERITONEAL")
pRoute            <- c("INTRAVENOUS BOLUS", "SUBCUTANEOUS")
pFromDTC          <-  "2017"
pToDTC            <-  "2019"
pSex              <-  "M"                # CT: SEX
pStudyPhase       <-  "Treatment"        # Valid: "Screening", "Treatment", "Recovery"
pStudyPhaseInclUncertain <- TRUE    # Valid: TRUE, FALSE
pFindingsFromAge  <-  "4m"
pFindingsToAge    <-  "6m"
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
source("studyListStudyStartDate.R")
source("animalListControl.R")
source("filterAnimalsSex.R")
source("subjDataExtract.R")
source("filterFindingsPhase.R")
source("addFindingsAnimalAge.R")
source("filterFindingsAnimalAge.R")

# Extract list of studies per condition
studiesSDESIGN<-GetStudyListSDESIGN(pStudyDesign)
#studiesSPECIES_STRAIN<-GetStudyListSPECIES_STRAIN(pSpecies, pStrain)
studiesSTSTDTC<-GetStudyListSTSTDTC(pFromDTC,pToDTC)

# Get the combined list of all studies to look into 
studiesAll<-merge(studiesSDESIGN, studiesSTSTDTC, by='STUDYID')

# Get a unique list of the study ud values
studiesAllID<-unique(studiesAll[ ,.(STUDYID)])

# Extract subset of of all control animals for the selected  studies 
controlAnimals<-GetControlAnimals(studiesAllID)

# Limit to set of animals to relevant sex
controlAnimals<-filterAnimalsSex(controlAnimals, pSex)

# Limit to set of animals to relevant species/strain(s)
controlAnimals<-FilterAnimalsSpeciesStrain(controlAnimals, pSpecies, pStrain)

# Limit to set of animals to relevant route(s) of administration
controlAnimals<-FilterAnimalListRoute(controlAnimals, pRoute)

# Extract all MI findings for the control animals
allMI<-ExtractSubjData("MI", controlAnimals)

# Add animal age of finding column
allMI<-addFindingsAnimalAge('mi', allMI, inclUncertainMsg=TRUE)

# Filter MI findings - save Dosing phase findings (and include rows where phase cannot be identified )
dosingMI<-FilterFindingsPhase('MI', allMI, pStudyPhase, pStudyPhaseInclUncertain)

# Filter MI findings for a specific age interval
dosingMI<-filterFindingsAnimalAge(dosingMI, pFindingsFromAge, pFindingsToAge)

##########################################################################
message("Execution time: ",round(difftime(Sys.time(), start, units = 'min'),1)," minutes")

