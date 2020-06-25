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
#                   readxl
#                   
#
###################################################################################

start<-Sys.time()

###################################################################################
# Input parameter values
pStudyDesign      <-  "PARALLEL"         # CT: DESIGN
#pSpecies          <-  "Rat"              # CT: SPECIES (extensible)
#pStrain           <-  "SPRAGUE-DAWLEY"   # CT: STRAIN  (extensible)
#pStrain           <-  "wistar"
#pStrain           <-  ""
pSpecies          <-  "RAT"
#pStrain          <-  "WISTAR"
pStrain           <- c("WISTAR","SPRAGUE-DAWLEY")
#pSpecies         <- "DOG  "
#pSpecies         <- c("MONKEY","RAT")
#pStrain           <-  "BEAGLE"
#pRoute           <-  c("SUBCUTANEOUS")     # CT: ROUTE   (extensible)
#pRoute           <- c("INTRAPERITONEAL")
#pRoute           <- c("ORAL", "ORAL GAVAGE")
#pRoute           <- c("ORAL GAVAGE")
pRoute            <- c("INTRAVENOUS BOLUS", "SUBCUTANEOUS")
pFromDTC          <-  "2016"
pToDTC            <-  "2019"
pSex              <-  "M"                # CT: SEX
pStudyPhase       <-  "Treatment"        # Valid: "Screening", "Treatment", "Recovery"
pStudyPhaseInclUncertain <- TRUE    # Valid: TRUE, FALSE
pFindingsFromAge  <-  "4m"
pFindingsToAge    <-  "6m"

pInclUncertain=TRUE
###################################################################################

library(data.table)

# define the path to R scripts to actual script location
dummyuseCaseQuestionMiFindings<-function() {
    # dummy function only to be used to get this script's location
}
setwd(getSrcDirectory(dummyuseCaseQuestionMiFindings))

source("miscFunctions.R")
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


# Get the combined list of all studies to look into 
studiesAll<-GetStudyListSDESIGN(pStudyDesign, 
                                studyList=GetStudyListSTSTDTC(pFromDTC, pToDTC, 
                                                              inclUncertain=pInclUncertain), 
                                inclUncertain=pInclUncertain)

# Extract subset of of all control animals for the selected  studies 
controlAnimalsAll<-GetControlAnimals(studiesAll, inclUncertain=pInclUncertain)

# Limit to set of animals to relevant sex
controlAnimals<-filterAnimalsSex(controlAnimalsAll, pSex, inclUncertain=pInclUncertain)

# Limit to set of animals to relevant species/strain(s)
controlAnimals<-FilterAnimalsSpeciesStrain(controlAnimals, pSpecies, pStrain, inclUncertain=pInclUncertain)

# Limit to set of animals to relevant route(s) of administration
controlAnimals<-FilterAnimalListRoute(controlAnimals, pRoute, inclUncertain=pInclUncertain)

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

