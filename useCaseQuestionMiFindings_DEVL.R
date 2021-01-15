###################################################################################
# Script name   : <script name>.R
# Date Created  : dd-Mon-2020
# Programmer    : <programmer name>
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : <overall purpose of script>
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
<<<<<<< HEAD
pStudyDesign      <-  "PARALLEL" # CT: DESIGN
pSpecies          <-  "DOG" # CT: SPECIES (extensible)
pStrain           <- "BEAGLE" # CT: STRAIN  (extensible)
pRoute        <-c("ORAL", "ORAL GAVAGE")
pFromDTC          <-  "2017"
pToDTC            <-  "2020"
pSex              <-  "M"                # CT: SEX
pStudyPhase       <-  "Treatment"        # Valid: "Screening", "Treatment", "Recovery"
pStudyPhaseInclUncertain <- FALSE    # Valid: TRUE, FALSE
pFindingsFromAge  <-  "12m"
pFindingsToAge    <-  "18m"
=======
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
#pSpecies         <- c("DOG","RAT")
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
#pStudyPhase       <-  c("Treatment", "Screening")
pFindingsFromAge  <-  "4m"
pFindingsToAge    <-  "6m"

pInclUncertain    <-  TRUE
pExclusively      <-  FALSE
pMatchAll         <-  FALSE
>>>>>>> devl_bo
###################################################################################

library(data.table)

# define the path to R scripts to actual script location
dummyuseCaseQuestionMiFindings<-function() {
    # dummy function only to be used to get this script's location
}
setwd(getSrcDirectory(dummyuseCaseQuestionMiFindings))

source("initSENDFunctions.R")

# Get the combined list of all studies to look into 
studiesAll<-GetStudyListSDESIGN(studyDesignFilter = pStudyDesign, 
                                studyList         = GetStudyListSTSTDTC(fromDTC       = pFromDTC, 
                                                                        toDTC         = pToDTC, 
                                                                        inclUncertain = pInclUncertain), 
                                inclUncertain     = pInclUncertain, 
                                exclusively       = pExclusively)

# Extract subset of of all control animals for the selected  studies 
controlAnimalsAll<-GetControlAnimals(studyList     = studiesAll, 
                                     inclUncertain = pInclUncertain)

# Limit to set of animals to relevant sex
controlAnimals<-filterAnimalsSex(animalList    = controlAnimalsAll, 
                                 sexFilter     = pSex, 
                                 inclUncertain = pInclUncertain)

# Limit to set of animals to relevant species/strain(s)
controlAnimals<-FilterAnimalsSpeciesStrain(animalList    = controlAnimals, 
                                           speciesFilter = pSpecies, 
                                           strainFilter  = pStrain, 
                                           inclUncertain = pInclUncertain, 
                                           exclusively   = pExclusively)

# Limit to set of animals to relevant route(s) of administration
controlAnimals<-FilterAnimalListRoute(animalList    = controlAnimals, 
                                      routeFilter   = pRoute, 
                                      inclUncertain = pInclUncertain,
                                      exclusively   = pExclusively, 
                                      matchAll      = pMatchAll)

# Extract all MI findings for the control animals

allMI<-ExtractSubjData(domain     = "MI", 
                       animalList = controlAnimals)

# Add animal age of finding column
allMI<-addFindingsAnimalAge(domain        = "MI", 
                            findings      = allMI, 
                            inclUncertain = pInclUncertain)

# Filter MI findings - save Dosing phase findings (and include rows where phase cannot be identified )
dosingMI<-FilterFindingsPhase(domain        = "MI", 
                              findings      = allMI, 
                              phaseFilter   = pStudyPhase,  
                              inclUncertain = pInclUncertain)

# Filter MI findings for a specific age interval
dosingMI<-filterFindingsAnimalAge(findings      = dosingMI, 
                                  fromAge       = pFindingsFromAge, 
                                  toAge         = pFindingsToAge,  
                                  inclUncertain = pInclUncertain)

##########################################################################
message("Execution time: ",round(difftime(Sys.time(), start, units = 'min'),1)," minutes")

