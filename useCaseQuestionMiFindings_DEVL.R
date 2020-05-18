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
source("dbManager.R")


# total studies
totalStudiesTS <- GenericQuery('SELECT DISTINCT STUDYID FROM TS')

nStudiesTotal <- nrow(totalStudiesTS)

print(sprintf("Total Number of Studies in TS: %s", nStudiesTotal))

#studiesSPECIES_STRAIN<-GetStudyListSPECIES_STRAIN(pSpecies, pStrain)
studiesSTSTDTC<-GetStudyListSTSTDTC(pFromDTC,pToDTC)

nStudies <- nrow(studiesSTSTDTC)

print(sprintf("Studies after Start Date: %s/%s", nStudies, nStudiesTotal))


# Extract list of studies per condition
studiesSDESIGN<-GetStudyListSDESIGN(pStudyDesign)

# Get the combined list of all studies to look into 
studiesAll<-merge(studiesSDESIGN, studiesSTSTDTC, by='STUDYID')

# Get a unique list of the study ud values
studiesAllID<-unique(studiesAll[ ,.(STUDYID)])

nStudies <- nrow(studiesAllID)

print(sprintf("Studies after Study Design: %s/%s", nStudies, nStudiesTotal))


# total animals
totalAnimalsDM <- GenericQuery('SELECT DISTINCT USUBJID FROM DM')

nAnimalsTotal <- nrow(totalAnimalsDM)

# Extract subset of of all control animals for the selected  studies 
controlAnimals<-GetControlAnimals(studiesAllID)

nAnimals <- nrow(controlAnimals)

print(sprintf("Animals after control: %s/%s", nAnimals, nAnimalsTotal))

# Limit to set of animals to relevant route(s) of administration
controlAnimals<-FilterAnimalListRoute(controlAnimals, pRoute)

nAnimals <- nrow(controlAnimals)

print(sprintf("Animals after Route: %s/%s", nAnimals, nAnimalsTotal))


# Limit to set of animals to relevant species/strain(s)
controlAnimals<-FilterAnimalsSpeciesStrain(controlAnimals, pSpecies)

nAnimals <- nrow(controlAnimals)

print(sprintf("Animals after Species: %s/%s", nAnimals, nAnimalsTotal))

# Limit to set of animals to relevant species/strain(s)
controlAnimals<-FilterAnimalsSpeciesStrain(controlAnimals, pSpecies, pStrain)

nAnimals <- nrow(controlAnimals)

print(sprintf("Animals after Strain: %s/%s", nAnimals, nAnimalsTotal))


# Limit to set of animals to relevant sex
controlAnimals<-filterAnimalsSex(controlAnimals, pSex)

nAnimals <- nrow(controlAnimals)

print(sprintf("Animals after Sex: %s/%s", nAnimals, nAnimalsTotal))

# total studies
totalFindingsMI <- GenericQuery('SELECT DISTINCT * FROM MI')

nFindingsTotal <- nrow(totalFindingsMI)

print(sprintf("Total Number of Findings in MI: %s", nFindingsTotal))


# Extract all MI findings for the control animals
allMI<-ExtractSubjData("MI", controlAnimals)

# Add animal age of finding column
allMI<-addFindingsAnimalAge('mi', allMI, inclUncertainMsg=TRUE)

# Filter MI findings - save Dosing phase findings (and include rows where phase cannot be identified )
dosingMI <- FilterFindingsPhase('MI', allMI, pStudyPhase, pStudyPhaseInclUncertain)

nFindings <- nrow(dosingMI)

print(sprintf("Findings after Phase: %s/%s", nFindings, nFindingsTotal))


# Filter MI findings for a specific age interval
dosingMI<-filterFindingsAnimalAge(dosingMI, pFindingsFromAge, pFindingsToAge)

nFindings <- nrow(dosingMI)

print(sprintf("Findings after Age: %s/%s", nFindings, nFindingsTotal))


dosingMI$MISTRESC <- toupper(dosingMI$MISTRESC)


findingsCount <- dosingMI %>%
          dplyr::count(MISPEC, MISTRESC) %>%
          dplyr::arrange(-n) %>%
          dplyr::filter(MISTRESC != 'NORMAL', MISTRESC != 'UNREMARKABLE', MISTRESC != '')


nUniqueAnimals <- length(unique(dosingMI$USUBJID))

findingsCount$Incidence <- (findingsCount$n / nUniqueAnimals) * 100

findingsCount$Incidence <- paste0(round(findingsCount$Incidence, 2), '%')

findingsCount <- dplyr::select(findingsCount, -n)

write.csv(findingsCount, 'data/RankedMIFindings.csv')

##########################################################################
message("Execution time: ",round(difftime(Sys.time(), start, units = 'min'),1)," minutes")

