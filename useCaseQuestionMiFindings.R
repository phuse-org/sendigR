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
#                   DescTools
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
pStrain           <- "BEAGLE"
# pRoute            <-  "SUBCUTANEOUS"     # CT: ROUTE   (extensible)
#pRoute            <- "INTRAVENOUS BOLUS"
pRouteList        <-c("ORAL", "ORAL GAVAGE")
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

# Extract list of studies per condition
studiesSDESIGN<-GetStudyListSDESIGN(pStudyDesign)
studiesSPECIES_STRAIN<-GetStudyListSPECIES_STRAIN(pSpecies, pStrain)
studiesROUTE<-data.table()
for (route in pRouteList) {
  studiesROUTE <- rbind(studiesROUTE, GetStudyListROUTE(route))
}
studiesSTSTDTC<-GetStudyListSTSTDTC(pFromDTC,pToDTC)

# Get the combined list of all studies to look into 
#  - inclusive information of potentiel species/strain and/or route values to be 
# used for extraction of subset of animals
setkey(studiesSDESIGN, "STUDYID")
setkey(studiesSPECIES_STRAIN, "STUDYID")
setkey(studiesROUTE, "STUDYID")
setkey(studiesSTSTDTC, "STUDYID")

# Merging one at a time to be able to get the counts 
# need for Table X.A in the manuscrpint 

studiesSDESIGN<-unique(GetStudyListSDESIGN(pStudyDesign))
studiesSPECIES_STRAIN<-unique(GetStudyListSPECIES_STRAIN(pSpecies, pStrain))
# there are two studies listed as having 
# '2' routes, but are ORAL and ORAL GAVAGE
studiesROUTE<-unique(studiesROUTE)
studiesSTSTDTC<-unique(GetStudyListSTSTDTC(pFromDTC,pToDTC))

# While order doesn't 
# necessarily matter, 
# the current Order of 
# Table X.A goes:
# Study Start Data
# Route
# Design 
# SPECIES STRAIN

# Get a unique list of the study ID values
# starting with Study start date
studiesAllID<-unique(studiesSTSTDTC[ ,.(STUDYID)])
print(sprintf("Current number of studies after study start date: %s", nrow(studiesAllID)))

# interesction of study ids from start date and route
studiesAllID <- unique(merge(studiesAllID, studiesROUTE[ ,.(STUDYID)], by='STUDYID'))
print(sprintf("Current number of studies after filtering ROUTE at study level: %s", nrow(studiesAllID)))

# now interesection of design as well 
studiesAllID <- unique(merge(studiesAllID, studiesSDESIGN[ ,.(STUDYID)]))
print(sprintf("Current number of studies after filtering DESGIN at study level: %s", nrow(studiesAllID)))

# now interesection of species and strain 
studiesAllID <- unique(merge(studiesAllID, studiesSPECIES_STRAIN[ ,.(STUDYID)]))
print(sprintf("Current number of studies after filtering Species and Strain at study level: %s", nrow(studiesAllID)))

studiesAll<-merge(merge(merge(studiesSDESIGN,studiesSPECIES_STRAIN),studiesROUTE), studiesSTSTDTC)

# move on to animal level 

# Extract subset of of all control animals for the selected  studies 
controlAnimals<-GetControlAnimals(studiesAll)
print(sprintf("Current number of animals: %s", nrow(controlAnimals)))

if (!is.null(pSpecies) & is.null(pStrain)) {
  # Only a species is used as input to GetStudyListSPECIES_STRAIN:

  # Get a unique list of study id and species to use for extraction of subsets of animals.
  studiesSubsetSpecies<-unique(studiesAll[NUM_SPECIES>1 ,.(STUDYID, SPECIES)])
  # If any studies are identified with multiple routes specified at study level
  # - filter the list of control animals to remove the animals which are include in EX with another route than the specified route(s).
  if (studiesSubsetSpecies[,.N] != 0) {
    controlAnimals<-FilterAnimalsSpeciesStrain(controlAnimals, speciesFilter=studiesSubsetSpecies)
  }
} else {
  # Both a species and a strain are used as input to GetStudyListSPECIES_STRAIN:

  # For studies with one species and multiple strains 
  # - get a unique list of study id and species/strain to use for extraction of subsets of animals 
  studiesSubsetStrain<-unique(studiesAll[NUM_SPECIES==1 & NUM_SPECIES_STRAIN>1 ,.(STUDYID, STRAIN)])
  # If any studies are identified with multiple species/strain specified at study level
  # - filter the list of control animals to remove the animals which are include in DM/TX with another species than the specified species/strain.
  if (studiesSubsetStrain[,.N] != 0) {
    controlAnimals<-FilterAnimalsSpeciesStrain(controlAnimals, strainFilter=studiesSubsetStrain)
  }  
  
  # For studies with multiple species and strains 
  # - get a unique list of study id and species/strain to use for extraction of subsets of animals 
  studiesSubsetSpeciesStrain<-unique(studiesAll[NUM_SPECIES>1 & NUM_SPECIES_STRAIN>1 ,.(STUDYID, SPECIES, STRAIN)])
  # If any studies are identified with multiple species/strain specified at study level
  # - filter the list of control animals to remove the animals which are include in DM/TX with another species than the specified species/strain.
  if (studiesSubsetSpeciesStrain[,.N] != 0) {
    controlAnimals<-FilterAnimalsSpeciesStrain(controlAnimals, speciesStrainFilter=studiesSubsetSpeciesStrain)
  }
}


print(sprintf("Current number of animals after filtering by num of species and strain
                    at the animal level: %s", nrow(controlAnimals)))

# Get a unique list of study id and route to use for extraction of subsets of animals
studiesSubsetRoute<-unique(studiesAll[NUM_ROUTE>1 ,.(STUDYID, ROUTE)])
if (studiesSubsetRoute[,.N] != 0) {
  controlAnimals<-FilterAnimalListRoute(controlAnimals, studiesSubsetRoute)
}

print(print(sprintf("Current number of animals after filtering by route at the animal level: %s", nrow(controlAnimals))))

# # Limit to set of animals to relevant sex
# controlAnimals<-filterAnimalsSex(controlAnimals, pSex)

controlData <- unique(ExtractSubjData("DM", controlAnimals)[,.(STUDYID,USUBJID,SEX)])
controlAnimals <- unique(merge(controlAnimals, controlData))
controlAnimals <- controlAnimals[controlAnimals$SEX == pSex]
print(sprintf("Number of control animals in the current study after sex: %s", nrow(controlAnimals)))


# Extract all MI findings for the control animals
allMI<-unique(ExtractSubjData("MI", controlAnimals))

# Filter MI findings - save Dosing phase findings (and include rows where phase cannot be identified )
dosingMI<-FilterFindingsPhase('MI', allMI, pStudyPhase, pStudyPhaseInclUncertain)
print(sprintf("Number of MI findings after dosing: %s", nrow(dosingMI)))

# Add animal age of finding column
ageMI<-addFindingsAnimalAge('mi', dosingMI, inclUncertainMsg=FALSE)
# Filter MI findings for a specific age interval
finalFindings<-filterFindingsAnimalAge(ageMI, pFindingsFromAge, pFindingsToAge)
print(sprintf("Number of MI findings after dosing: %s", nrow(finalFindings)))


finalFindings$MISTRESC <- toupper(finalFindings$MISTRESC)

# count the number of findings per animal, per organ
# only keep interesting finings, ie., remove 
# NORMAL, UNREMARKABLE, or blank
findingsCount <- finalFindings %>%
                    dplyr::distinct(STUDYID, USUBJID, MISPEC, MISTRESC) %>% # only one organ, finding per animal (input errors cause duplications)
                    dplyr::count(MISPEC, MISTRESC) %>%
                    dplyr::arrange(-n) %>%
                    dplyr::filter(MISTRESC != 'NORMAL', MISTRESC != 'UNREMARKABLE', MISTRESC != '')

findingsCount$Incidence <- (findingsCount$n / length(unique(finalFindings$USUBJID))) * 100

findingsCount$Incidence <- paste0(round(findingsCount$Incidence, 2), '%')

findingsCount <- dplyr::select(findingsCount, -n)

write.csv(findingsCount, 'data/RankedMIFindings.csv')


##########################################################################

message("Execution time: ",round(difftime(Sys.time(), start, units = 'min'),1)," minutes")

