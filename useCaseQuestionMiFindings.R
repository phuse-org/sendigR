###################################################################################
# Script name   : useCaseQuestionMiFindings.R
# Date Created  : 23-Jul-2020
# Programmer    : BoLr
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Execute use case described in the article 
#                 'LEVERAGING THE VALUE OF CDISC SEND DATASETS FOR CROSS-STUDY 
#                 ANALYSIS: INCIDENCE OF MICROSCOPIC FINDINGS IN CONTROL ANIMALS' 
#
# Description   : <detailed description of the algorithm(s) implemented in the script>
#
# Input         : A database or another kind of data store containing SEND data
#                 for set number of studies
#
# Output        : A data table with the extracted set of MI findings - subsetFindings
#
# Usage notes   : Before execution, ensure that:
#                 - these R packages are installed:
#                     data.table
#                     parsedate
#                     stringr
#                     stringi
#                     varhandle
#                     readxl
#                     tools
#                     The package relevant for the actual database type 
#                     (eg.RSQLite or ROracle )
#                 - A set SEND study data is available in data store.
#                 - Download the newest CDISC CT file with SEND terminology (from
#                   CDISC or NCI)
#                   
###################################################################################

start<-Sys.time()

###################################################################################
# Input parameter values
pStudyDesign      <- "PARALLEL" 
pFromDTC          <- "2017"
pToDTC            <- "2020"
pRoute            <- c("ORAL", "ORAL GAVAGE")
pSpecies          <- "DOG"
pStrain           <- "BEAGLE"
pSex              <- "M"
pStudyPhase       <- "Treatment"
pFindingsFromAge  <- "12m"
pFindingsToAge    <- "18m"
pFindingsDomain   <- "MI"

# Parameters to control behavior the filtering functions
pInclUncertain    <-  TRUE
pExclusively      <-  FALSE
pMatchAll         <-  FALSE

# database type - valid types are ('sqlite', 'oracle')
pDbType           <- 'sqlite'
###################################################################################

# define the path to R scripts to actual script location
dummyuseCaseQuestionMiFindings<-function() {
    # dummy function only to be used to get this script's location
}
setwd(getSrcDirectory(dummyuseCaseQuestionMiFindings))

# initiate setup and connect to db
source("initSENDFunctions.R")
if (pDbType == 'sqlite') {
  initEnvironment(dbType='sqlite', 
                  dbPath='<full path to the SQLite dn file>', 
                  ctFile='<full path to a CISC CT Excel (xlcs) file downloaded from either NCI or CDISC>')
} else {
  initEnvironment(dbType='oracle', 
                  dbPath='<Oracle db name>', 
                  dbUser='<username>', dbPwd='password', 
                  dbSchema = '<name of table owner if relevant>',
                  ctFile='<full path to a CISC CT Excel (xlcs) file downloaded from either NCI or CDISC>')
}

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

# Extract all findings from actual domain for the control animals
allFindings<-ExtractSubjData(domain     = pFindingsDomain, 
                             animalList = controlAnimals)

# Add animal age of finding to the extracted findings
allFindings<-addFindingsAnimalAge(domain        = pFindingsDomain, 
                                  findings      = allFindings, 
                                  inclUncertain = pInclUncertain)

# Filter extracted findings for relevant phase
subsetFindings<-FilterFindingsPhase(domain        = pFindingsDomain, 
                                    findings      = allFindings, 
                                    phaseFilter   = pStudyPhase,  
                                    inclUncertain = pInclUncertain)

# Filter extracted findings for a specific age interval
subsetFindings<-filterFindingsAnimalAge(findings      = subsetFindings, 
                                        fromAge       = pFindingsFromAge, 
                                        toAge         = pFindingsToAge,  
                                        inclUncertain = pInclUncertain)

##########################################################################
message("Execution time: ",round(difftime(Sys.time(), start, units = 'min'),1)," minutes")

