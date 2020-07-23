###################################################################################
# Script name   : initSENDFunctions.R
# Date Created  : 23-Jul-2020
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Initiate the set of SENd functions
#
# Description   : Check if the parameter file sysParameters.R exists and contains 
#                 the minimum set of required parameters
#                 Check if a CDISC CT files exists
#                 Compile all functions
#                   
###################################################################################

# define the path to R scripts to actual script location
dummyuseCaseQuestionMiFindings<-function() {
  # dummy function only to be used to get this script's location
}
setwd(getSrcDirectory(dummyuseCaseQuestionMiFindings))

# Check existence of parameter file
if (!file.exists('sysParameters.R'))
  stop("The parameter file sysParameters.R is missing")

source("sysParameters.R")

# Check if location of CDISC CT file is define and if at least one CT filexists.
if (!exists('metadataRoot'))
  stop('A variable named metadataRoot must be defined an point to location for CDISC CT file(s) in XLS format')

if (!dir.exists(metadataRoot))
  stop(paste('The folder defined in variable metadataRoot does not exist: ', metadataRoot, sep=''))

ctFiles<-list.files(metadataRoot, pattern="SEND[_ ]Terminology.*.\\.xls", ignore.case = TRUE, full.names = TRUE)
if (length(ctFiles) == 0)
  stop(paste('No CDISC CT files exists in XLS format in folder ', metadataRoot, sep=''))

print(paste("Using CDISC CT file: ", max(ctFiles), sep=''))

# Compiling all functions
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
