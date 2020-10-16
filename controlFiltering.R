# 
# This module is just a wrapper 
# for calling all the functions 
# available in controlFunctions.R

GetFilteredControlAnimals <- function(pFromDTC, 
                                      pToDTC, 
                                      pStudyDesign, 
                                      pRoute, 
                                      pSpecies, 
                                      pStrain, 
                                      pSex, 
                                      pInclUncertain) {

  ### Ensure all is executed for now...work to be continued...
  execGetStudyData <- TRUE
  execGetControlAnimals <- TRUE
  execFilterControlAnimals <- TRUE
  ####################################################################
  
  if (execGetStudyData) {
    # Extract list of studies based on study-only filter parameters
    studiesAll <- GetStudyListSDESIGN(studyDesignFilter = pStudyDesign, 
                                      studyList         = GetStudyListSTSTDTC(fromDTC       = pFromDTC, 
                                                                              toDTC         = pToDTC, 
                                                                              inclUncertain = pInclUncertain), 
                                      inclUncertain     = pInclUncertain)
      ## TO DO: delete domain tables from workspace if the existing list of studies are changed....
  }
  
  if (execGetControlAnimals)
    # Get list of all control animals
    controlAnimals<-GetControlAnimals(studyList     = studiesAll, 
                                      inclUncertain = pInclUncertain)
  
  if (execGetControlAnimals) {
    if (!is.null(pRoute))
      # Limit to set of animals to relevant route(s) of administration
      controlAnimals<-FilterAnimalListRoute(animalList    = controlAnimals, 
                                            routeFilter   = pRoute, 
                                            inclUncertain = pInclUncertain)
    
    if (!is.null(pSpecies))
      # Limit to set of animals to relevant species/strain(s)
      controlAnimals<-FilterAnimalsSpeciesStrain(animalList    = controlAnimals, 
                                                 speciesFilter = pSpecies, 
                                                 strainFilter  = pStrain, 
                                                 inclUncertain = pInclUncertain)
    
    if (pSex != '')# Limit to set of animals to relevant sex
      #TODO: except error length of dimnames
      controlAnimals<-filterAnimalsSex(animalList    = controlAnimals, 
                                       sexFilter     = pSex, 
                                       inclUncertain = pInclUncertain)
  }

  return(controlAnimals)
}

