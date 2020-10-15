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
  ### UNDER CONSTRUCTION ######
  if (!exists("lastFilterValues")) {
    # First time the results data are generated
    execGetStudyData <- TRUE
    execGetControlAnimals <- TRUE
    execFilterControlAnimals <- TRUE
  }
  else {
    # Refresh of output data - check changed filter values
    # to decide how much to be re-executed
    if (lastFilterValues['pFromDTC'] != pFromDTC)
    # Study level only filter parameters
    if (lastFilterValues['pFromDTC'] != pFromDTC
      | lastFilterValues['pToDTC'] != pToDTC
      | lastFilterValues['pStudyDesign'] != pStudyDesign) {
      execGetStudyData <- TRUE
      # Decide later whether it's needed to extraxct list of all control animals
      execGetControlAnimals <- as.logical(NA)
    }
    else {
      execGetStudyData <- FALSE
      execGetControlAnimals <- FALSE
    }
    # Animal(/study/set) level filter parameters
    if (lastFilterValues['pRoute'] != pRoute
        | lastFilterValues['pSpecies'] != pSpecies
        | lastFilterValues['pStrain'] != pStrain
        | lastFilterValues['pSex'] != pSex) {
      execFilterControlAnimals <- TRUE
    }
  }
  
  # - Save the current value for each filter value to be used in next refresh 
  # of data
  lastFilterValues <<- list(
    pFromDTC  = pFromDTC,            
    pToDTC  = pToDTC,
    pStudyDesign  = pStudyDesign,
    pRoute  = pRoute, 
    pSpecies  = pSpecies,
    pStrain  = pStrain, 
    pSex  = pSex,
    pInclUncertain  = pInclUncertain
  )
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

