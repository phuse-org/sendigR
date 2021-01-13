# 
# This module is just a wrapper 
# for calling all the functions 
# available in the SEND package

doFilterAnimalsSpeciesStrain <- function(controlAnimals, 
                                         pSpecies, 
                                         pStrain, 
                                         pInclUncertain) {
  
  execFilter <- function(species) {
    # Extract list of select strains for current species 
    # - remove prefixed species value
    strain <- str_replace(pStrain[str_detect(pStrain, paste0(species,': '))], 
                          paste0(species,': '),'')
    if (length(strain) == 0) strain <- NULL
    
    # Execute species/strain filtering for current species/strain(s)
    return(
      FilterAnimalsSpeciesStrain(animalList    = controlAnimals, 
                                 speciesFilter = species, 
                                 strainFilter  = strain, 
                                 inclUncertain = pInclUncertain))
  }
  
  if (length(pSpecies) == 1) 
    # One species selected - just execute  the filtering of the species/strain
    # and return result
    return(
      FilterAnimalsSpeciesStrain(animalList    = controlAnimals, 
                                 speciesFilter = pSpecies, 
                                 strainFilter  = pStrain, 
                                 inclUncertain = pInclUncertain))
  else 
    # Multiple species selected - execute filtering for species/strain per species
    # - combine all outputs into one table and return
    return(rbindlist(lapply(pSpecies, function(species) {execFilter(species)}), 
                     use.names=TRUE, fill=TRUE))
}


GetFilteredControlAnimals <- function(pFromDTC, 
                                      pToDTC, 
                                      pStudyDesign, 
                                      pRoute, 
                                      pSpecies, 
                                      pStrain, 
                                      pSex, 
                                      pInclUncertain) {
  
  # compare two character parameter values - each of them may be vectors of values
  isEqualParams <- function(p1, p2) {
    l1 <- length(p1)
    l2 <- length(p2)
    if (l1 != l2)
      return(FALSE)
    else 
      return (isTRUE(unique(p1 == p2))) 
  }
  
  # Save current filter values
  # - Convert eventual NULL values to ""
  thisFilterValues <- lapply(list(
    pFromDTC  = pFromDTC,
    pToDTC  = pToDTC,
    pStudyDesign  = pStudyDesign,
    pRoute  = pRoute,
    pSpecies  = pSpecies,
    pStrain  = pStrain,
    pSex  = pSex,
    pInclUncertain = pInclUncertain
  ), function(p) {if (is.null(p)) '' else p})
  
  if (!exists("lastFilterValues")) {
    # First time the results data are generated
    execGetStudyData <- TRUE
    execGetControlAnimals <- TRUE
    execFilterControlAnimals <- TRUE
  }
  else {
    prevLastFilterValues <<- lastFilterValues
    # Check for refresh of output data 
    # - first check if any differences since last execution
    if ((length(setdiff(thisFilterValues, lastFilterValues)) +
        length(setdiff(lastFilterValues, thisFilterValues))) == 0)  {
      print('No changes...')
      # No changes in filter values - leave output data as is
      execGetStudyData <- FALSE
      execGetControlAnimals <- FALSE
      execFilterControlAnimals <- FALSE
    }
    else {
      # Changes in filter values - check specific filter values
      # to decide how much to be re-executed
      if (lastFilterValues$pInclUncertain != thisFilterValues$pInclUncertain) {
        # Uncertaity has ben en- or disavble - refresh all levels of output data
        execGetStudyData <- TRUE
        execGetControlAnimals <- TRUE
        execFilterControlAnimals <- TRUE
      }
      # Study level only filter parameters
      else {
        if (lastFilterValues$pFromDTC != thisFilterValues$pFromDTC
          | lastFilterValues$pToDTC != thisFilterValues$pToDTC
          | lastFilterValues$pStudyDesign != thisFilterValues$pStudyDesign) {
          execGetStudyData <- TRUE
          # Decide later whether it's needed to extract list of all control animals 
          # and do filtering
          execGetControlAnimals <- as.logical(NA)
          execFilterControlAnimals <- as.logical(NA)
        }
        else {
          # No changes in in the filtering at study level
          # - then it isn't necessary to refresh list of control animals either
          execGetStudyData <- FALSE
          execGetControlAnimals <- FALSE
        }
        # Check changes in animal(/study/set) level filter parameters
        if (!isEqualParams(lastFilterValues$pRoute,thisFilterValues$pRoute)
            | !isEqualParams(lastFilterValues$pSpecies, thisFilterValues$pSpecies)
            | !isEqualParams(lastFilterValues$pStrain, thisFilterValues$pStrain)
            | !isEqualParams(lastFilterValues$pSex, thisFilterValues$pSex)) {
          execFilterControlAnimals <- TRUE
        }
      }
    }
  }

  # - Save the current value for each filter value to be used in next refresh
  # of data
  lastFilterValues <<- thisFilterValues
 
  ####################################################################
  
  if (execGetStudyData) {
    print('get study info')
    if (exists("studiesAll"))
      # We save the current set of selected studies...
      studiesAllPrev <- studiesAll[,.(STUDYID)]
    
    # Extract list of studies based on study-only filter parameters
    studiesAll <<- GetStudyListSDESIGN(studyDesignFilter = pStudyDesign, 
                                       studyList         = GetStudyListSTSTDTC(fromDTC       = pFromDTC, 
                                                                               toDTC         = pToDTC, 
                                                                               inclUncertain = pInclUncertain), 
                                       inclUncertain     = pInclUncertain)
    
    if (exists("studiesAllPrev"))
      if (nrow(setdiff(studiesAll[,.(STUDYID)], studiesAllPrev)) > 0) {
        # The new set of filtered studies contains studies not included 
        # in previous set - delete all domain data fetched from database 
        # for the previous set of studies except the TS domain
        domainTabs<-intersect(ls(.GlobalEnv), all_domains)
        rm(list=domainTabs[!domainTabs == 'TS'], envir = .GlobalEnv)
        
        
        # And we need also to refresh list of ContolAnimals 
        # - and do the full filtering 
        execGetControlAnimals <- TRUE
        execFilterControlAnimals <- TRUE
      }
        
    if (is.na(execGetControlAnimals)) {
      # Check if the new set of studies contans fewer studies than the previous set
      if (nrow(setdiff(studiesAllPrev, studiesAll[,.(STUDYID)])) > 0) {
        # The new set of filtered studies contains fewer studies than
        # in previous set - we must refresh list of ControlAnimals 
        # - and do the full filtering 
        execGetControlAnimals <- TRUE
        execFilterControlAnimals <- TRUE
      }
      else {
        execGetControlAnimals <- FALSE
        execFilterControlAnimals <- FALSE
      }
    }
  }
  
  if (execGetControlAnimals) {
    print('GetControlAnimals')
    # Get list of all control animals 
    # - save as global table and to be possible avoid unnecessary regeneration in a later execution 
    controlAnimalsAll <<- GetControlAnimals(studyList     = studiesAll, 
                                            inclUncertain = pInclUncertain)
  }
  
  if (execFilterControlAnimals) {
    # Copy to a table used as input/output in the animal filtering tables
    controlAnimals <<- copy(controlAnimalsAll)
    
    # If EX exists in workspace - delete it, to ensure correct set output 
    # animals from the filtering process
    if (exists("EX")) rm(EX)
    
    print('filterAnimalsSex')
    if (pSex != '')
      # Limit to set of animals to relevant sex
      controlAnimals <<- filterAnimalsSex(animalList    = controlAnimals, 
                                          sexFilter     = pSex, 
                                          inclUncertain = pInclUncertain)    
    print('FilterAnimalsSpeciesStrain')
    if (!is.null(pSpecies))
      # Limit to set of animals to relevant species/strain(s)
      controlAnimals <<- doFilterAnimalsSpeciesStrain(controlAnimals, 
                                                      pSpecies, 
                                                      pStrain, 
                                                      pInclUncertain)
    
    print('FilterAnimalListRoute')
    if (!is.null(pRoute))
      # Limit to set of animals to relevant route(s) of administration
      controlAnimals<<-FilterAnimalListRoute(animalList    = controlAnimals, 
                                             routeFilter   = pRoute, 
                                             inclUncertain = pInclUncertain)
    print('Animal filtering done!')

  }

  return(controlAnimals)
}

