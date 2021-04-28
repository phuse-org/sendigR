################################################################################
## A set of internal functions to handle the extraction and filtering of control
## subjects for the Shiny app
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2021-04-28   Yousuf Ali,           Initial version
##              Daniel Russo
##              Bo Larsen
################################################################################

doFilterAnimalsSpeciesStrain <- function(pControlAnimals,
                                         pSpecies,
                                         pStrain,
                                         pInclUncertain) {

  execFilter <- function(species) {
    # Extract list of select strains for current species
    # - remove prefixed species value
    strain <- stringr::str_replace(pStrain[stringr::str_detect(pStrain, paste0(species,': '))],
                          paste0(species,': '),'')
    if (length(strain) == 0) strain <- NULL

    # Execute species/strain filtering for current species/strain(s)
    return(
      sendigR::getSubjSpeciesStrain(.sendigRenv$dbToken,
                                    animalList    = pControlAnimals,
                                    speciesFilter = pSpecies,
                                    strainFilter  = pStrain,
                                    inclUncertain = pInclUncertain))
  }

  if (length(pSpecies) == 1)
    # One species selected - just execute  the filtering of the species/strain
    # and return result
    return(
      sendigR::getSubjSpeciesStrain(.sendigRenv$dbToken,
                                    animalList    = pControlAnimals,
                                    speciesFilter = pSpecies,
                                    strainFilter  = pStrain,
                                    inclUncertain = pInclUncertain))
  else
    # Multiple species selected - execute filtering for species/strain per species
    # - combine all outputs into one table and return
    return(data.table::rbindlist(lapply(pSpecies,
                                        function(species) {execFilter(species)}),
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

  if (!exists('lastFilterValues', envir = .sendigRenv)) {
    # First time the results data are generated
    execGetStudyData <- TRUE
    execGetControlAnimals <- TRUE
    execFilterControlAnimals <- TRUE
  }
  else {
    #prevLastFilterValues <<- lastFilterValues
    # Check for refresh of output data
    # - first check if any differences since last execution
    if ((length(setdiff(thisFilterValues, .sendigRenv$lastFilterValues)) +
        length(setdiff(.sendigRenv$lastFilterValues, thisFilterValues))) == 0)  {
      print('No changes...')
      # No changes in filter values - leave output data as is
      execGetStudyData <- FALSE
      execGetControlAnimals <- FALSE
      execFilterControlAnimals <- FALSE
    }
    else {
      # Changes in filter values - check specific filter values
      # to decide how much to be re-executed
      if (.sendigRenv$lastFilterValues$pInclUncertain != thisFilterValues$pInclUncertain) {
        # Uncertainty has been en- or disable - refresh all levels of output data
        execGetStudyData <- TRUE
        execGetControlAnimals <- TRUE
        execFilterControlAnimals <- TRUE
      }
      # Study level only filter parameters
      else {
        if (.sendigRenv$lastFilterValues$pFromDTC != thisFilterValues$pFromDTC
          | .sendigRenv$lastFilterValues$pToDTC != thisFilterValues$pToDTC
          | .sendigRenv$lastFilterValues$pStudyDesign != thisFilterValues$pStudyDesign) {
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
        if (!isEqualParams(.sendigRenv$lastFilterValues$pRoute,thisFilterValues$pRoute)
            | !isEqualParams(.sendigRenv$lastFilterValues$pSpecies, thisFilterValues$pSpecies)
            | !isEqualParams(.sendigRenv$lastFilterValues$pStrain, thisFilterValues$pStrain)
            | !isEqualParams(.sendigRenv$lastFilterValues$pSex, thisFilterValues$pSex)) {
          execFilterControlAnimals <- TRUE
        }
      }
    }
  }

  # - Save the current value for each filter value to be used in next refresh
  # of data
  assign('lastFilterValues', thisFilterValues, envir = .sendigRenv)

  ####################################################################

  if (execGetStudyData) {
    print('get study info')
    if (exists('studiesAll', envir = .sendigRenv)) {
      # We save the current set of selected studies...
      studiesAllPrev <- .sendigRenv$studiesAll[,c('STUDYID')]
    }

    # Extract list of studies based on study-only filter parameters
    #  - save in a retained variable
    assign('studiesAll',
           sendigR::getStudiesSDESIGN(.sendigRenv$dbToken,
                                      studyDesignFilter
                                                = pStudyDesign,
                                      studyList =
                     sendigR::getStudiesSTSTDTC(.sendigRenv$dbToken,
                                                fromDTC = pFromDTC,
                                                toDTC   = pToDTC,
                                                inclUncertain
                                                        = pInclUncertain),
                                      inclUncertain
                                                = pInclUncertain),
           envir = .sendigRenv)

    print(.sendigRenv$studiesAll)

    if (exists("studiesAllPrev")) {
      if (nrow(setdiff(.sendigRenv$studiesAll[,c('STUDYID')], studiesAllPrev)) > 0) {
        # The new set of filtered studies contains studies not included
        #  - refresh list of ContolAnimals
        #  - do the full filtering
        execGetControlAnimals <- TRUE
        execFilterControlAnimals <- TRUE
      }
    }
    if (is.na(execGetControlAnimals)) {
      # Check if the new set of studies contans fewer studies than the previous set
      if (nrow(setdiff(studiesAllPrev, .sendigRenv$studiesAll[,c('STUDYID')])) > 0) {
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
    # - save as global table to be possible avoid unnecessary regeneration in a later execution
    assign('controlAnimalsAll',
           sendigR::getControlSubj(.sendigRenv$dbToken,
                                   studyList     = .sendigRenv$studiesAll,
                                   inclUncertain = pInclUncertain),
           envir = .sendigRenv)

  }
  #print(.sendigRenv$controlAnimalsAll)
  if (execFilterControlAnimals) {
    # Copy to a table used as input/output in the animal filtering tables
    assign('controlAnimals',
           data.table::copy(.sendigRenv$controlAnimalsAll),
           envir = .sendigRenv)

    print('filterAnimalsSex')
    print(pSex)
    if (pSex != 'All') {
      # Limit to set of animals to relevant sex

      assign('controlAnimals',
             sendigR::getSubjSex(.sendigRenv$dbToken,
                                 animalList    = .sendigRenv$controlAnimals,
                                 sexFilter     = pSex,
                                 inclUncertain = pInclUncertain),
             envir = .sendigRenv)

    } else {
      assign('controlAnimals',
             sendigR::getSubjSex(.sendigRenv$dbToken,
                                 animalList    = .sendigRenv$controlAnimals,
                                 inclUncertain = pInclUncertain),
             envir = .sendigRenv)

      print(.sendigRenv$controlAnimals)
    }
    print('FilterAnimalsSpeciesStrain')
    if (!is.null(pSpecies)) {
      # Limit to set of animals to relevant species/strain(s)
      assign('controlAnimals',
             doFilterAnimalsSpeciesStrain(.sendigRenv$controlAnimals,
                                          pSpecies,
                                          pStrain,
                                          pInclUncertain),
             envir = .sendigRenv)
    }
    print('FilterAnimalListRoute')
    if (!is.null(pRoute)) {
      # Limit to set of animals to relevant route(s) of administration
      assign('controlAnimals',
             sendigR::getSubjRoute(.sendigRenv$dbToken,
                                   animalList    = .sendigRenv$controlAnimals,
                                   routeFilter   = pRoute,
                                   inclUncertain = pInclUncertain),
             envir = .sendigRenv)
    } else {
      assign('controlAnimals',
             sendigR::getSubjRoute(.sendigRenv$dbToken,
                                   animalList  = .sendigRenv$controlAnimals,
                                   inclUncertain = pInclUncertain),
             envir = .sendigRenv)
    }
    print('Animal filtering done!')

  }
  #print(.sendigRenv$controlAnimals)

  return(.sendigRenv$controlAnimals)
}



################################################################################
# Avoid  'no visible binding for global variable' notes from check of package:
#lastFilterValues  <- controlAnimalsAll <- controlAnimals <- NULL
.sendigRenv <- NULL
