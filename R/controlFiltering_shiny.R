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


################################################################################
# Execute extraction of animals based on the  user specified  filter conditions.
# The final extracted set of animals are returned.
#
# For each of the filtering functions
#   - If a set of condition(s) is specified, the function is executed with the
#     condition(s)
#   - else the function is execute with no conditions to add the relevant
#     variables to the set of extracted animals
#
# First time the function is called in a session, all the extraction functions
# are executed
# The set of given conditions is saved internally (in the sendigR specific
# environment '.sendigRenv').
# And sets of data are saved in the same environment:
#   - studiesAll        - all the extracted studies based on the specified filter
#                         conditions
#   - controlAnimalsAll - all the control animals extracted for the extracted
#                         studies (before the conditional filtering)
#   - controlAnimals    - the set of control animals from the execution of
#                         the filtering extraction functions - i.e. the set of
#                         data which is returned
#
# In the subsequent execution of the function, only the necessary extraction
# functions are executed - i.e. extractions/filterings which need to be
# re-executed due to changes in the user specified conditions.
#
# If the given set of conditions is equal to the conditions for last execution
# of function, none of the extraction functions are executed and the last final
# set of extracted animals is returned.
#
# If any of the study level conditions (study dates, design) are changed, all
# the extraction functions are executed, like an initial execution of the
# function.
#
# If any of the animal level filter conditions (sex, route, species/strain) are
# changed, only the animal level extraction functions are executed, using the
# saved data table controlAnimalsAll as the initial input.
#
# In the last two scenarios, the set of given conditions and any new extracted
# set of data are saved as described above.
################################################################################
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
    # Check for refresh of output data
    # - first check if any differences since last execution
    if ((length(setdiff(thisFilterValues, .sendigRenv$lastFilterValues)) +
        length(setdiff(.sendigRenv$lastFilterValues, thisFilterValues))) == 0)  {
      message('No changes...')
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
    # print('get study info')
    if (exists('studiesAll', envir = .sendigRenv)) {
      # We save the current set of selected studies...
      studiesAllPrev <- .sendigRenv$studiesAll[,c('STUDYID')]
    }

    # print(paste0(' - pStudyDesign: ',pStudyDesign))
    # print(paste0(' - pfromDTC: ',pFromDTC))
    # print(paste0(' - ptoDTC: ',pToDTC))

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

    # print(.sendigRenv$studiesAll)

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
    # print('GetControlAnimals')
    # Get list of all control animals
    # - save as global table to be possible avoid unnecessary regeneration in a later execution
    assign('controlAnimalsAll',
           sendigR::getControlSubj(.sendigRenv$dbToken,
                                   studyList     = .sendigRenv$studiesAll,
                                   inclUncertain = pInclUncertain),
           envir = .sendigRenv)

  }
#   print(.sendigRenv$controlAnimalsAll)

  if (execFilterControlAnimals) {
    # Copy to a table used as input/output in the animal filtering tables
    assign('controlAnimals',
           data.table::copy(.sendigRenv$controlAnimalsAll),
           envir = .sendigRenv)

    # print('filterAnimalsSex')
    if (pSex != 'All') {
    #   print(paste0(' - pSex: ',pSex))

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

    }
    # print(.sendigRenv$controlAnimals)

    # print('FilterAnimalsSpeciesStrain')
    if (!is.null(pSpecies)) {
    #   print(paste0(' - pSpecies: ',pSpecies))
    #   print(paste0(' - pStrain: ',pStrain))

      # Limit to set of animals to relevant species/strain(s)
      assign('controlAnimals',
             sendigR::getSubjSpeciesStrain(.sendigRenv$dbToken,
                                           animalList    = .sendigRenv$controlAnimals,
                                           speciesFilter = pSpecies,
                                           strainFilter = pStrain,
                                           inclUncertain = pInclUncertain),
             envir = .sendigRenv)
    }else {
      assign('controlAnimals',
             sendigR::getSubjSpeciesStrain(.sendigRenv$dbToken,
                                           animalList    = .sendigRenv$controlAnimals,
                                           inclUncertain = pInclUncertain),
             envir = .sendigRenv)

    }
    # print(.sendigRenv$controlAnimals)

    # print('FilterAnimalListRoute')
    if (!is.null(pRoute)) {
    #   print(paste0(' - pRoute: ',pRoute))

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
    # print(.sendigRenv$controlAnimals)

    message('Animal filtering done!')

  }

  return(.sendigRenv$controlAnimals)
}


