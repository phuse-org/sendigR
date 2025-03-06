################################################################################
## The function getSubjSpeciesStrain.
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2020-12-23   Bo Larsen             Initial version
################################################################################

#' Extract the set of animals of the specified species and strain - or just add
#' the species and strain for each animal.
#'
#' Returns a data table with the set of animals included in the
#' \code{animalList} matching the species and strain specified in the
#' \code{speciesFilter} and \code{strainFilter}.\cr
#' If the \code{speciesFilter} and \code{strainFilter} are empty (null, na or
#' empty string) - all rows from \code{animalList} are returned with additional
#' populated SPECIES and STRAIN columns.
#'
#' The species and strain per animal respectively are identified by a
#' hierarchical lookup in these domains
#' \itemize{
#'   \item DM - If the DM.SPECIES (DM.STRAIN) isn't empty, this value is
#'   included in the output.\cr
#'   \item TX - if a TX parameter 'SPECIES' ('STRAIN') exists for the group
#'   related to the animal, the TXVAL value for this is included in the
#'   output.\cr
#'   \item TS - if a TS parameter 'SPECIES' ('STRAIN') exists, this is included
#'   in the output.\cr
#' }
#' The comparisons of species/strain values is done case insensitive and trimmed
#' for leading/trailing blanks.
#'
#' If input parameter \code{inclUncertain=TRUE}, uncertain animals are included
#' in the output set. These uncertain situations are identified and reported for
#' SPECIES and STRAIN respectively (in column UNCERTAIN_MSG):
#' \itemize{
#'   \item TS parameter SPECIES/STRAIN is missing or invalid (not CT value -
#'   CDISC SEND code list SPECIES/STRAIN) and TX parameter SPECIES/STRAIN is
#'   missing or invalid (not CT value) and DM.SPECIES/STRAIN is missing or
#'   invalid (not CT value)
#'   \item Different values of SPECIES/STRAIN across TS, TX and DM for studies
#'   where no or only one TS parameter SPECIES/STRAIN is registered
#'   \item Multiple TS parameter SPECIES/STRAIN values are registered for study
#'   and TX parameter SPECIES/STRAIN and/or DM.SPECIES/STRAIN do not match any
#'   of the TS values.
#'   \item  Multiple TS parameter SPECIES/STRAIN values are registered for study
#'   and TX parameter SPECIES/STRAIN and DM.SPECIES/STRAIN are unequal.
#' }
#' The same checks are performed and reported in column NOT_VALID_MSG if
#' \code{speciesFilter} and \code{strainFilter} are empty and
#' \code{noFilterReportUncertain=TRUE}.
#'
#' @param dbToken Mandatory\cr
#'   Token for the open database connection (see \code{\link{initEnvironment}}).
#' @param animalList  Mandatory, data.table.\cr
#'  A table with the list of animals to process.\cr
#'  The table must include at least columns named 'STUDYID' and 'USUBJID'.
#' @param speciesFilter  Optional, character.\cr
#'  The species value(s) to use as criterion for filtering of the input data
#'  table.\cr
#'  It can be a single string, a vector or a list of multiple strings.
#' @param strainFilter  Optional, character.\cr
#'  The strain value(s) to use as criterion for filtering of the input data
#'  table.\cr
#'  It is only valid to specify value(s) if one or more values have been
#'  specified for parameter \code{speciesFilter}\cr
#'  It can be a single string, a vector or a list of multiple strings.
#'  When multiple values are specified for \code{speciesFilter}, each strain
#'  value must be prefixed by species and ':' , e.g.
#'  \code{c('RAT:WISTAR','DOG: BEAGLE')}.\cr
#'  There may be included any number of blanks after ':'
#' @param inclUncertain  Mandatory, boolean.\cr
#'  Indicates whether animals for which the species or strain cannot be
#'  confidently identified shall be included or not in the output data table.
#' @param exclusively Mandatory, boolean.
#'   \itemize{
#'   \item TRUE: Include animals only for studies with no other species and
#'   optional strains then included in \code{speciesFilter} and
#'   \code{strainFilter}
#'   \item FALSE: Include animals for all studies with species and strain
#'   matching \code{speciesFilter} and \code{strainFilter} respectively.
#' }
#' @param noFilterReportUncertain  Optional, boolean.\cr
#'  Only relevant if the \code{speciesFilter} and  \code{strainFilter} are
#'  empty.\cr
#'  Indicates if the reason should be included if the species or strain cannot
#'  be confidently decided for an animal.
#'
#' @return The function returns a data.table with columns:
#'   \itemize{
#'   \item STUDYID       (character)
#'   \item Additional columns contained in the \code{animalList} table
#'   \item SPECIES       (character)
#' The value is always returned in uppercase and trimmed for leading/trailing
#' blanks.
#'   \item STRAIN        (character)
#' The value is always returned in uppercase and trimmed for leading/trailing
#' blanks.
#'   \item UNCERTAIN_MSG (character)\cr
#' Included when parameter \code{inclUncertain=TRUE}.\cr
#' In case the species or strain cannot be confidently matched during the
#' filtering of data, the column contains an indication of the reason.\cr
#' Is NA for rows where species and strain can be confidently matched.\cr
#' A non-empty UNCERTAIN_MSG value generated by this function is merged with
#' non-empty UNCERTAIN_MSG values which may exist in the input set of animals
#' specified in \code{animalList} - separated by '|'.
#'   \item NOT_VALID_MSG (character)\cr
#' Included when parameter \code{noFilterReportUncertain=TRUE}.\cr
#' In case the species or strain cannot be confidently decided, the column
#' contains an indication of the reason.\cr
#' Is NA for rows where species and strain can be confidently decided.\cr
#' A non-empty NOT_VALID_MSG value generated by this function is merged with
#' non-empty NOT_VALID_MSG values which may exist in the input set of animals
#' \code{animalList} - separated by '|'.
#'}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract rats and mice plus uncertain animals
#' getSubjSpeciesStrain(dbToken, controlAnimals,
#'                      speciesFilter = c('RAT', 'MOUSE'),
#'                      inclUncertain = TRUE)
#' # Extract Spargue-Dawley rats plus uncertain animals.
#' # Include only animals from studies which do not contain other species or
#' # strains
#' getSubjSpeciesStrain(dbToken, controlAnimals,
#'                      speciesFilter = 'RAT',
#'                      strainFilter = 'SPRAGUE-DAWLEY',
#'                      inclUncertain = TRUE,
#'                      exclusively = TRUE,
#'                      noFilterReportUncertain = TRUE)
#' # Extract Wistar rats and and Beagle dogs - and no uncertain animals
#' getSubjSpeciesStrain(dbToken, controlAnimals,
#'                      speciesFilter = c('RAT', 'DOG'),
#'                      strainFilter = c('RAT: WISTAR', 'DOG: BEAGLE'))
#' # No filtering, just add SPECIES and STRAIN - do not include messages when
#' # these values cannot be confidently found
#' getSubjSpeciesStrain(dbToken, controlAnimals,
#'                      noFilterReportUncertain = FALSE)
#' }
getSubjSpeciesStrain <- function(dbToken,
                                 animalList,
                                 speciesFilter = NULL,
                                 strainFilter = NULL,
                                 inclUncertain = FALSE,
                                 exclusively = FALSE,
                                 noFilterReportUncertain = TRUE) {


  ##  Evaluate input parameters
  if (!data.table::is.data.table(animalList)) {
    stop("animalList must be be specified with a data table")
  }

  if (is.null(speciesFilter) | isTRUE(is.na(speciesFilter)) | isTRUE(speciesFilter=="")) {
    if (!(is.null(strainFilter) | isTRUE(is.na(strainFilter)) | isTRUE(strainFilter=="")))
      stop('Parameter strainFilter must not be specified when no speciesFilter has been specified')
    else
      execFilter <- FALSE
  } else {
    execFilter <- TRUE
    # Trim all filter conditions and convert to uppercase
    speciesFilter <- toupper(trimws(speciesFilter))
    if (!(is.null(strainFilter) | isTRUE(is.na(strainFilter)) | isTRUE(strainFilter=="")))
    {
      strainFilter <- toupper(trimws(strainFilter))
      inclStrainFilter <- TRUE
    }
    else
      inclStrainFilter <- FALSE
  }
  if (execFilter & !(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter inclUncertain must be either TRUE or FALSE")
  }
  if (execFilter & !(exclusively %in% c(TRUE,FALSE))) {
    stop("Parameter Exclusively must be either TRUE or FALSE")
  }
  if (!execFilter & !(noFilterReportUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter noFilterReportUncertain must be either TRUE or FALSE")
  }

  # Extract set of all animals for list of studyid values
  # included in the input table of animals
  # Join species and strains values at trial level (TS), set level (TX) and
  # animal level (DM) together for each animal
  #  - ensure all empty SPECIES_xx and STRAIN_xx values are NA
  # Trim all species and strain values and convert to uppercase
  if(dbToken$dbType=='sqlite'){
  animalSpeciesStrainDB <-
    genericQuery(dbToken,
                 "select distinct
                         dm.studyid  as STUDYID,
                         dm.usubjid   as USUBJID,
                         case ts1.tsval
                            when '' then null
                            else upper(trim(ts1.tsval))
                         end          as SPECIES_TS,
                         case ts2.tsval
                            when '' then null
                            else upper(trim(ts2.tsval))
                         end          as STRAIN_TS,
                         case tx2.txval
                            when '' then null
                            else upper(trim(tx2.txval))
                         end          as SPECIES_TX,
                         case tx3.txval
                            when '' then null
                            else upper(trim(tx3.txval))
                         end          as STRAIN_TX,
                         case dm.species
                            when '' then null
                            else upper(trim(dm.species))
                         end          as SPECIES_DM,
                         case dm.strain
                            when '' then null
                            else upper(trim(dm.strain))
                         end          as STRAIN_DM
                    from dm
                    left join (select distinct studyid, setcd
                             from tx
                            where txparmcd = 'TCNTRL')  tx1
                      on dm.studyid = tx1.studyid
                     and dm.setcd = tx1.setcd
                    left join ts                    ts1
                      on ts1.studyid = dm.studyid
                     and ts1.tsparmcd = 'SPECIES'
                    left join ts                    ts2
                      on ts2.studyid = dm.studyid
                     and coalesce(ts2.tsgrpid, '<null>') = coalesce(ts1.tsgrpid, '<null>')
                     and ts2.tsparmcd = 'STRAIN'
                    left join tx                    tx2
                      on tx2.studyid = dm.studyid
                     and tx2.setcd = dm.setcd
                     and tx2.txparmcd = 'SPECIES'
                    left join tx                    tx3
                      on tx3.studyid = dm.studyid
                     and tx3.setcd = dm.setcd
                     and tx3.txparmcd = 'STRAIN'
                   where dm.studyid in (:1)",
                 unique(animalList[,c('STUDYID')]))

  }else if(dbToken$dbType=='postgresql'){

  animalSpeciesStrainDB <-
    DBI::dbGetQuery(dbToken$dbHandle,
                 'SELECT DISTINCT
                         "DM"."STUDYID"  AS "STUDYID",
                         "DM"."USUBJID"   AS "USUBJID",
                         CASE "TS1"."TSVAL"
                            WHEN \'\' THEN NULL
                            ELSE UPPER(TRIM("TS1"."TSVAL"))
                         END          AS "SPECIES_TS",
                         CASE "TS2"."TSVAL"
                            WHEN \'\' THEN NULL
                            ELSE UPPER(TRIM("TS2"."TSVAL"))
                         END          AS "STRAIN_TS",
                         CASE "TX2"."TXVAL"
                            WHEN \'\' THEN NULL
                            ELSE UPPER(TRIM("TX2"."TXVAL"))
                         END          AS "SPECIES_TX",
                         CASE "TX3"."TXVAL"
                            WHEN \'\' THEN NULL
                            ELSE UPPER(TRIM("TX3"."TXVAL"))
                         END          AS "STRAIN_TX",
                         CASE "DM"."SPECIES"
                            WHEN \'\' THEN NULL
                            ELSE UPPER(TRIM("DM"."SPECIES"))
                         END          AS "SPECIES_DM",
                         CASE "DM"."STRAIN"
                            WHEN \'\' THEN NULL
                            ELSE UPPER(TRIM("DM"."STRAIN"))
                         END          AS "STRAIN_DM"
                    FROM "DM"
                    LEFT JOIN (SELECT DISTINCT "STUDYID", "SETCD"
                             FROM "TX"
                            WHERE "TXPARMCD" = \'TCNTRL\')  "TX1"
                      ON "DM"."STUDYID" = "TX1"."STUDYID"
                     AND "DM"."SETCD" = "TX1"."SETCD"
                    LEFT JOIN "TS"                    "TS1"
                      ON "TS1"."STUDYID" = "DM"."STUDYID"
                     AND "TS1"."TSPARMCD" = \'SPECIES\'
                    LEFT JOIN "TS"                    "TS2"
                      ON "TS2"."STUDYID" = "DM"."STUDYID"
                     AND COALESCE("TS2"."TSGRPID", \'<NULL>\') = COALESCE("TS1"."TSGRPID", \'<NULL>\')
                     AND "TS2"."TSPARMCD" = \'STRAIN\'
                    LEFT JOIN "TX"                    "TX2"
                      ON "TX2"."STUDYID" = "DM"."STUDYID"
                     AND "TX2"."SETCD" = "DM"."SETCD"
                     AND "TX2"."TXPARMCD" = \'SPECIES\'
                    LEFT JOIN "TX"                    "TX3"
                      ON "TX3"."STUDYID" = "DM"."STUDYID"
                     AND "TX3"."SETCD" = "DM"."SETCD"
                     AND "TX3"."TXPARMCD" = \'STRAIN\'
                   WHERE "DM"."STUDYID" IN ($1)',
                 params=list((animalList$STUDYID)))

    animalSpeciesStrainDB <- data.table::as.data.table(animalSpeciesStrainDB)

  }




  # Limit the set to the animals included in the input animalList
  animalSpeciesStrainDB <-
    data.table::merge.data.table(animalSpeciesStrainDB,
                                 animalList[,c('STUDYID', 'USUBJID')],
                                 by = c('STUDYID', 'USUBJID'))

  # Add variables SPECIES and STRAIN with the first non-empty species/strain
  # value from (in this order) DM, TX or TS
  animalSpeciesStrainDB[, `:=` (SPECIES = data.table::fcoalesce(as.character(SPECIES_DM),
                                                                 as.character(SPECIES_TX),
                                                                 as.character(SPECIES_TS)),
                                 STRAIN  = data.table::fcoalesce(as.character(STRAIN_DM),
                                                                 as.character(STRAIN_TX),
                                                                 as.character(STRAIN_TS)))]
  ## Look into the SPECIES

  # Extract unique species data per animals
  animalSpeciesAll <-
    unique(animalSpeciesStrainDB[,list(STUDYID,
                                       USUBJID,
                                       SPECIES_TS = as.character(SPECIES_TS),
                                       SPECIES_TX = as.character(SPECIES_TX),
                                       SPECIES_DM = as.character(SPECIES_DM),
                                       SPECIES = as.character(SPECIES))])
  # Add variables with
  #  - count of number of distinct SPECIES per study
  #  - concatenation of all species per study (for studies with one species, this is equal to SPECIES_TS)
  studySpecies <-
    unique(unique(animalSpeciesAll[, c('STUDYID', 'SPECIES_TS')])[
            , `:=` (NUM_SPECIES_TS = .N), by = STUDYID][
            , `:=` (ALL_SPECIES_TS = c(.SD)), by = STUDYID, .SDcols='SPECIES_TS'][
            , c('STUDYID','NUM_SPECIES_TS','ALL_SPECIES_TS')], by='STUDYID')
  # Add calculated columns to the list of animals
  animalSpeciesAll <- data.table::merge.data.table(animalSpeciesAll,
                                                   studySpecies,
                                                   by = 'STUDYID')

  # Add variable with count of unique USUBJID per study (is expected to be one usubjid per studyid per TSPARMCD 'SPECIES' )
  animalSpeciesAll[, `:=` (NUM_ANIMALS = .N), by = c('STUDYID', 'USUBJID')]


  # Identify uncertain animals
  # - remove temp columns used in the processing and remove duplicates for
  #   multiple SPECIES at study level
  animalSpecies <-
    unique(identifyUncertainSPECIESAll(dbToken,
                                       animalSpeciesAll)[, `:=` (SPECIES_TS = NULL,
                                                                 SPECIES_TX = NULL,
                                                                 SPECIES_DM = NULL,
                                                                 ALL_SPECIES_TS = NULL,
                                                                 NUM_SPECIES_TS = NULL,
                                                                 NUM_ANIMALS = NULL)],
      by=c('STUDYID', 'USUBJID'))

  ## Look into the STRAIN

  # Extract unique strain data per animal matching the set of animals filtered for species
  animalStrainAll <-
    unique(animalSpeciesStrainDB[,list(STUDYID,
                                       USUBJID,
                                       STRAIN_TS = as.character(STRAIN_TS),
                                       STRAIN_TX = as.character(STRAIN_TX),
                                       STRAIN_DM = as.character(STRAIN_DM),
                                       STRAIN = as.character(STRAIN))])

  # Add variables with
  #  - count of number of distinct STRAINS per study
  #  - concatenation of all strains per study (for studies with one strain, this is equal to STRAIN_TS)
  studyStrain <-
    unique(unique(animalStrainAll[, c('STUDYID', 'STRAIN_TS')])[
      , `:=` (NUM_STRAIN_TS = .N), by = STUDYID][
        , `:=` (ALL_STRAIN_TS = c(.SD)), by = STUDYID, .SDcols='STRAIN_TS'][
          , c('STUDYID','NUM_STRAIN_TS','ALL_STRAIN_TS')], by='STUDYID')
  # Add calculated columns to the list of animals
  animalStrainAll <- data.table::merge.data.table(animalStrainAll,
                                                  studyStrain,
                                                  by = 'STUDYID')

  # Add variable with count of unique USUBJID per study (is expected to be one usubjid per studyid per TSPARMCD 'STRAIN' )
  animalStrainAll[, `:=` (NUM_ANIMALS = .N), by = c('STUDYID', 'USUBJID')]

  # Identify uncertain animals
  # - remove temp columns used in the processing and remove duplicates for for multiple STRAIN at study level
  animalStrain <-
    unique(identifyUncertainSTRAINAll(dbToken,
                                      animalStrainAll)[, `:=` (STRAIN_TS = NULL,
                                                               STRAIN_TX = NULL,
                                                               STRAIN_DM = NULL,
                                                               ALL_STRAIN_TS = NULL,
                                                               NUM_STRAIN_TS = NULL,
                                                               NUM_ANIMALS = NULL)],
      by=c('STUDYID', 'USUBJID'))

  ## Merge lists of animals species and strain respectively
  animalSpeciesStrainAll <-
    data.table::merge.data.table(animalSpecies,
                                 animalStrain,
                                 by = c('STUDYID', 'USUBJID'))

  # Merge content of SPECIES_UNCERTAIN_MSG and STRAIN_UNCERTAIN_MSG into MSG
  #   - non-empty messages are separated by ' & '
  #   - A prefix to identify the source of the message is included as first
  #     part of the non-empty combined texts
  funcPrefix <- 'SpeciesStrain: '
  animalSpeciesStrainAll[,MSG := ifelse(!is.na(SPECIES_UNCERTAIN_MSG) & !is.na(STRAIN_UNCERTAIN_MSG),
                                        paste0(funcPrefix, paste(STRAIN_UNCERTAIN_MSG, SPECIES_UNCERTAIN_MSG, sep=' & ')),
                                        ifelse(!is.na(SPECIES_UNCERTAIN_MSG),
                                               paste0(funcPrefix, SPECIES_UNCERTAIN_MSG),
                                               ifelse(!is.na(STRAIN_UNCERTAIN_MSG),
                                                      paste0(funcPrefix, STRAIN_UNCERTAIN_MSG),
                                                      as.character(NA))))]

  # Check if a message column for uncertainties shall be included
  if (execFilter) {
    if (inclUncertain)
      # Include uncertain rows - rename MSG column accordingly
      data.table::setnames(animalSpeciesStrainAll, 'MSG' ,'UNCERTAIN_MSG')
    else
      # Don't include uncertain rows - remove rows and MSG column
      animalSpeciesStrainAll <-
        animalSpeciesStrainAll[is.na(MSG)][,`:=` (MSG = NULL,
                                                  SPECIES_UNCERTAIN_MSG = NULL,
                                                  STRAIN_UNCERTAIN_MSG = NULL)]
  } else {
    # Remove not relevant species/strain message columns
    animalSpeciesStrainAll[,`:=` (SPECIES_UNCERTAIN_MSG = NULL,
                                  STRAIN_UNCERTAIN_MSG = NULL)]
    if (noFilterReportUncertain)
      # Include all rows (no filtering of species/strain)
      #  - include reason message for species and/strain that can't be found
      # - rename MSG column accordingly
      data.table::setnames(animalSpeciesStrainAll, 'MSG' ,'NOT_VALID_MSG')
    else
      # Include all rows (no filtering of species/strain)
      #  - exclude reason message for species and/strain that can't be found
      #  - remove MSG column
      animalSpeciesStrainAll[,MSG := NULL]
  }

  if (execFilter) {
    if (length(speciesFilter) == 1)
      # One species selected - just execute  the filtering of the species/strain
      # and return result
      foundAnimalSpeciesStrain <- doFiltering(animalSpeciesStrainAll,
                                              speciesFilter,
                                              strainFilter,
                                              inclUncertain,
                                              exclusively)
    else
      # Multiple species selected - execute filtering for species/strain per species
      # - combine all outputs into one table
      foundAnimalSpeciesStrain <- unique(
        data.table::rbindlist(lapply(speciesFilter,
                                     function(species) {
                                       execOneSpeciesFilter(animalSpeciesStrainAll,
                                                            species,
                                                            strainFilter,
                                                            inclUncertain,
                                                            exclusively)
                                    }),
                              use.names=TRUE, fill=TRUE))

    ###########################

    if (exclusively) {

      if (inclUncertain) {
        # Set of studies/species where species are confidently identified
        allStudySpecies <-
          unique(animalSpeciesStrainAll[is.na(SPECIES_UNCERTAIN_MSG),
                                        list(STUDYID, SPECIES)])
        # Set of studies where all species values are uncertain
        uncertainSpeciesStudy <-
          data.table::fsetdiff(unique(animalSpeciesStrainAll[!is.na(SPECIES_UNCERTAIN_MSG),
                                                             list(STUDYID)]),
                               unique(allStudySpecies[,list(STUDYID)]))
        # Set of studies/species where species are confidently matched
        # with specified filter
        foundStudySpecies <-
          unique(foundAnimalSpeciesStrain[,list(STUDYID, SPECIES)])
      }
      else {
        # Set of studies/species - all species are confidently identified
        allStudySpecies <-
          unique(animalSpeciesStrainAll[,list(STUDYID, SPECIES)])
        # Set of studies/species - all species are confidently matched
        # with specified filter
        foundStudySpecies <-
          unique(foundAnimalSpeciesStrain[,list(STUDYID, SPECIES)])
      }

      # Get list of studies to keep at SPECIES level
      # - i.e. studies with no other SPECIES included
      keepStudies <-
        data.table::fsetdiff(unique(foundStudySpecies[,list(STUDYID)]),
                             # Find studies with animals having other SPECIES
                             # than the requested
                             unique(data.table::fsetdiff(allStudySpecies,
                                                         foundStudySpecies)[,
                                                                            list(STUDYID)]))
      if (inclUncertain)
        # Add studies where all species values are uncertain to list of studies
        # to keep (if any)
        keepStudies <-
          unique(data.table::rbindlist(list(keepStudies, uncertainSpeciesStudy)))

      if (inclStrainFilter) {
        if (inclUncertain) {
          # Set of studies/strains where strain is confidently identified
          #  - limited to the set of studies identified at species level
          allStudyStrain <-
            unique(data.table::merge.data.table(keepStudies,
                                                animalSpeciesStrainAll[is.na(STRAIN_UNCERTAIN_MSG),
                                                                            list(STUDYID, STRAIN)],
                                                by = 'STUDYID'))
          # Set of studies where all strain values are uncertain
          #  - limited to the set of studies identified at species level
          uncertainStrainStudy <-
            data.table::fsetdiff(unique(data.table::merge.data.table(keepStudies,
                                                                     animalSpeciesStrainAll[!is.na(STRAIN_UNCERTAIN_MSG),
                                                                                            list(STUDYID)],
                                                                     by = 'STUDYID')),
                                 unique(allStudyStrain[,list(STUDYID)]))
          # Set of studies/strain where strain is confidently matched
          # with specified filter
          #  - limited to the set of studies identified at species level
          foundStudyStrain <-
            unique(data.table::merge.data.table(keepStudies,
                                                foundAnimalSpeciesStrain[,list(STUDYID, STRAIN)],
                                                by = 'STUDYID'))
        }
        else {
          # Set of studies/strain - all strain values are confidently identified
          #  - limited to the set of studies identified at species level
          allStudyStrain <-
            unique(data.table::merge.data.table(keepStudies,
                                                animalSpeciesStrainAll[,list(STUDYID, STRAIN)],
                                                by = 'STUDYID'))
          # Set of studies/strain - all strain values are confidently matched
          # with specified filter
          #  - limited to the set of studies identified at species level
          foundStudyStrain <-
            unique(data.table::merge.data.table(keepStudies,
                                                foundAnimalSpeciesStrain[,list(STUDYID, STRAIN)],
                                                by = 'STUDYID'))
        }

        # Get list of studies to keep at STRAIN level
        # - i.e. studies with no other STRAIN included
        keepStudies <-
          data.table::fsetdiff(unique(foundStudyStrain[,list(STUDYID)]),
                               # Find studies with animals having other STRAIN than the requested
                               unique(data.table::fsetdiff(allStudyStrain,
                                                           foundStudyStrain)[,list(STUDYID)]))
        if (inclUncertain)
          # Add studies where all strain values are uncertain to list of studies
          # to keep (if any)
          keepStudies <-
            unique(data.table::rbindlist(list(keepStudies, uncertainStrainStudy)))


      }

      # Keep animals for studies included in the limited set of studies
      # exclusively containing filtered SPECIES/STRAIN
      foundAnimalSpeciesStrain <-
        data.table::merge.data.table(foundAnimalSpeciesStrain,
                                     keepStudies,
                                     by = 'STUDYID')
    }

    ######################################################################
  }
  else
    foundAnimalSpeciesStrain <- animalSpeciesStrainAll


  ##################################################################################################################

  # Handling of the final set of animals to return

  # Merge the set of extracted animals with the input set of animals to keep
  # any additional columns from the input table
  foundAnimals <- data.table::merge.data.table(foundAnimalSpeciesStrain,
                                               animalList,
                                               by=c('STUDYID', 'USUBJID'))

  # Do final preparation of set of found animals and return
  prepareFinalResults(foundAnimals,
                             names(animalList),
                             c('SPECIES', 'STRAIN'))
}


##  Helper functions:

##################################################################################################################
#### Identify uncertain animals at species level
##################################################################################################################
identifyUncertainSPECIESAll <- function(dbToken,
                                        animalSpeciesAll) {

  # Identify uncertainty for one animal
  identifyUncertainSPECIES <- function(SPECIES,
                                       SPECIES_TS,
                                       SPECIES_TX,
                                       SPECIES_DM,
                                       ALL_SPECIES_TS,
                                       NUM_SPECIES_TS,
                                       NUM_ANIMALS) {
    msgArr<-c()
    if (is.na(SPECIES))
      msgArr<-c(msgArr, 'TS and TX parameters SPECIES and DM.SPECIES are all missing')
    else {
      if (NUM_ANIMALS > NUM_SPECIES_TS)
        msgArr<-c(msgArr, 'TX parameter SPECIES included multiple times for the SET')
      else {
        if (! SPECIES %in% ctSPECIES) {
          if (!is.na(SPECIES_DM) & ! SPECIES_DM %in% ctSPECIES)
            msgArr<-c(msgArr, 'DM.SPECIES does not contain a valid CT value')
          else if (!is.na(SPECIES_TX) & ! SPECIES_TX %in% ctSPECIES)
            msgArr<-c(msgArr, 'TX parameter SPECIES does not contain a valid CT value')
          else if (!is.na(SPECIES_TS) & ! SPECIES_TS %in% ctSPECIES)
            msgArr<-c(msgArr, 'TS parameter SPECIES does not contain a valid CT value')
        }

        if (NUM_SPECIES_TS == 1 & length(unique(na.omit(c(SPECIES_TS, SPECIES_TX, SPECIES_DM)))) > 1)
          msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters SPECIES and/or DM.SPECIES')
        else
          if (NUM_SPECIES_TS > 1 & ((!SPECIES %in% ALL_SPECIES_TS) | (!is.na(SPECIES_TX) & !is.na(SPECIES_DM) & SPECIES_TX != SPECIES_DM)))
            msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters SPECIES and/or DM.SPECIES')
      }
    }
    msg<-paste(msgArr, collapse = ' & ')
    return(ifelse(msg=="", as.character(NA), msg))
  }

  # Get values of code lists SPECIES from CDISC CT
  ctSPECIES<-getCTCodListValues(dbToken, "SPECIES")
  # Identify uncertain animals - add variable SPECIES_UNCERTAIN_MSG
    animalSpeciesAll[,`:=` (SPECIES_UNCERTAIN_MSG=mapply(identifyUncertainSPECIES,
                                                         SPECIES,
                                                         SPECIES_TS,
                                                         SPECIES_TX,
                                                         SPECIES_DM,
                                                         ALL_SPECIES_TS,
                                                         NUM_SPECIES_TS,
                                                         NUM_ANIMALS ))]
}


##################################################################################################################
#### Identify uncertain animals at strain level
##################################################################################################################
identifyUncertainSTRAINAll <- function(dbToken,
                                       animalStrainAll) {

  # Identify uncertainty for one animal
  identifyUncertainSTRAIN <- function(STRAIN,
                                      STRAIN_TS,
                                      STRAIN_TX,
                                      STRAIN_DM,
                                      ALL_STRAIN_TS,
                                      NUM_STRAIN_TS,
                                      NUM_ANIMALS) {
    msgArr<-c()
    if (is.na(STRAIN))
      msgArr<-c(msgArr, 'TS and TX parameters STRAIN and DM.STRAIN are all missing')
    else {
      if (NUM_ANIMALS > NUM_STRAIN_TS)
        msgArr<-c(msgArr, 'TX parameter STRAIN included multiple times for the SET')
      else {
        if (! STRAIN %in% ctSTRAIN) {
          if (!is.na(STRAIN_DM) & ! STRAIN_DM %in% ctSTRAIN)
            msgArr<-c(msgArr, 'DM.STRAIN does not contain a valid CT value')
          else if (!is.na(STRAIN_TX) & ! STRAIN_TX %in% ctSTRAIN)
            msgArr<-c(msgArr, 'TX parameter STRAIN does not contain a valid CT value')
          else if (!is.na(STRAIN_TS) & ! STRAIN_TS %in% ctSTRAIN)
            msgArr<-c(msgArr, 'TS parameter STRAIN does not contain a valid CT value')
        }

        if (NUM_STRAIN_TS == 1 & length(unique(na.omit(c(STRAIN_TS, STRAIN_TX, STRAIN_DM)))) > 1)
          msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters STRAIN and/or DM.STRAIN')
        else
          if (NUM_STRAIN_TS > 1 & ((!STRAIN %in% ALL_STRAIN_TS) | (!is.na(STRAIN_TX) & !is.na(STRAIN_DM) & STRAIN_TX != STRAIN_DM)))
            msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters STRAIN and/or DM.STRAIN')
      }
    }
    msg<-paste(msgArr, collapse = ' & ')
    ifelse(msg=="", as.character(NA), msg)
  }

  # Get values of code lists STRAINS from CDISC CT
  ctSTRAIN<-getCTCodListValues(dbToken, "STRAIN")
  # Identify uncertain animals - add variable STRAIN_UNCERTAIN_MSG

  animalStrainAll[,`:=` (STRAIN_UNCERTAIN_MSG=mapply(identifyUncertainSTRAIN,
                                                       STRAIN,
                                                       STRAIN_TS,
                                                       STRAIN_TX,
                                                       STRAIN_DM,
                                                       ALL_STRAIN_TS,
                                                       NUM_STRAIN_TS,
                                                       NUM_ANIMALS ))]
}

##################################################################################################################
#### Doing the filtering for one species and potential list of related strain(s)
##################################################################################################################
doFiltering <- function(animalSpeciesStrainAll,
                        speciesFilter,
                        strainFilter,
                        inclUncertain,
                        exclusively) {

  if (!(is.null(strainFilter) | isTRUE(is.na(strainFilter)) | isTRUE(strainFilter == "")))
    InclStrainFilter <- TRUE
  else InclStrainFilter <- FALSE

  ## Do filtering at SPECIES level

  # Extract animals matching the species filter
  if (inclUncertain)
    # - include species level uncertain rows
    foundAnimals <-
      animalSpeciesStrainAll[SPECIES %in% speciesFilter | !is.na(SPECIES_UNCERTAIN_MSG)]
  else
    # - no uncertain rows
    foundAnimals <-
      animalSpeciesStrainAll[SPECIES %in% speciesFilter]

  if (InclStrainFilter) {
    ## Do filtering at STRAIN level

    # Extract animals matching the strain filter
    if (inclUncertain)
      # - include all uncertain rows
      foundAnimals <-
        foundAnimals[STRAIN %in% strainFilter | !is.na(UNCERTAIN_MSG)]
    else
      # - no uncertain rows
      foundAnimals <-
        animalSpeciesStrainAll[STRAIN %in% strainFilter]

  }

  if (inclUncertain)
    # Exclude species/strain level message columns
    foundAnimals[,`:=` (SPECIES_UNCERTAIN_MSG = NULL,
                        STRAIN_UNCERTAIN_MSG = NULL)]
  foundAnimals
}

##################################################################################################################
#### Extract potential list of strains from strainFilter for actual species and execute filtering
##################################################################################################################
execOneSpeciesFilter <- function(animalSpeciesStrainAll,
                                 species,
                                 strainFilter,
                                 inclUncertain,
                                 exclusively) {
  # Extract list of selected strains for current species
  # - remove prefixed species value
  strainList <-
    stringr::str_replace(strainFilter[stringr::str_detect(strainFilter,
                                      paste0(species,': *'))],
                         paste0(species,': *'),'')
  if (length(strainList) == 0) strainList <- NULL

  # Execute species/strain filtering for current species/strain(s)
  doFiltering(animalSpeciesStrainAll,
                     species,
                     strainList,
                     inclUncertain,
                     exclusively)
}


