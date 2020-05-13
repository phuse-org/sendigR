source("importSENDDomains.R")
source("filterStudyAnimalRoute.R")
source("studyListStudyDesign.R")
source("studyListStudyStartDate.R")
source("animalListControl.R")
source("filterAnimalsSex.R")
source("filterFindingsPhase.R")
source("subjDataExtract.R")
source("addFindingsAnimalAge.R")
source("filterFindingsAnimalAge.R")
source("filterStudyAnimalSpeciesStrain.R")


# adjustable parameters

# study level
design <- "PARALLEL"
route1 <- "ORAL"
route2 <- "ORAL GAVAGE"
sex <- "M"
species <- "DOG"
strain <- "BEAGLE"
startdate <- "2017"
enddate <- "2020"
sex <- "M"
lowerBoundAge <- '12m'
upperBoundAge <- '18m'
phase <- "Treatment"

# Get study level parameters first

#### Study Start Date ####

main_df <- unique(GetStudyListSTSTDTC(fromDTC=startdate, toDTC=enddate))
print(sprintf("Current number of studies after study start date: %s", nrow(main_df)))

#### Study Route ####
# studies could be label as 'ORAL' or 'ORAL GAVAGE'
# so query both, but drop duplicates (i.e., studies
# listed as both)

route_studies <- rbind(GetStudyListROUTE(route1), GetStudyListROUTE(route2))

# only get one unique studies 
# e.g., those not listed as ORAL and 
# ORAL GAVAGE
route_studies <- route_studies[!duplicated(route_studies$STUDYID),]

main_df <- merge(main_df, route_studies)
print(sprintf("Number of studies after route: %s", nrow(main_df)))


#### Study Design ####

design_studies <- GetStudyListSDESIGN(design)
main_df <- merge(main_df, design_studies)
print(sprintf("Current number of studies after study design: %s", nrow(main_df)))

#### Species at study level ####

species_studies <- GetStudyListSPECIES_STRAIN(species=species)
main_df <- merge(main_df, species_studies)
print(sprintf("Current number of studies after species: %s", nrow(main_df)))

#### Species and Strain at study level ####

main_df <- main_df[main_df$STRAIN == strain]
print(sprintf("Current number of studies after strain: %s", nrow(main_df)))


#### Switch from Study level to Animal level

#### Get Control Animals for all then filter by current study list ###
control_animals <- unique(GetControlAnimals(main_df))
print(sprintf("Number of control animals in current studies: %s", nrow(control_animals)))


# Apply animal filters for ROUTE and Species and Strain
# There seems to be a giant drop off when using the 
# filter animal species function, from 
# 1015 animals -> 173.  This seems strange since
# the only thing that should be filtered here 
# are duplicate animals, strangs
control_animals <- FilterAnimalListRoute(control_animals, main_df)
print(sprintf("Number of control animals after Route filter: %s", nrow(control_animals)))

control_animals <- FilterAnimalsSpeciesStrain(control_animals, speciesStrainFilter=main_df)
print(sprintf("Number of control animals after species strain filter: %s", nrow(control_animals)))

# unique requirement is necessary because there
# are duplicate entries in DM, due to user input
control_data <- unique(ExtractSubjData("DM", control_animals)[,.(STUDYID,USUBJID,SEX)])
control_animals <- unique(merge(control_animals, control_data))
control_animals <- control_animals[control_animals$SEX == sex]
print(sprintf("Number of control animals in the current study after sex: %s", nrow(control_animals)))

# get microscopic findings for current animals
# only need to get the variables required for
# finding the animal age and calculating the
# phase of the finding as well those needs
# to calculate organ level findings (MISPEC, MISTRESC)

mi_findings <- unique(ExtractSubjData("MI", control_animals)[,.(STUDYID, USUBJID, MISPEC, MISTRESC, MISEQ, MIDTC, MIDY)])

# only look at findings that are specific to the treatment phase
mi_findings <- FilterFindingsPhase("MI", mi_findings, phase)
print(sprintf("There are %s MI findings at the treatment phase", nrow(mi_findings)))

# add age to the fingdings and filter by the
# and lower bounds for animals of a certain
# age at the time of finding
mi_findings <- addFindingsAnimalAge("MI", mi_findings)
final_findings <- filterFindingsAnimalAge(mi_findings, fromAge=lowerBoundAge, toAge=upperBoundAge, inclUncertain=FALSE)
print(sprintf("Number of findings in the current study at age: %s", nrow(final_findings)))

final_findings$MISTRESC = toupper(final_findings$MISTRESC)

# count the number of findings per animal, per organ
# only keep interesting finings, ie., remove 
# NORMAL, UNREMARKABLE, or blank
findings_count <- final_findings %>%
                    dplyr::distinct(STUDYID, USUBJID, MISPEC, MISTRESC) %>% # only one organ, finding per animal (input errors cause duplications)
                    dplyr::count(MISPEC, MISTRESC) %>%
                    dplyr::arrange(-n) %>%
                    dplyr::filter(MISTRESC != 'NORMAL', MISTRESC != 'UNREMARKABLE', MISTRESC != '')
findings_count$incidence <- findings_count$n / length(unique(final_findings$USUBJID))
findings_count <- dplyr::select(findings_count, -n)

write.csv(findings_count, 'data/RankedMIFindings.csv')
