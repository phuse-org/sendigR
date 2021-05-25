library(magrittr)
library(sendigR)

dbToken <- initEnvironment(dbType='sqlite',
                           dbPath='//FSDKHQ001/dep402$/000-4284/DATA MANAGEMENT/@Data Science/BioCelerate/SEND Harmonization/Data/db/send02.db')
# ,
#                            ctFile='//FSDKHQ001/dep402$/000-4284/DATA MANAGEMENT/@Data Science/BioCelerate/SEND Harmonization/Data/metadata/SEND_Terminology_2019-12-27.xls')

pInclUncertain <- TRUE

studyList <-
  getStudiesSDESIGN(dbToken,
                    studyDesignFilter = 'PARALLEL',
                    inclUncertain     = pInclUncertain,
                    exclusively       = FALSE) %>%
  getStudiesSTSTDTC(dbToken, ., '2016','2020')

controlAnimals <-
  getControlSubj(dbToken, studyList,
                 inclUncertain = pInclUncertain)

animals <- getSubjSex(dbToken, controlAnimals, 'M')

animals <- getSubjSpeciesStrain(dbToken, animals,
                                speciesFilter = 'RAT',
                                strainFilter = NULL,
                                inclUncertain = pInclUncertain,
                                exclusively = FALSE,
                                noFilterReportUncertain = TRUE)

animals <- getSubjRoute(dbToken, animals,
                        #routeFilter = c('ORAL'),
                        #routeFilter = c('SUBCUTANEOUS'),
                        inclUncertain = TRUE,
                        exclusively = FALSE,
                        matchAll = FALSE,
                        noFilterReportUncertain = TRUE)

dataLB = getSubjData(dbToken, animals,
                     domain = 'LB',
                     colList = c('LBTESTCD', 'LBSTRESC'))

dataLBPhase = getFindingsPhase(dbToken, findings = dataLB,
                               phaseFilter = "treatment",
                               inclUncertain = pInclUncertain,
                               noFilterReportUncertain = TRUE)

dataLBAge = getFindingsSubjAge(dbToken,
                               findings = dataLBPhase,
                               animalList = animals,
                               fromAge = '160d',
                               inclUncertain = pInclUncertain,
                               noFilterReportUncertain = TRUE)

dataFW = getSubjData(dbToken, animals, 'FW')

dataFWPhase = getFindingsPhase(dbToken, findings = dataFW,
                               phaseFilter = "treatment",
                               inclUncertain = pInclUncertain,
                               noFilterReportUncertain = TRUE)

dataFWAge = getFindingsSubjAge(dbToken,
                               findings = dataFWPhase,
                               animalList = animals,
                               fromAge = '160d',
                               inclUncertain = pInclUncertain,
                               noFilterReportUncertain = TRUE)

dataDM = getSubjData(dbToken, animals, 'DM', colList = 'SPECIES')

dataMI =  getSubjData(dbToken, animals, 'MI')

#dummy = getSubjData(dbToken, animals, 'TZ')


disconnectDB(dbToken)


###############################################################################################
studyList <- getStudiesSDESIGN(dbToken)
