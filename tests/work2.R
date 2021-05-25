library(magrittr)
library(sendigR)

dbToken <- initEnvironment(dbType='sqlite',
                           dbPath='//FSDKHQ001/dep402$/000-4284/DATA MANAGEMENT/@Data Science/BioCelerate/SEND Harmonization/Data/db/send02.db')

pInclUncertain <- FALSE

studyList <-
  getStudiesSDESIGN(dbToken,
                    studyDesignFilter = 'PARALLEL',
                    inclUncertain     = pInclUncertain) %>%
  getStudiesSTSTDTC(dbToken, ., '1998','2021')

controlAnimals <-
  getControlSubj(dbToken, studyList,
                 inclUncertain = pInclUncertain)

animals <- getSubjSex(dbToken, controlAnimals)

animals <- getSubjSpeciesStrain(dbToken, animals,
                                speciesFilter = c('RAT','MONKEY'),
                                strainFilter = c('RAT:SPRAGUE-DAWLEY','RAT:SPRAGUE DAWLEY','RAT:WISTAR HAN','MONKEY:CYNOMOLGUS'),
                                inclUncertain = pInclUncertain)

animals <- getSubjRoute(dbToken, animals,
                        #routeFilter = c('ORAL'),
                        routeFilter = c('SUBCUTANEOUS','ORAL','ORAL GAVAGE'),
                        inclUncertain = pInclUncertain)

dataMI = getSubjData(dbToken, animals,
                     domain = 'MI')


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
