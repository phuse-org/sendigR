source("sendDB.R")


pFromDTC <- "2004-06-04"
pToDTC <- "2021-01-28"
pStudyDesign <- "PARALLEL"
pInclUncertain <- TRUE

studiesList <- sendigR::getStudiesSTSTDTC(dbToken,
                                          fromDTC       = pFromDTC, 
                                          toDTC         = pToDTC, 
                                          inclUncertain = pInclUncertain)

allParallel <- sendigR::genericQuery(dbToken, 'select * from ts where tsparmcd == "SDESIGN" and tsval == "PARALLEL"')

studiesAll <<- sendigR::getStudiesSDESIGN(dbToken, 
                                          studyDesignFilter = pStudyDesign, 
                                          studyList         = studiesList, 
                                          inclUncertain     = pInclUncertain,
                                          exclusively = TRUE)


controlAnimalsAll <<- sendigR::getControlSubj(dbToken,
                                              studyList     = studiesAll, 
                                              inclUncertain = pInclUncertain)


controlAnimals <<- sendigR::getSubjSex(dbToken,
                                       animalList    = controlAnimals,
                                       sexFilter     = "M",
                                       inclUncertain = pInclUncertain)