rm(list=ls())

# Load Packages
library(SASxport)
library(rlist)
library(purrr)

# Define %ni% function
'%ni%' <- Negate('%in%')

# Set working directory to a location with the following structure:
# A directory for each application (IND/NDA/BLA) containing
# directories for each study within each application containing
# the SEND dataset for each study
baseDir <- '~/PhUSE/BioCelerate/All_SEND_Data_From_Janus_Oct022019'
setwd(baseDir)

# Get list of application folders
studiesList <- paste(path.expand(baseDir),list.dirs(recursive = T,full.names = T),sep='/')
studies <- grep('\\d{6,}/',studiesList,value = T)

# List variables and parameter codes of interest
variableCountList <- list(
  # Dose Duration Variables:
  # TSparmcds = c('DOSDUR'=0,'EXPSTDTC'=0,'EXPENDTC'=0,'DOSSTDTC'=0,'DOSENDTC'=0),
  # TXparmcds = c('DOSSTDTC'=0,'DOSENDTC'=0),
  # DMvariables = c('RFSTDTC'=0,'RFENDTC'=0,'RFXSTDTC'=0,'RFXENDTC'=0),
  # EXvariables = c('EXSTDTC'=0,'EXENDTC'=0,'EXSTDY'=0,'EXENDY'=0,'EXDUR'=0)
  
  # Route of Administration Variables:
  TSparmcds = c('DOSDUR'=0,'EXPSTDTC'=0,'EXPENDTC'=0,'DOSSTDTC'=0,'DOSENDTC'=0),
  TXparmcds = c('DOSSTDTC'=0,'DOSENDTC'=0),
  DMvariables = c('RFSTDTC'=0,'RFENDTC'=0,'RFXSTDTC'=0,'RFXENDTC'=0),
  EXvariables = c('EXSTDTC'=0,'EXENDTC'=0,'EXSTDY'=0,'EXENDY'=0,'EXDUR'=0)
  # LBvariables = c('LBNAM'=0),
  # MAvariables = c('MANAM'=0),
  # MIvariables = c('MINAM'=0),
  # PCvariables = c('PCNAM'=0),
  # TFvariables = c('PCNAM'=0),
  # EGvariables = c('EGNAM'=0)
)

sampleDepth <- 1000

DOIs <- unique(paste0(substr(names(variableCountList),1,2)))
DOIfiles <- paste0(tolower(DOIs),'.xpt')

# Find paths to datasets containing all domains of interest
print('Finding valid studies for each domain...')
validStudies <- NULL
for (DOIfile in DOIfiles) {
  assign(DOIfile,NULL)
}
for (study in studies) {
  flag <- F
  studyFiles <- list.files(study)
  for (DOIfile in DOIfiles) {
    if (DOIfile %in% studyFiles) {
      flag <- T
      if (DOIfile=='ts.xpt') {
        if ('dm.xpt' %in% studyFiles) {
          DOIflag <- T
        } else {
          DOIflag <- F
        }
      } else {
        DOIflag <- T
      }
    } else {
      DOIflag <- F
    }
    assign(DOIfile,c(get(DOIfile),DOIflag))
  }
  if (flag==T) {
    validStudies <- c(validStudies,study)
  }
}
nValidStudies <- length(validStudies)
studyValidity <- studies
for (DOIfile in DOIfiles) {
  studyValidity <- as.data.frame(cbind(studyValidity,get(DOIfile)))
}
row.names(studyValidity) <- studies
studyValidity <- studyValidity[,-1]
colnames(studyValidity) <- DOIs
nStudyValidity <- sapply(studyValidity,table)['TRUE',]

nMarkers <- 20
markers <- round(seq(round(nValidStudies/10),nValidStudies,length.out = nMarkers))
markerStudies <- validStudies[markers]

variableValueList <- list()
variableUnitList <- list()
for (study in validStudies) {
  studyName <- unlist(strsplit(study,'/./'))[2]
  if (study %in% markerStudies) {
    print(paste0(round(100/nMarkers*which(markerStudies==study)),'% Complete...'))
  }
  for (domain in DOIs) {
    if (studyValidity[study,domain]==T) {
      domainData <- tryCatch(read.xport(paste0(study,'/',tolower(domain),'.xpt')),
                             error= function(err) {
                               message(paste0('Error reading file: ',study,'/',tolower(domain),'.xpt\n(file skipped)'))
                               return(NA)
                               })
      if (is.null(dim(domainData))) {
        next
      }
      domainMatches <- grep(domain,names(variableCountList),ignore.case = T)
      for (match in domainMatches) {
        domainName <- names(variableCountList[match])
        if (length(grep('parmcds',domainName)>0)) {
          parmcd <- T
        } else {
          parmcd <- F
        }
        if (domainName %ni% names(variableValueList)) {
          variableValueList[[domainName]] <- list()
          variableUnitList[[domainName]] <- list()
        }
        for (variable in names(variableCountList[[match]])) {
          if ((variable %in% names(domainData))|(variable %in% domainData[[paste0(domain,'PARMCD')]])) {
            if (parmcd==T) {
              index <- which(domainData[[paste0(domain,'PARMCD')]]==variable)
            }
            if ((length(which(!is.na(domainData[[variable]]))>0))|(length(which(!is.na(domainData[[paste0(domain,'VAL')]][index])))>0)) {
              variableCountList[[match]][[variable]] <- variableCountList[[match]][[variable]] + 1
              if (parmcd==T) {
                values <- domainData[[paste0(domain,'VAL')]][index]
              } else {
                values <- domainData[[variable]]
              }
              if (length(levels(values))>0) {
                values <- levels(values)[values]
              }
              uniqueValues <- unique(values)
              units <- NULL
              if (length(uniqueValues)<sampleDepth) {
                depth <- length(uniqueValues)
              } else {
                depth <- sampleDepth
              }
              for (value in sample(uniqueValues,depth)) {
                if (!is.na(value)) {
                  if (length(grep('P[0-9T]',substr(value,1,2)))>0) {
                    units <- c(units,substr(value,nchar(value),nchar(value)))
                  } else {
                    transformedValue <- gsub('[[:digit:]]+','*',value)
                    if (length(grep('*',transformedValue,fixed=T))>0) {
                      transformedValue <- gsub(' ','',transformedValue)
                      if (length(grep('**',transformedValue,fixed=T))>0) {
                        transformedValue <- gsub('**','*',transformedValue,fixed = T)
                      }
                    }
                    units <- c(units,transformedValue)
                  }
                }
              }
              units <- sort(unique(units))
              units <- paste(units,collapse=' & ')
              if (variable %ni% names(variableValueList[[domainName]])) {
                variableValueList[[domainName]][[variable]] <- list(newStudy=values)
                variableUnitList[[domainName]][[variable]] <- list(newStudy=units)
              } else {
                variableValueList[[domainName]][[variable]] <- list.append(variableValueList[[domainName]][[variable]],newStudy=values)
                variableUnitList[[domainName]][[variable]] <- list.append(variableUnitList[[domainName]][[variable]],newStudy=units)
              }
              names(variableValueList[[domainName]][[variable]])[length(variableValueList[[domainName]][[variable]])] <- studyName
              names(variableUnitList[[domainName]][[variable]])[length(variableUnitList[[domainName]][[variable]])] <- studyName
            }
          }
        }
      }
    }
  }
}

variableUnitSummary <- variableUnitList

for (domainName in names(variableCountList)) {
  domain <- substr(domainName,1,2)
  for (variable in names(variableCountList[[domainName]])) {
    variableUnitSummary[[domainName]][[variable]] <- data.frame(table(unlist(unname(map(variableUnitList[[domainName]][[variable]],1)))))
    if (nrow(variableUnitSummary[[domainName]][[variable]])>0) {
      variableUnitSummary[[domainName]][[variable]] <- variableUnitSummary[[domainName]][[variable]][order(variableUnitSummary[[domainName]][[variable]][,2],decreasing = T),]
      Examples <- NULL
      for (i in seq(nrow(variableUnitSummary[[domainName]][[variable]]))) {
        index <- which(variableUnitList[[domainName]][[variable]]==variableUnitSummary[[domainName]][[variable]][i,1])[1]
        terms <- variableUnitSummary[[domainName]][[variable]][i,1]
        if (length(levels(terms))>0) {
          terms <- levels(terms)[terms]
        }
        terms <- unlist(strsplit(terms,' & '))
        for (term in terms) {
          if (term=='-*') {
            term <- '-'
            termIndexTmp <- grep(term,variableValueList[[domainName]][[variable]][[index]],fixed=T)
          } else if (length(grep('(',term,fixed=T))>0) {
            termIndexTmp <- grep(term,variableValueList[[domainName]][[variable]][[index]],fixed=T)
          } else {
            termIndexTmp <- grep(term,variableValueList[[domainName]][[variable]][[index]])
          }
          # This is a kludge that should be refined
          if (length(termIndexTmp)==0) {
            termIndexTmp <- 1
          }
          if (term==terms[1]) {
            termIndex <- sample(termIndexTmp,1)
          } else {
            termIndex <- c(termIndex,sample(termIndexTmp,1))
          }
        }
        Examples[i] <- paste(variableValueList[[domainName]][[variable]][[index]][termIndex],collapse=' & ')
      }
      variableUnitSummary[[domainName]][[variable]] <- cbind(variableUnitSummary[[domainName]][[variable]],Examples)
      colnames(variableUnitSummary[[domainName]][[variable]]) <- c('Unit','Frequency','Example')
      if (length(grep('parmcd',domainName))>0) {
        cat(paste0(domain,' Domain PARMCD: ',variable,' was populated in ',
                   variableCountList[[domainName]][[variable]],' of ',nStudyValidity[[domain]], ' Studies\n'))
      } else {
        cat(paste0(domain,' Domain Variable: ',variable,' was populated in ',
                   variableCountList[[domainName]][[variable]],' of ',nStudyValidity[[domain]], ' Studies\n'))
      }
      cat('and used the following units (capped at 20):\n')
      print(head(variableUnitSummary[[domainName]][[variable]],n = 20),row.names=F)
      cat('\n')
    }
    else {
      if (length(grep('parmcd',domainName))>0) {
        cat(paste0(domain,' Domain PARMCD: ',variable,' was populated in ',
                   '0',' of ',nStudyValidity[[domain]], ' Studies\n\n'))
      } else {
        cat(paste0(domain,' Domain Variable: ',variable,' was populated in ',
                   '0',' of ',nStudyValidity[[domain]], ' Studies\n\n'))
      }
    }
  } 
}
# print(table(unlist(unname(map(variableUnitList$TSparmcds$DOSDUR,1)))))
