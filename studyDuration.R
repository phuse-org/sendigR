rm(list=ls())

library(stringr)
library(SASxport)

# source('https://raw.githubusercontent.com/phuse-org/phuse-scripts/master/contributed/Nonclinical/R/Functions/Functions.R')
source('~/PhUSE/Repo/trunk/contributed/Nonclinical/R/Functions/Functions.R')

datapath <- '~/PhUSE/BioCelerate/All_SEND_Data_From_Janus_Oct022019'

studiesList <- list.dirs(path = path.expand(datapath),recursive = T,full.names = T)
studies <- grep('\\d{6,}/',studiesList,value = T)

DOIs <- c('TS')
DOIfiles <- paste0(tolower(DOIs),'.xpt')

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
          flag <- F
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

nDOSDURn <- 0
nDOSDURm <- 0
studyDuration <- data.frame(DOSDUR=rep(NA,length(validStudies)),Duration=rep(NA,length(validStudies)),row.names = validStudies)
for (study in validStudies) {
  Data <- read.xport(paste0(study,'/ts.xpt'))
  if (length(levels(Data$TSVAL))>0) {
    Data$TSVAL <- levels(Data$TSVAL)[Data$TSVAL]
  }
  DOSDUR <- unique(Data$TSVAL[which(Data$TSPARMCD=='DOSDUR')])
  if (length(DOSDUR)==1) {
    studyDuration[study,'DOSDUR'] <- DOSDUR
    if (substr(DOSDUR,1,1)=='P') {
      DOSDURs <- DUR_to_seconds(DOSDUR)
      DOSDURd <- DOSDURs/60/60/24
      if (substr(DOSDUR,nchar(DOSDUR),nchar(DOSDUR))=='M') {
        DOSDURd <- (round(DOSDURd/7,digits=0)*7)
      }
      studyDuration[study,'Duration'] <- DOSDURd
    }
  } else if (length(DOSDUR)>1) {
    studyDuration[study,'DOSDUR'] <- '[Multiple Values]'
    message(paste0('Multiple DOSDUR Values Provided for Study:\n'))
    message(paste0(DOSDUR,collapse=', '))
    nDOSDURm <- nDOSDURm + 1
  } else {
    studyDuration[study,'DOSDUR'] <- '[Missing]'
    message(paste0('No DOSDUR Value Provided for Study:\n'))
    nDOSDURn <- nDOSDURn + 1
  }
}

resultTable <- table(studyDuration$Duration)
print('Frequency of All DOSDUR Durations:')
print(resultTable)

threshold <- 25
filteredResultTable <- resultTable[which(resultTable>=threshold)]
print('Frequency of Common DOSDUR Durations:')
print(filteredResultTable)

tickBy <- 50
plot(as.numeric(names(resultTable)),resultTable,type='l',
     main='DOSDUR Analysis',xlab='Dosing Duration',ylab='# of Studies',
     xaxt='n',yaxt='n')
xtick <- seq(0,max(studyDuration$Duration,na.rm=T),by=tickBy)
axis(side=1,at=xtick,labels=F)
text(x=xtick,  par("usr")[3], 
     labels = xtick, srt = 45, pos = 1, xpd = TRUE)

ytick <- seq(0,max(resultTable),by=tickBy)
axis(side=2, at=ytick, labels = FALSE)
text(par("usr")[1], ytick,  
     labels = ytick, srt = 45, pos = 2, xpd = TRUE)

print('Frequency of ISO 8601 non-conformant values of DOSDUR:')
print(sort(table(studyDuration[which(is.na(studyDuration$Duration)),'DOSDUR']),decreasing=T))

