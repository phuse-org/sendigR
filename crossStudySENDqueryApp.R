# Load Packages
library(shiny)
library(SASxport)
library(rlist)
library(purrr)
library(shinyFiles)

# Bugs:
# - Can't uncheck the last parameter in a domain

# Define %ni% function
'%ni%' <- Negate('%in%')

# Set working directory to a location with the following structure:
# A directory for each application (IND/NDA/BLA) containing
# directories for each study within each application containing
# the SEND dataset for each study
defaultDir <- '~/PhUSE/BioCelerate/All_SEND_Data_From_Janus_Oct022019'
# setwd(baseDir)

nMarkers <- 20

values <- reactiveValues()
values$parameterList <- list()
values$flag <- F
values$variableCountList <- list()

# # List variables and parameter codes of interest
# variableCountList <- list(
#   # Dose Duration Variables:
#   # TSparmcds = c('DOSDUR'=0,'EXPSTDTC'=0,'EXPENDTC'=0,'DOSSTDTC'=0,'DOSENDTC'=0),
#   # TXparmcds = c('DOSSTDTC'=0,'DOSENDTC'=0),
#   # DMvariables = c('RFSTDTC'=0,'RFENDTC'=0,'RFXSTDTC'=0,'RFXENDTC'=0),
#   # EXvariables = c('EXSTDTC'=0,'EXENDTC'=0,'EXSTDY'=0,'EXENDY'=0,'EXDUR'=0)
# 
#   # Route of Administration Variables:
#   TSparmcds = c('DOSDUR'=0,'EXPSTDTC'=0,'EXPENDTC'=0,'DOSSTDTC'=0,'DOSENDTC'=0),
#   TXparmcds = c('DOSSTDTC'=0,'DOSENDTC'=0),
#   DMvariables = c('RFSTDTC'=0,'RFENDTC'=0,'RFXSTDTC'=0,'RFXENDTC'=0),
#   EXvariables = c('EXSTDTC'=0,'EXENDTC'=0,'EXSTDY'=0,'EXENDY'=0,'EXDUR'=0)
#   # LBvariables = c('LBNAM'=0),
#   # MAvariables = c('MANAM'=0),
#   # MIvariables = c('MINAM'=0),
#   # PCvariables = c('PCNAM'=0),
#   # TFvariables = c('PCNAM'=0),
#   # EGvariables = c('EGNAM'=0)
# )

sampleDepth <- 100

cap <- 20

ui <- fluidPage(
  
  titlePanel("Query SEND Database"),
  
  sidebarLayout(
    sidebarPanel(
      h4('Selected SEND Database Folder:'),
      verbatimTextOutput("dir", placeholder = TRUE),
      shinyDirButton(id = "dir", label = "Change Selection",title='Select SEND Database Folder'),
      br(),br(),
      actionButton('run','Run Analysis'),
      br(),br(),
      selectInput('sendVersion','Select Version of SEND:','3.1'),
      uiOutput('selectDomain'),
      radioButtons('type','Variable or PARMCD?',c('Variable')),
      uiOutput('selectParameter'),
      actionButton('addParameter','Add to List'),
      br(),br(),
      uiOutput('parameterList')
    ),
    
    mainPanel(
      verbatimTextOutput('printResults',placeholder = TRUE)
    )
  )
)
  
server <- function(session,input, output) {
  
  getSENDIG <- reactive({
    SENDIG <- read.csv(paste0('SENDv',input$sendVersion,'/variables.csv'),stringsAsFactors = F)
    return(SENDIG)
  })
  
  output$selectDomain <- renderUI({
    SENDIG <- getSENDIG()
    domains <- sort(unique(SENDIG$Domain.Prefix))
    selectInput('selectDomain','Select SEND Domain:',domains)
  })
  
  observe({
    req(input$selectDomain)
    SENDIG <- getSENDIG()
    index <- which(SENDIG$Domain.Prefix==input$selectDomain)
    parameters <- SENDIG$Variable.Name[index]
    if (length(grep('PARMCD',parameters))==1) {
      updateRadioButtons(session,'type',choices=c('Variable','PARMCD'))
    } else {
      updateRadioButtons(session,'type',choices='Variable')
    }
  })
  
  output$selectParameter <- renderUI({
    SENDIG <- getSENDIG()
    if (input$type=='Variable') {
      index <- which(SENDIG$Domain.Prefix==input$selectDomain)
      parameters <- SENDIG$Variable.Name[index]
    } else if (input$type=='PARMCD') {
      PARMCDs <- read.csv(paste0('SENDv',input$sendVersion,'/parmcds.csv'),stringsAsFactors = F)
      index <- which(PARMCDs$Domain==input$selectDomain)
      parameters <- PARMCDs$PARMCD[index]
    }
    selectizeInput(inputId='selectParameter',label=paste0('Select ',input$type,':'),choices=parameters,selected=NULL,options=list(placeholder='Select a Value or Start Typing...',onInitialize= I('function() { this.setValue(""); }')))
  })
  
  observeEvent(input$addParameter,{
    if (input$type == 'Variable') {
      values$parameterList[[input$selectDomain]] <- c(values$parameterList[[input$selectDomain]],input$selectParameter)
    } else if (input$type == 'PARMCD') {
      values$parameterList[[paste0(input$selectDomain,' PARMCD')]] <- c(values$parameterList[[paste0(input$selectDomain,' PARMCD')]],input$selectParameter)
    }
    values$flag <- F
  })
  
  output$parameterList <- renderUI({
    if (length(values$parameterList)>0) {
      lapply(1:length(values$parameterList), function(i) {
        domain <- names(values$parameterList)[i]
        checkboxGroupInput(paste0(domain,'parameterList'),domain,values$parameterList[[domain]],selected=values$parameterList[[domain]])
      })
    }
  })
  
  observe({
    for (item in names(values$parameterList)) {
      isolate({
        values$variableCountList[[item]] <- rep(0,length(values$parameterList[[item]]))
        names(values$variableCountList[[item]]) <- values$parameterList[[item]]
      })
    }
  })
  
  toListen <- reactive({
    domainInputList <- list()
    if (length(values$parameterList)>0) {
      for (i in seq(length(values$parameterList))) {
        # req(input[[paste0(names(values$parameterList)[i],'parameterList')]])
        domainInputList[[i]] <- input[[paste0(names(values$parameterList)[i],'parameterList')]]
      }
    }
    return(domainInputList)
  })
  
  observeEvent({
    toListen()
  },
  {
    domains <- names(values$parameterList)
    for (domain in domains) {
      if (values$flag==T) {
        if (is.null(input[[paste0(domain,'parameterList')]])) { # it doesn't work becasue the input is already null/gone!
          index <- which(names(values$parameterList)==domain)
          values$parameterList <- values$parameterList[-index]
          removeUI(selector=paste0('#',domain,'parameterList'))
        } else {
          values$parameterList[[domain]] <- input[[paste0(domain,'parameterList')]]
          updateCheckboxGroupInput(session,paste0(domain,'parameterList'),choices=values$parameterList[[domain]],selected=values$parameterList[[domain]])
        }
      }
    }
    values$flag <- T
  })
  
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = path.expand('~'))
  )
  
  global <- reactiveValues(datapath = defaultDir)
  
  dir <- reactive(input$dir)
  
  output$dir <- renderText({
    path.expand(global$datapath)
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
               })
  
  getValidStudies <- reactive({
    for (i in seq(length(values$parameterList))) {
      req(input[[paste0(names(values$parameterList)[i],'parameterList')]])
    }

    # Get list of application folders
    studiesList <- list.dirs(path = path.expand(global$datapath),recursive = T,full.names = T)
    studies <- grep('\\d{6,}/',studiesList,value = T)
    
    DOIs <- unique(paste0(substr(names(values$parameterList),1,2)))
    DOIfiles <- paste0(tolower(DOIs),'.xpt')
    
    # Find paths to datasets containing all domains of interest
    # print('Finding valid studies for each domain...')
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
    colnames(studyValidity) <- c('studies',DOIs)
    row.names(studyValidity) <- studies
    # studyValidity <- studyValidity[,-1]
    if (ncol(studyValidity)>2) {
      nStudyValidity <- sapply(studyValidity[,2:ncol(studyValidity)],table)['TRUE',]
    } else {
      nStudyValidity <- length(which(studyValidity[,2]==TRUE))
      names(nStudyValidity) <- colnames(studyValidity)[2]
    }
    result <- list(validStudies,nValidStudies,studyValidity,nStudyValidity)
    return(result)
  })
  
  queryValues <- reactive({
    withProgress(message = 'Finding Valid Datasets...',value=0,{
      studyList <- getValidStudies()
      validStudies <- studyList[[1]]
      nValidStudies <- studyList[[2]]
      studyValidity <- studyList[[3]]
      nStudyValidity <- studyList[[4]]
      
      variableCountList <- values$variableCountList

      DOIs <- unique(paste0(substr(names(values$parameterList),1,2)))
      
      markers <- round(seq(round(nValidStudies/10),nValidStudies,length.out = nMarkers))
      markerStudies <- validStudies[markers]
      
      variableValueList <- list()
      variableUnitList <- list()
      for (study in validStudies) {
        studyName <- unlist(strsplit(study,'/./'))[2]
        if (study %in% markerStudies) {
          setProgress(value=1/nMarkers*which(markerStudies==study),message='Evaluating Datasets...')
          # print(paste0(round(100/nMarkers*which(markerStudies==study)),'% Complete...'))
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
              if (length(grep('PARMCD',domainName)>0)) {
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
                  } else {
                    index <- NULL
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
    })
    resultList <- list(variableCountList,variableValueList,variableUnitList,nStudyValidity)
    return(resultList)
  })
  
  prepareResults <- reactive({
    resultList <- queryValues()
    variableCountList <- resultList[[1]]
    variableValueList <- resultList[[2]]
    variableUnitList <- resultList[[3]]
    nStudyValidity <- resultList[[4]]
    
    variableUnitSummary <- variableUnitList
    
    toPrint <- ''
    
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
          if (length(grep('PARMCD',domainName))>0) {
            toPrint <- paste0(toPrint,domain,' Domain PARMCD: ',variable,' was populated in ',
                       variableCountList[[domainName]][[variable]],' of ',nStudyValidity[[domain]], ' Studies\n\n')
          } else {
            toPrint <- paste0(toPrint,domain,' Domain Variable: ',variable,' was populated in ',
                       variableCountList[[domainName]][[variable]],' of ',nStudyValidity[[domain]], ' Studies\n\n')
          }
          # toPrint <- paste0(toPrint,'and used the following units (capped at ',cap,'):\n\n')
          print(domainName)
          print(variable)
          print(head(variableUnitSummary[[domainName]][[variable]],n = cap),row.names=F)
          # cat('\n')
        }
        else {
          if (length(grep('PARMCD',domainName))>0) {
            paste0(toPrint,domain,' Domain PARMCD: ',variable,' was populated in ',
                       '0',' of ',nStudyValidity[[domain]], ' Studies\n\n')
          } else {
            paste0(toPrint,domain,' Domain Variable: ',variable,' was populated in ',
                       '0',' of ',nStudyValidity[[domain]], ' Studies\n\n')
          }
        }
      } 
      toPrint <- paste0(toPrint,'\n')
    }
    return(toPrint)
  })
  
  output$printResults <- renderText({
    input$run
    isolate({
      results <- prepareResults()
      results
    })
  })
  
}

shinyApp(ui = ui, server = server)