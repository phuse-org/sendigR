# Shiny app for visualizing 
# historical control data from 
# SEND data loaded into a 
# SQLite database. 

# This script, app.R contains 
# all the shiny code to run 
# the front end/GUI, but relies
# on sendDB.R for many backend
# operations.  


library(shiny)
library(shinydashboard)
library(tidyverse)
library(MASS)
library(htmltools)
library(DT)


# define the path to R scripts to actual script location
dummyuseCaseQuestionMiFindings<-function() {
  # dummy function only to be used to get this script's location
}
setwd(getSrcDirectory(dummyuseCaseQuestionMiFindings))

# Include the SEND functions (to be changed to inclusion of SEND package)
source("initSENDFunctions.R")


source('sendDB.R')
source('controlFiltering.R')

# These functions come from
# sendDB.R and query the database
# for unique values to populate 
# the available selections in the
# drop down bars, inputs, etc.  



#### Animal filtering selections ####

minStudyStartDate <- as.Date(getMinStudyStartDate())

availableStudies <- GetAvailableStudies()
availableStudies <- as.list(setNames(availableStudies, availableStudies))

availableSex <- c('M', 'F', 'U', 'All')

availablePhases <- c('Screening', 'Treatment', 'Recovery')


#### Domains-specific filtering selections ####

# For the MI domain, allow filtering 
# by organs available in the SEND DB.  

availableOrgans <- GetUniqueOrgans()
availableOrgans <- as.list(setNames(availableOrgans, availableOrgans))

# The LB domain is of the larger
# domains.  This is currently 
# converting all LBTESTCD to a 
# singular unit to show distributions.
# TODO: Find a better way to do this
# without having to menaully enter
# unit conversions.  

availableLBTESTCD <- GetUniqueLBTESTCD('CLINICAL CHEMISTRY')
availableLBTESTCD <- as.list(setNames(availableLBTESTCD, availableLBTESTCD))

liverEnzymes <- list(
  ALT='ALT',
  BILI='BILI',
  AST='AST',
  ALP='ALP'
)

# add function drag and drop menu ----
addUIDep <- function(x) {
  jqueryUIDep <- htmlDependency("jqueryui", "1.10.4", c(href="shared/jqueryui/1.10.4"),
                                script = "jquery-ui.min.js",
                                stylesheet = "jquery-ui.min.css")
  attachDependencies(x, c(htmlDependencies(x), list(jqueryUIDep)))
}




#### UI ####


# The basic input is 
# the standard sidebar 
# dashboard layout. 
# User has the option
# filter control animals 
# based on a set of inputs, 
# or can be extracted from
# a study ID.


ui <- dashboardPage(
  dashboardHeader(title = "Historical Control Collator", titleWidth = 300),
  dashboardSidebar(width = 300,
    sidebarMenu(
      menuItem("Choose Parameters:", icon = icon("paw"), startExpanded = TRUE,
               
               # sliderInput("STSTDTC",
               #             "Select study start date range:",
               #             min = minStudyStartDate,
               #             max = Sys.Date(),
               #             value=c(minStudyStartDate, Sys.Date()),
               #             timeFormat="%Y-%m", 
               #             dragRange=TRUE),
               
               dateRangeInput("STSTDTC",
                              "Select Study Start Date Range:",
                              start = minStudyStartDate,
                              end = Sys.Date(),
                              min = minStudyStartDate,
                              max = Sys.Date(),
                              format="yyyy-mm-dd",
                              startview = "year"),
               
               selectInput("SDESIGN",
                           "Select Study Design:",
                           GetUniqueDesign(), 
                           selected='PARALLEL'),
               
               # selectInput("ROUTE",
               #             "Select route of administration:",
               #             GetUniqueRoutes(),
               #             selected = '', 
               #             multiple=TRUE),
               
               addUIDep(selectizeInput("ROUTE",label='Select Route of Administration:',
                                       choices=GetUniqueRoutes(),
                                       selected='',
                                       multiple=TRUE,
                                       options=list(plugins=list('drag_drop','remove_button')))),
               
               addUIDep(selectizeInput("SPECIES",label='Select Species:',
                                       choices= GetUniqueSpecies(),
                                       selected='',
                                       multiple=TRUE,
                                       options=list(plugins=list('drag_drop','remove_button')))),
               
            
               
               # Strain should change based
               # on current SPECIES .  This is 
               # implemented server side. 
               addUIDep(selectizeInput("STRAIN",
                           "Select Strain:",
                           '', 
                           selected='',
                           multiple=TRUE,
                           options=list(plugins=list('drag_drop','remove_button')))),
               
               
               
               
               
               selectInput("SEX",
                           "Select Sex:",
                           c('', as.character(availableSex)), 
                           selected=''),
               
               # TODO: Get phase of study working.
               # 
               #               selectInput("PHASE",
               #                           "Select phase of study:",
               #                           availablePhases)
               
               
               checkboxInput('INCL_UNCERTAIN',
                             'Include uncertain rows', 
                             value = FALSE),
               
               actionButton("refreshData", "Generate/Update Data"),
               br()
           
      ),
      
      # TODO: implement function 
      # to take STUDYID, read study 
      # study parameters and populate
      # inputs.
      
      menuItem("By Study:", icon = icon("flask"),
               selectInput("STUDYID",
                           "Select STUDYID:", 
                           choices=availableStudies)
      )
    ), 
    tags$head(
      tags$style(
        HTML(".sidebar {height: 94vh; overflow-y: auto;}")
      )
    )
    # TODO: Get animal age working.
    # selectInput("AGE", 
    #             "Input animal age:", 
    #             'e.g., 2-4 M'),
    
  ),
  
  # Main body of the app. Consists
  # of different of different tabs
  # each displaying a domain and 
  # analysis/analyses relevant to
  # that particular domain.  
  
  dashboardBody(  
    fluidRow(
      column(12, 
             tabBox(
               title = "Findings domains", width=NULL,
               # The id lets us use input$tabset1 on the server to find the current tab
               id = "findingsTab",
               tabPanel('ANIMALS',
                        fluidRow(title = "Filtered control animals",
                                     DT::dataTableOutput("animals"))),
               tabPanel("MI",
                        fluidRow(
                          column(width = 3,
                          selectInput("MISPEC",
                                          "Select MISPEC:",
                                          availableOrgans,
                                          selected='KIDNEY')),
                          column(width = 6, offset = 1,
                                 DT::dataTableOutput("findingsTable")))),
               tabPanel("LB", fluidRow(box(selectInput("LBTESTCD",
                                                       "Select LBTESTCD:",
                                                       availableLBTESTCD),
                                           radioButtons("dist", "Distribution type:",
                                                        c("Normal" = "norm",
                                                          "Log-normal" = "lnorm"))
               ),
               box(title = "LBTESTCD", plotOutput("labTestHist"))) )
             )
      )
    )
  )
)



#### Server ####

# Server/backend logic for the app. 
# main function is a reactive 
# called animalList that generates
# a new set of control animals 
# anytime the animal input UI 
# changes.  It calls the function
# GetFilteredControlAnimals defined
# in controlFiltering.R. 

server <- function(input, output, session) {
  
  # This is the logic for changing 
  # the STRAIN based ON changes SPECIES
  observeEvent(input$SPECIES, {
    if (length(input$SPECIES) != 0) {
      updateSelectInput(session, "STRAIN", 
                        choices = GetUniqueStrains(input$SPECIES))
    }
    else {
      updateSelectInput(session, "STRAIN", 
                        choices ='')
    }
  }, ignoreNULL = FALSE)
  
  # Get the list of studies and animals based on new/changed filter criterion
  animalList<-eventReactive(input$refreshData, {
    
    GetFilteredControlAnimals(as.character(input$STSTDTC[1]), 
                              as.character(input$STSTDTC[2]),
                              input$SDESIGN,
                              input$ROUTE,
                              input$SPECIES,
                              input$STRAIN,
                              input$SEX,
                              input$INCL_UNCERTAIN) 
  })
  
  # findings <- reactive({ MiFindings(animalList(), input$MISPEC) })
  
  output$animals <- DT::renderDataTable({
    
    animal_df <- animalList()
    # make last column as date
    
    
    #convert character to factor to make filter work
    animal_df <- animal_df %>% mutate_if(is.character,as.factor)
   
    # cols <- c('STUDYID','USUBJID','ROUTE','SPECIES','STRAIN','SEX','TCNTRL','SDESIGN')

    animal_df <- DT::datatable(
      animal_df,
      class = "cell-border stripe",
      filter = list(position = 'top'),
      extensions = list("Buttons" = NULL,
                        "ColReorder" = NULL),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center; font-size: 20px; color: black",
        "Table :", htmltools::strong("Filtered Control Animal")
      ),
      options = list(
        dom = "lfrtipB",
        buttons = c("csv", "excel", "copy", "pdf"),
        colReorder = TRUE,
        scrollY = TRUE,
        pageLength = 10,
        columnDefs = list(list(className = "dt-center", targets = "_all")),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}")
        ))
    animal_df
    })
  
  ########################### MI TAB #######################################
  
  # MI is basic.  Right
  # now just displays a 
  # table of findings for 
  # a given organ. 
  # Use DataTable to display
  # dataframe as a dynamic
  # sortable table. Relies on
  # the function called MiFindings()
  # defined in sendDB.R
  
  output$findingsTable <- DT::renderDataTable({
    
    req(input$STRAIN)
    findings <- MiFindings(animalList(), input$MISPEC)
    
    findings <- findings %>% mutate_if(is.character,as.factor)
    
    findings <- DT::datatable(
      findings,
      class = "cell-border stripe",
      filter = list(position = 'top'),
      extensions = list("Buttons" = NULL,
                        "ColReorder" = NULL),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center; font-size: 20px; color: black",
        "Table :", htmltools::strong("Findings")
      ),
      options = list(
        dom = "lfrtipB",
        buttons = c("csv", "excel", "pdf"),
        colReorder = TRUE,
        scrollY = TRUE,
        pageLength = 10,
        columnDefs = list(list(className = "dt-left", targets = "_all")),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}")
      ))
    findings
    
  })
  
  
  # TODO: Implement function to download 
  # Findings in MI
  # output$ExportMI <- downloadHandler(
  #   filename = function() {
  #     paste(input$SDESIGN, '-' ,input$SPECIES, '-', input$MISPEC, ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     
  #     findingsToFile <- findings()
  #     
  #     findingsToFile$SDESIGN <- rep(input$SDESIGN, nrow(findingsToFile))
  #     findingsToFile$SPECIES <- rep(input$SPECIES, nrow(findingsToFile))
  #     findingsToFile$MISPEC <- rep(input$MISPEC, nrow(findingsToFile))
  #     
  #     print(findingsToFile)
  #     write.csv(findingsToFile, file, row.names = FALSE)
  #   }
  # )
  
  
  
  ########################### LB TAB #######################################
  
  # LB displays a histogram 
  # and probability density
  # density function given 
  # a particular LBTESTCD.  
  # Currently, only 4 Liver 
  # enzymes are implemented.
  # User has the option
  # of dsiplaying as normal
  # or log-normal distribution.
  
  output$labTestHist <- renderPlot({
    
    # LiverFindings() gets 
    # lab test results for 
    # a user-defined LBTESTCD
    
    labResults <- LiverFindings(animalList(), input$LBTESTCD)
    
    
    # change the distbution 
    # function depending on 
    # user input
    
    if (input$dist == 'norm') {
      labResults$distribution <- labResults$LBSTRESC_TRANS
      dist <- MASS::fitdistr(labResults$distribution, 'normal')
      fun <- dnorm
    } else if (input$dist == 'lnorm') {
      labResults$distribution <- log(labResults$LBSTRESC_TRANS + 1) 
      dist <- MASS::fitdistr(labResults$distribution, 'lognormal')
      fun <- dlnorm
    }
    
    # plot the probability 
    # distribution and 
    # the pdf
    
    ggplot(labResults) +  
      geom_histogram(aes(x = distribution, y = ..density..),
                     fill = "blue",  
                     colour = "grey", alpha=0.6) + 
      stat_function(fun = fun, 
                    args = list(mean = dist$estimate[1], sd = dist$estimate[2], log = F), 
                    color="grey", lwd=1, alpha=0.6)
    
  })
  
  
  # Two panels not yet
  # ready.  BW Tab simply
  # diplays the average 
  # bodyweight through 
  # study length.  eDISH
  # was meant to diplsay
  # user selected x and y 
  # axis as liver enzyme
  # responses. 
  
  #################### BW Tab ###############################
  
  output$bodyWeightTime <- renderPlot({
    
    animalList <- GetAnimalList(input$SDESIGN, input$SPECIES)
    bw <- BodyWeight(animalList)
    
    
    bw %>%
      group_by(USUBJID) %>%
      ggplot( aes(x=days, y=BWSTRESN,  color='black')) +
      geom_line()
    
  })
  
  ################## eDish #############################
  
  studyAnimalList <- reactive({
    studyInfo <- GetStudyTS(input$STUDYID)
    
    design <- toupper(studyInfo[studyInfo$TSPARMCD == 'SDESIGN',]$TSVAL)
    species <- toupper(studyInfo[studyInfo$TSPARMCD == 'SPECIES',]$TSVAL)
    
    GetAnimalList(design, species)
  })
  
  
  
  output$eDish <- renderPlot({
    
    
    speciesControls1 <- studyAnimalList()
    
    xTest <- LiverFindings(speciesControls1, input$x_axis_LBTESTCD)
    print(xTest)
    yTest <- LiverFindings(speciesControls1, input$y_axis_LBTESTCD)
    print(yTest)
    data  <- unique(merge(xTest, yTest, by='USUBJID'))
    
    xAvg <- mean(data$LBSTRESC_TRANS.x)
    yAvg <- mean(data$LBSTRESC_TRANS.y)
    
    data$LBSTRESC_TRANS.x <- data$LBSTRESC_TRANS.x / xAvg
    data$LBSTRESC_TRANS.y <- data$LBSTRESC_TRANS.y / yAvg
    
    thisStudyAnimals <- GetAnimalGroupsStudy(input$STUDYID)
    
    studyx <- LiverFindings(thisStudyAnimals, input$x_axis_LBTESTCD)
    print(studyx)
    studyy <- LiverFindings(thisStudyAnimals, input$y_axis_LBTESTCD)
    
    
    thisStudyAnimals <- merge(thisStudyAnimals, studyx, by='USUBJID')
    thisStudyAnimals <- unique(merge(thisStudyAnimals, studyy, by='USUBJID'))
    
    
    ggplot(data) +
      geom_point(aes(x=LBSTRESC_TRANS.x, y=LBSTRESC_TRANS.y), color=rgb(0, 0, 0, 0.4), size=4) +
      xlab(input$x_axis_LBTESTCD) + ylab(input$y_axis_LBTESTCD) + 
      geom_point(data=thisStudyAnimals, mapping=aes(x=LBSTRESC_TRANS.x / xAvg, 
                                                    y=LBSTRESC_TRANS.y / yAvg, 
                                                    color=SET), size=4)
    
  })
  
  # CLose connection to database at end of execution
  onSessionEnded(function() {
    disconnectDB()
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)

