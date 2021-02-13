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
library(sendigR)


# define the path to R scripts to actual script location
dummyuseCaseQuestionMiFindings<-function() {
  # dummy function only to be used to get this script's location
}
setwd(getSrcDirectory(dummyuseCaseQuestionMiFindings))

# Include the SEND functions (to be changed to inclusion of SEND package)
#source("initSENDFunctions.R")



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

#reactive value
values <- reactiveValues()

values$selected_routes <- NULL

# 
# mi_col_names <- c('STUDYID','USUBJID','DOMAIN','MISEQ','MITESTCD','MITEST',
# 'MIORRES','MISTRESC','MIRESCAT','MISPEC','MISPCCND','MISPCUFL','MISEV',
# 'MIDTHREL','MIDTC','MIDY','MIGRPID','MIREFID','MISPID','MIBODSYS','MISTAT',
# 'MIREASND','MINAM','MIANTREG','MIMETHOD','MILAT','MIDIR','MIEVAL','MICHRON','MIDISTR',
# 'ROUTE','SPECIES','STRAIN','SEX','TCNTRL','SDESIGN','STSTDTC')

#order of column in MI individual records table to match with excel file or sendig
mi_col_names <- c('STUDYID','DOMAIN','USUBJID','MISEQ','MIGRPID','MIREFID','MISPID','MITESTCD','MITEST','MIBODSYS',
                  'MIORRES','MISTRESC','MIRESCAT','MICHRON','MIDISTR','MISTAT','MIREASND','MINAM','MISPEC','MIANTREG',
                  'MISPCCND','MISPCUFL','MILAT','MIDIR','MIMETHOD','MIEVAL','MISEV','MIDTHREL','MIDTC','MIDY','SEX',
                  'ROUTE','TCNTRL','SPECIES','STRAIN','SDESIGN','STSTDTC')

sub_mi_col_names <- c('STUDYID','DOMAIN','USUBJID','MISEQ','MIGRPID','MIREFID','MISPID','MITESTCD','MITEST','MIBODSYS',
                  'MIORRES','MIRESCAT','MISTAT','MIREASND','MINAM','MISPEC','MIANTREG',
                  'MISPCCND','MISPCUFL','MILAT','MIDIR','MIMETHOD','MIEVAL','MISEV','MIDTHREL','MIDTC','MIDY')
# list of column that by default selected in MI individual records table
mi_col_names_selected <- c('STUDYID','USUBJID','MIBODSYS', 'MISTRESC','MIRESCAT','MICHRON','MIDISTR','MISPEC',
                           'MISEV','MIDTC','MIDY','SEX','ROUTE','TCNTRL','SPECIES','STRAIN')

lb_col_names_selected <- c('STUDYID','USUBJID','LBTEST','LBTESTCD','LBORRES','LBORRESU',
                           'LBSTRESC','LBSTRESN','LBSTRESU','LBSPEC')
cl_col_names_selected <- c('STUDYID','USUBJID','CLTESTCD','CLTEST','CLCAT','CLORRES','CLSTRESC','CLRESCAT')
bw_col_names_selected <- c('STUDYID','USUBJID','BWTEST','BWSTRESN','BWSTRESU','VISITDY')



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
               
               
               # addUIDep(selectizeInput("ROUTE",label='Select Route of Administration:',
               #                         choices=GetUniqueRoutes(),
               #                         selected=values$selected_routes,
               #                         multiple=TRUE,
               #                         options=list(plugins=list('drag_drop','remove_button')))),
               
               uiOutput('ROUTE'),
               actionButton('clear_route',label='Clear All'),
               actionButton('select_route',label='Display All'),
               
               
               
               
               addUIDep(selectizeInput("SPECIES",label='Select Species:',
                                       choices= GetUniqueSpecies(),
                                       selected='RAT',
                                       multiple=TRUE,
                                       options=list(plugins=list('drag_drop','remove_button')))),
               
            
               
               # Strain should change based
               # on current SPECIES .  This is 
               # implemented server side. 
               addUIDep(selectizeInput("STRAIN",
                           "Select Strain:",
                           '', 
                           selected='WISTAR HAN',
                           multiple=TRUE,
                           options=list(plugins=list('drag_drop','remove_button')))),
    
               selectInput("SEX",
                           "Select Sex:",
                           c('', as.character(availableSex)), 
                           selected='M'),
               
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
    
    # left side scroller
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
    # fluidRow(
    #   column(12, 
             # tabBox(
             #   title = "Findings domains", width=NULL,
             #   # The id lets us use input$tabset1 on the server to find the current tab
             #   id = "findingsTab",
    tabsetPanel(type = 'tab',
               tabPanel('ANIMALS',
                        fluidRow(title = "Filtered control animals",
                                 br(),
                                     DT::dataTableOutput("animals"))),
               tabPanel("MI", # MI ----
                        tabsetPanel(
                          tabPanel("MI Findings",
                                   fluidRow(
                                     br(),
                                     column(width = 3,
                                            selectInput("MISPEC",
                                                        "Select MISPEC:",
                                                        availableOrgans,
                                                        selected='KIDNEY')),
                                     column(width = 6, offset = 1,
                                            DT::dataTableOutput("findingsTable")))),     
                          
                          tabPanel("Individual Records",
                                  checkboxInput('hide_check_column', label = 'Show Only Table', 
                                                value = 0),
                                  br(),
                                     uiOutput('mi_indiv_table')),
    
                          tabPanel("Aggregate Table",
                                   DT::dataTableOutput('mi_agg_tab')))),
               
               tabPanel("LB", #LB ----
                        tabsetPanel(
                          tabPanel("LB Findings",
                                   fluidRow(
                                     br(),
                                     column(width = 2,
                                            selectInput("LBTESTCD",
                                                        "Select LBTESTCD:",
                                                        availableLBTESTCD)),
                                     column(width = 2,
                                            radioButtons("dist", "Distribution type:",
                                                         c("Normal" = "norm",
                                                           "Log-normal" = "lnorm"))),
                                     column(width = 7,offset = 1,
                                            plotOutput("labTestHist")))),
                          tabPanel("Individual Records",
                                   checkboxInput('lb_hide_check_column', label = 'Show Only Table', 
                                                 value = 0),
                                   br(),
                                   uiOutput('lb_indiv_table')),
                          
                          tabPanel("Aggregate Table",
                                   DT::dataTableOutput('lb_agg_tab'))
                          
                        )
                        ),
               tabPanel("CL",
                        tabsetPanel(
                          tabPanel("Individual Records",
                                   checkboxInput("cl_hide_check_column", label = "Show Only Table",
                                                 value = 0),
                                   br(),
                                   uiOutput("cl_indiv_table")),
                          tabPanel("Aggregate Table",
                                   DT::dataTableOutput('cl_agg_tab')))),
               tabPanel("BW",
                        tabsetPanel(
                          tabPanel("Individual Records",
                                   checkboxInput("bw_hide_check_column", label = "Show Only Table",
                                                 value = 0),
                                   br(),
                                   uiOutput("bw_indiv_table")),
                          tabPanel("Aggregate Table",
                                   DT::dataTableOutput('bw_agg_tab'))))
               
               )))



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
  
  
  # Clear All Functionality
  observeEvent(ignoreNULL=TRUE,eventExpr=input$clear_route,
               handlerExpr={values$selected_routes <- NULL})
  
 
  
  # Display All Functionality
  observeEvent(ignoreNULL=TRUE,eventExpr=input$select_route,
               handlerExpr={values$selected_routes <- GetUniqueRoutes()})
  
  
  
  
  
  # Get the list of studies and animals based on new/changed filter criterion
  animalList<-eventReactive(input$refreshData, {
    
    # print(c(as.character(input$STSTDTC[1]), 
    #       as.character(input$STSTDTC[2]),
    #       input$SDESIGN,
    #       input$ROUTE,
    #       input$SPECIES,
    #       input$STRAIN,
    #       input$SEX,
    #       input$INCL_UNCERTAIN))
    
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
  
  
  
  output$ROUTE <- renderUI({
    
    addUIDep(selectizeInput("ROUTE",label='Select Route of Administration:',
                            choices=GetUniqueRoutes(),
                            selected=values$selected_routes,
                            multiple=TRUE,
                            options=list(plugins=list('drag_drop','remove_button'))))
    
  })
  
  # Control Animal Table ----
  
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
  
  #### MI findings table UI ---- 
  
  output$findingsTable <- DT::renderDataTable({
    
    req(input$STRAIN)
    findings <- MiFindings(animalList(), input$MISPEC)
    findings <- findings %>% mutate_if(is.character,as.factor)
    
    findings <- DT::datatable(findings,
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
  
  
  #### get MI individual records table ----
  MI_subject <- reactive({
    animal_list <- animalList()
    mi_sub <- sendigR::getSubjData(dbToken = dbToken, domain = 'mi',
                                   animalList =  animal_list,
                                   colList = sub_mi_col_names)
    mi_sub
  })
 
  # call MI subject 
  # merge with conf/non conf reocrds 
  
  MI_column <- reactive({
    if (nrow(MI_subject())>0) {
      get_col_name <- colnames(MI_subject())
      order_to_match <- get_col_name[order(match(get_col_name, mi_col_names))]
    } else{
      order_to_match <- mi_col_names
    }
    order_to_match
  })
  
  table_to_show <- reactive({
    tabl <- MI_subject()
    tabl <- subset(tabl, select=input$filter_column)
    tabl
  })
  
  selected_column_to_show <- reactive({
    col_selected <- intersect(mi_col_names_selected,MI_column())
    col_selected
    
  })
  #### MI individual record table UI with hide/show side column ----
  
  output$mi_indiv_table <- renderUI({
    
    if (input$hide_check_column==0) {
      fluidRow(
        br(),
        column(width = 1,
               checkboxGroupInput(inputId = 'filter_column', label = "Display Column",
                                  choices = MI_column(),
                                  selected = selected_column_to_show())),
        column(width = 11,
               DT::dataTableOutput('mi_subj')))
    } else {
      fluidRow(
        br(),
        column(width = 12,
               DT::dataTableOutput('mi_subj')))
       }
  })
  
  #### update selected column in MI individual table
  observeEvent(input$hide_check_column,{
    updateCheckboxGroupInput(session = session, inputId = 'filter_column',
                             selected = input$filter_column)
  })
  
  
  #### output rendertable for MI individual table
  output$mi_subj <- DT::renderDataTable({
    
    tab <- DT::datatable(table_to_show(),
                         filter = list(position = 'top'),
                         options = list(
                           dom = "lfrtipB",
                           buttons = c("csv", "excel", "pdf"),
                           #colReorder = TRUE,
                           scrollY = TRUE,
                           scrollX=TRUE,
                           pageLength = 10,
                           #columnDefs = list(list(className = "dt-center", targets = "_all")),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                             "}")))
    
    tab
      
  })
  
 #### MI aggregate table

  
  ########################### LB TAB #######################################
  
  #### get LB individual records table ----
  LB_subject <- reactive({
    animal_list <- animalList()
    lb_sub <- sendigR::getSubjData(dbToken = dbToken, domain = 'lb',
                                   animalList =  animal_list)
    lb_sub
  })
  

  
  LB_column <- reactive({
    if (nrow(LB_subject())>0) {
      get_col_name <- colnames(LB_subject())
      
    } 
    get_col_name
  })
  
  lb_table_to_show <- reactive({
    tabl <- LB_subject()
    tabl <- subset(tabl, select=input$lb_filter_column)
    tabl
  })
  
  
  lb_selected_column_to_show <- reactive({
    col_selected <- intersect(lb_col_names_selected,LB_column())
    col_selected
    
  })
  #### MI individual record table UI with hide/show side column ----
  
  output$lb_indiv_table <- renderUI({
    
    if (input$lb_hide_check_column==0) {
      fluidRow(
        br(),
        column(width = 1,
               checkboxGroupInput(inputId = 'lb_filter_column', label = "Display Column",
                                  choices = LB_column(),
                                  selected = lb_selected_column_to_show())),
        column(width = 11,
               DT::dataTableOutput('lb_subj')))
    } else {
      fluidRow(
        br(),
        column(width = 12,
               DT::dataTableOutput('lb_subj')))
    }
  })
  
  #### update selected column in LB individual table
  observeEvent(input$lb_hide_check_column,{
    updateCheckboxGroupInput(session = session, inputId = 'lb_filter_column',
                             selected = input$lb_filter_column)
  })
  
  
  #### output rendertable for LB individual table
  output$lb_subj <- DT::renderDataTable({
  
    tab <- DT::datatable(lb_table_to_show(),
                         filter = list(position = 'top'),
                         options = list(
                           dom = "lfrtipB",
                           buttons = c("csv", "excel", "pdf"),
                           #colReorder = TRUE,
                           scrollY = TRUE,
                           scrollX=TRUE,
                           pageLength = 10,
                           #columnDefs = list(list(className = "dt-center", targets = "_all")),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                             "}")))
    
    tab
    
  })
  
  #### MI aggregate table
  
  
  
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
 
  
  ################### CL TAB ########################
  
  #### get CL individual records table ----
  CL_subject <- reactive({
    animal_list <- animalList()
    cl_sub <- sendigR::getSubjData(dbToken = dbToken, domain = 'cl',
                                   animalList =  animal_list)
    cl_sub
  })
  
  
  
  CL_column <- reactive({
    if (nrow(CL_subject())>0) {
      get_col_name <- colnames(CL_subject())
      
    } 
    get_col_name
  })
  
  cl_table_to_show <- reactive({
    tabl <- CL_subject()
    tabl <- subset(tabl, select=input$cl_filter_column)
    tabl
  })
  
  
  cl_selected_column_to_show <- reactive({
    col_selected <- intersect(cl_col_names_selected,CL_column())
    col_selected
    
  })
  #### MI individual record table UI with hide/show side column ----
  
  output$cl_indiv_table <- renderUI({
    
    if (input$cl_hide_check_column==0) {
      fluidRow(
        br(),
        column(width = 1,
               checkboxGroupInput(inputId = 'cl_filter_column', label = "Display Column",
                                  choices = CL_column(),
                                  selected = cl_selected_column_to_show())),
        column(width = 11,
               DT::dataTableOutput('cl_subj')))
    } else {
      fluidRow(
        br(),
        column(width = 12,
               DT::dataTableOutput('cl_subj')))
    }
  })
  
  #### update selected column in cl individual table
  observeEvent(input$cl_hide_check_column,{
    updateCheckboxGroupInput(session = session, inputId = 'cl_filter_column',
                             selected = input$cl_filter_column)
  })
  
  
  #### output rendertable for cl individual table
  output$cl_subj <- DT::renderDataTable({
    
    tab <- DT::datatable(cl_table_to_show(),
                         filter = list(position = 'top'),
                         options = list(
                           dom = "lfrtipB",
                           buttons = c("csv", "excel", "pdf"),
                           #colReorder = TRUE,
                           scrollY = TRUE,
                           scrollX=TRUE,
                           pageLength = 10,
                           #columnDefs = list(list(className = "dt-center", targets = "_all")),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                             "}")))
    
    tab
    
  })
  
  #### MI aggregate table
  
  
  #################### BW Tab ###############################
  
  output$bodyWeightTime <- renderPlot({
    
    animalList <- GetAnimalList(input$SDESIGN, input$SPECIES)
    bw <- BodyWeight(animalList)
    
    
    bw %>%
      group_by(USUBJID) %>%
      ggplot( aes(x=days, y=BWSTRESN,  color='black')) +
      geom_line()
    
  })
  
  
  #### get BW individual records table ----
  BW_subject <- reactive({
    animal_list <- animalList()
    bw_sub <- sendigR::getSubjData(dbToken = dbToken, domain = 'bw',
                                   animalList =  animal_list)
    bw_sub
  })
  
  
  
  BW_column <- reactive({
    if (nrow(BW_subject())>0) {
      get_col_name <- colnames(BW_subject())
      
    } 
    get_col_name
  })
  
  bw_table_to_show <- reactive({
    tabl <- BW_subject()
    tabl <- subset(tabl, select=input$bw_filter_column)
    tabl
  })
  
  
  bw_selected_column_to_show <- reactive({
    col_selected <- intersect(bw_col_names_selected,BW_column())
    col_selected
    
  })
  #### MI individual record table UI with hide/show side column ----
  
  output$bw_indiv_table <- renderUI({
    
    if (input$bw_hide_check_column==0) {
      fluidRow(
        br(),
        column(width = 1,
               checkboxGroupInput(inputId = 'bw_filter_column', label = "Display Column",
                                  choices = BW_column(),
                                  selected = bw_selected_column_to_show())),
        column(width = 11,
               DT::dataTableOutput('bw_subj')))
    } else {
      fluidRow(
        br(),
        column(width = 12,
               DT::dataTableOutput('bw_subj')))
    }
  })
  
  #### update selected column in bw individual table
  observeEvent(input$bw_hide_check_column,{
    updateCheckboxGroupInput(session = session, inputId = 'bw_filter_column',
                             selected = input$bw_filter_column)
  })
  
  
  #### output rendertable for bw individual table
  output$bw_subj <- DT::renderDataTable({
    
    tab <- DT::datatable(bw_table_to_show(),
                         filter = list(position = 'top'),
                         options = list(
                           dom = "lfrtipB",
                           buttons = c("csv", "excel", "pdf"),
                           #colReorder = TRUE,
                           scrollY = TRUE,
                           scrollX=TRUE,
                           pageLength = 10,
                           #columnDefs = list(list(className = "dt-center", targets = "_all")),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                             "}")))
    
    tab
    
  })
  
  #### BW aggregate table
  
  
  
  
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
    #print(xTest)
    yTest <- LiverFindings(speciesControls1, input$y_axis_LBTESTCD)
    #print(yTest)
    data  <- unique(merge(xTest, yTest, by='USUBJID'))
    
    xAvg <- mean(data$LBSTRESC_TRANS.x)
    yAvg <- mean(data$LBSTRESC_TRANS.y)
    
    data$LBSTRESC_TRANS.x <- data$LBSTRESC_TRANS.x / xAvg
    data$LBSTRESC_TRANS.y <- data$LBSTRESC_TRANS.y / yAvg
    
    thisStudyAnimals <- GetAnimalGroupsStudy(input$STUDYID)
    
    studyx <- LiverFindings(thisStudyAnimals, input$x_axis_LBTESTCD)
    #print(studyx)
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
    sendigR::disconnectDB(dbToken)
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)

