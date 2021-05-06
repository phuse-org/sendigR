################################################################################
## Shiny app for visualizing historical control data from SEND data
## Contains all the shiny code to run the front end/GUI
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2021-04-28   Yousuf Ali,           Initial version
##              Daniel Russo
##              Bo Larsen
################################################################################


# 'Private' environment to keep a cache of control animals and retain some
# 'global'  variables inside the package scope
.sendigRenv <- new.env(parent = emptyenv())

#' Execute sendDashboard app
#'
#' Executes an encapsulated Shiny which to query, visualize and extract historical
#' control data from a SEND database.
#'
#' See ++ADD REFERENCE TO A VIGNETTE WITH DOC OF APP++ for further details
#'
#' @param dbToken Mandatory - token for the open database connection
#'
#' @return The function dores not return anything, but it is possible to extract
#' data from the app in different formats to use for further processing
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dbToken <- initEnvironment(dbType='sqlite', dbPath='/path/to/database/send.db')
#' execSendDashboard(dbToken)
#' disconnectDB(dbToken)
#' }
execSendDashboard <- function(dbToken) {

  # Clear environment
  rm(list = ls(envir = .sendigRenv,
               pattern = 'controlAnimals|lastFilterValues|studiesAll'),
     envir = .sendigRenv)
  # retain dbToken
  assign('dbToken', dbToken, envir = .sendigRenv)

  #### Animal filtering selections ####

  minStudyStartDate <- as.Date(getMinStudyStartDate())

  availableStudies <- GetAvailableStudies()
  availableStudies <- as.list(stats::setNames(availableStudies, availableStudies))

  #availableSex <- as.list(stats::setNames(c('M', 'F', 'U', 'All'), c('M', 'F', 'U', 'All')))
  availableSex <- GetUniqueSex()
  availableSex <- availableSex[[1]]
  availableSex <- c(availableSex, "All")
  availableSex <- as.list(stats::setNames(availableSex,availableSex))

  availablePhases <- c('Screening', 'Treatment', 'Recovery')


  #### Domains-specific filtering selections ####

  # For the MI domain, allow filtering
  # by organs available in the SEND DB.

  availableOrgans <- GetUniqueOrgans()
  availableOrgans <- as.list(stats::setNames(availableOrgans, availableOrgans))

  # The LB domain is of the larger
  # domains.  This is currently
  # converting all LBTESTCD to a
  # singular unit to show distributions.
  # TODO: Find a better way to do this
  # without having to manually enter
  # unit conversions.

  availableLBTESTCD <- GetUniqueLBTESTCD('CLINICAL CHEMISTRY')
  availableLBTESTCD <- as.list(stats::setNames(availableLBTESTCD, availableLBTESTCD))

  liverEnzymes <- list(
    ALT='ALT',
    BILI='BILI',
    AST='AST',
    ALP='ALP'
  )

  # add function drag and drop menu ----
  addUIDep <- function(x) {
    jqueryUIDep <- htmltools::htmlDependency("jqueryui", "1.10.4", c(href="shared/jqueryui/1.10.4"),
                                  script = "jquery-ui.min.js",
                                  stylesheet = "jquery-ui.min.css")
    htmltools::attachDependencies(x, c(htmltools::htmlDependencies(x), list(jqueryUIDep)))
  }

  #reactive value ----
  values <- shiny::reactiveValues()

  values$selected_routes <- NULL

  #
  # mi_col_names <- c('STUDYID','USUBJID','DOMAIN','MISEQ','MITESTCD','MITEST',
  # 'MIORRES','MISTRESC','MIRESCAT','MISPEC','MISPCCND','MISPCUFL','MISEV',
  # 'MIDTHREL','MIDTC','MIDY','MIGRPID','MIREFID','MISPID','MIBODSYS','MISTAT',
  # 'MIREASND','MINAM','MIANTREG','MIMETHOD','MILAT','MIDIR','MIEVAL','MICHRON','MIDISTR',
  # 'ROUTE','SPECIES','STRAIN','SEX','TCNTRL','SDESIGN','STSTDTC')

  #order of column in MI individual records table to match with excel file or sendig
  mi_col_names <- c('STUDYID','DOMAIN','USUBJID','MISEQ','MIGRPID','MIREFID',
                    'MISPID','MITESTCD','MITEST','MIBODSYS','MIORRES','MISTRESC',
                    'MIRESCAT','MICHRON','MIDISTR','MISTAT','MIREASND','MINAM',
                    'MISPEC','MIANTREG','MISPCCND','MISPCUFL','MILAT','MIDIR',
                    'MIMETHOD','MIEVAL','MISEV','MIDTHREL','MIDTC','MIDY','SEX',
                    'ROUTE','TCNTRL','SPECIES','STRAIN','SDESIGN','STSTDTC')

  # list of column that by default selected in MI individual records table
  mi_col_names_selected <- c('STUDYID','USUBJID','MIBODSYS', 'MISTRESC','MIRESCAT',
                             'MICHRON','MIDISTR','MISPEC','MISEV','MIDTC','MIDY',
                             'SEX','ROUTE','TCNTRL','SPECIES','STRAIN')

  lb_col_names_selected <- c('STUDYID','USUBJID','LBTEST','LBTESTCD','LBORRES',
                             'LBORRESU','LBSTRESC','LBSTRESN','LBSTRESU','LBSPEC')
  cl_col_names_selected <- c('STUDYID','USUBJID','CLTESTCD','CLTEST','CLCAT',
                             'CLORRES','CLSTRESC','CLRESCAT')
  bw_col_names_selected <- c('STUDYID','USUBJID','BWTEST','BWSTRESN'
                             ,'BWSTRESU','VISITDY')



########### UI #######


  # The basic input is
  # the standard sidebar
  # dashboard layout.
  # User has the option
  # filter control animals
  # based on a set of inputs,
  # or can be extracted from
  # a study ID.


  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Historical Control Collator",
                                    titleWidth = 300),
    ##### Sidebar ----
    shinydashboard::dashboardSidebar(width = 300,
                                     shinydashboard::sidebarMenu(
                                       shinydashboard::menuItem("Choose Parameters:",
                                                                icon = shiny::icon("paw"),
                                                                startExpanded = TRUE,

                 # sliderInput("STSTDTC",
                 #             "Select study start date range:",
                 #             min = minStudyStartDate,
                 #             max = Sys.Date(),
                 #             value=c(minStudyStartDate, Sys.Date()),
                 #             timeFormat="%Y-%m",
                 #             dragRange=TRUE),
##### Date Range, Design, Route, Species, Strain, Sex, Uncertain ----
                 shiny::dateRangeInput("STSTDTC",
                                "Select Study Start Date Range:",
                                start = minStudyStartDate,
                                end = Sys.Date(),
                                min = minStudyStartDate,
                                max = Sys.Date(),
                                format="yyyy-mm-dd",
                                startview = "year"),

                 shiny::selectInput("SDESIGN",
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

                 shiny::uiOutput('ROUTE'),
                 # shiny::actionButton('clear_route',label='Clear All'),
                 # shiny::actionButton('select_route',label='Display All'),




                 addUIDep(shiny::selectizeInput("SPECIES",label='Select Species:',
                                         choices= GetUniqueSpecies(),
                                         selected='RAT',
                                         multiple=TRUE,
                                         options=list(plugins=list('drag_drop','remove_button')))),



                 # Strain should change based
                 # on current SPECIES .  This is
                 # implemented server side.
                 addUIDep(shiny::selectizeInput("STRAIN",
                             "Select Strain:",
                             '',
                             selected='WISTAR HAN',
                             multiple=TRUE,
                             options=list(plugins=list('drag_drop','remove_button')))),

                 shiny::selectInput("SEX",
                             "Select Sex:",
                             availableSex,
                             selected='M'),

                 # TODO: Get phase of study working.
                 #
                 #               selectInput("PHASE",
                 #                           "Select phase of study:",
                 #                           availablePhases)


                 shiny::checkboxInput('INCL_UNCERTAIN',
                               'Include uncertain rows',
                               value = FALSE),

                 shiny::actionButton("refreshData", "Generate/Update Data"),
                 htmltools::br()

        )

        # TODO: implement function
        # to take STUDYID, read study
        # study parameters and populate
        # inputs.

        # shinydashboard::menuItem("By Study:", icon = shiny::icon("flask"),
        #                          shiny::selectInput("STUDYID",
        #                      "Select STUDYID:",
        #                      choices=availableStudies)
        # )
      ),

      # left side scroller
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(".sidebar {height: 94vh; overflow-y: auto;}")
        )
      )
      # TODO: Get animal age working.
      # selectInput("AGE",
      #             "Input animal age:",
      #             'e.g., 2-4 M'),

    ),

##### Mainbody ----
    # Main body of the app. Consists
    # of different of different tabs
    # each displaying a domain and
    # analysis/analyses relevant to
    # that particular domain.

    shinydashboard::dashboardBody(
      # shiny::fluidRow(
      #   column(12,
               # tabBox(
               #   title = "Findings domains", width=NULL,
               #   # The id lets us use input$tabset1 on the server to find the current tab
               #   id = "findingsTab",
      shiny::tabsetPanel(type = 'tab',
                 shiny::tabPanel('ANIMALS', ##### Animal Tab ----
                          shiny::fluidRow(title = "Filtered control animals",
                                   htmltools::br(),
                                       DT::dataTableOutput("animals"),
                                   htmltools::br(),
                                   htmltools::br(),
                                   htmltools::br(),
                                   htmltools::br()
                                   )),
                 shiny::tabPanel("MI", ##### MI ----
                          shiny::tabsetPanel(
                            shiny::tabPanel("MI Findings",
                                     shiny::fluidRow(
                                       htmltools::br(),
                                       shiny::column(width = 3,
                                              shiny::selectInput("MISPEC",
                                                          "Select MISPEC:",
                                                          availableOrgans,
                                                          selected='KIDNEY')),
                                       shiny::column(width = 6, offset = 1,
                                              DT::dataTableOutput("findingsTable"),
                                              htmltools::br(),
                                              htmltools::br(),
                                              htmltools::br(),
                                              htmltools::br()))),

                            shiny::tabPanel("Individual Records",
                                            shiny::checkboxInput('hide_check_column',
                                                                 label = 'Show Only Table',
                                                                 value = 0),
                                    htmltools::br(),
                                    shiny::uiOutput('mi_indiv_table'),
                                    htmltools::br(),
                                    htmltools::br(),
                                    htmltools::br(),
                                    htmltools::br()),

                            shiny::tabPanel("Aggregate Table",
                                     DT::dataTableOutput('mi_agg_tab'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br()))),

                 shiny::tabPanel("LB", #####LB ----
                          shiny::tabsetPanel(
                            shiny::tabPanel("LB Findings",
                                     shiny::fluidRow(
                                       htmltools::br(),
                                       shiny::column(width = 2,
                                                     shiny::selectInput("LBTESTCD",
                                                          "Select LBTESTCD:",
                                                          availableLBTESTCD)),
                                       shiny::column(width = 2,
                                                     shiny::radioButtons("dist", "Distribution type:",
                                                           c("Normal" = "norm",
                                                             "Log-normal" = "lnorm"))),
                                       shiny::column(width = 7,offset = 1,
                                                     shiny::plotOutput("labTestHist")))),
                            shiny::tabPanel("Individual Records",
                                            shiny::checkboxInput('lb_hide_check_column',
                                                                 label = 'Show Only Table',
                                                                 value = 0),
                                     htmltools::br(),
                                     shiny::uiOutput('lb_indiv_table'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br()),

                            shiny::tabPanel("Aggregate Table",
                                     DT::dataTableOutput('lb_agg_tab'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br())

                          )
                          ),
                 shiny::tabPanel("CL", ##### CL ----
                          shiny::tabsetPanel(
                            shiny::tabPanel("Individual Records",
                                            shiny::checkboxInput("cl_hide_check_column",
                                                                 label = "Show Only Table",
                                                                 value = 0),
                                     htmltools::br(),
                                     shiny::uiOutput("cl_indiv_table"),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br()),
                            shiny::tabPanel("Aggregate Table",
                                     DT::dataTableOutput('cl_agg_tab'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br()))),
                 shiny::tabPanel("BW", ##### BW ----
                          shiny::tabsetPanel(
                            shiny::tabPanel("Individual Records",
                                            shiny::checkboxInput("bw_hide_check_column",
                                                                 label = "Show Only Table",
                                                                 value = 0),
                                     htmltools::br(),
                                     shiny::uiOutput("bw_indiv_table"),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br()),
                            shiny::tabPanel("Aggregate Table",
                                     DT::dataTableOutput('bw_agg_tab'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br(),
                                     htmltools::br())))

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
    shiny::observeEvent(input$SPECIES, {
      if (length(input$SPECIES) != 0) {
        shiny::updateSelectInput(session, "STRAIN",
                                 choices = GetUniqueStrains(input$SPECIES))
      }
      else {
        shiny::updateSelectInput(session, "STRAIN",
                          choices ='')
      }
    }, ignoreNULL = FALSE)


    # #Clear All Functionality
    # shiny::observeEvent(ignoreNULL=TRUE,eventExpr=input$clear_route,
    #              handlerExpr={values$selected_routes <- NULL})



    ## Display All Functionality
    # shiny::observeEvent(ignoreNULL=TRUE,eventExpr=input$select_route,
    #              handlerExpr={values$selected_routes <- GetUniqueRoutes()})




##### AnimalList ----
    # Get the list of studies and animals based on new/changed filter criterion 
    animalList<-shiny::eventReactive(input$refreshData, {

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



    output$ROUTE <- shiny::renderUI({

      addUIDep(shiny::selectizeInput("ROUTE",
                                     label='Select Route of Administration:',
                                     choices=GetUniqueRoutes(),
                                     selected=values$selected_routes,
                                     multiple=TRUE,
                                     options=list(plugins=list('drag_drop','remove_button'))))

    })

    # Control Animal Table ----

    output$animals <- DT::renderDataTable(server = F,{
      animal_df <- animalList()
      # make last column as date

      #convert character to factor to make filter work
      animal_df <- animal_df %>% dplyr::mutate_if(is.character,as.factor)

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
          #buttons = c("csv", "excel", "copy", "pdf"),
          buttons=list(list(
            extend = 'collection',
            buttons = list(list(extend='csv',
                                filename = 'Filtered Control Animal'),
                           list(extend='excel',
                                filename = 'Filtered Control Animal')
                           # list(extend='pdf',
                           #      pageSize = 'A4',
                           #      orientation = 'landscape',
                           #      filename= 'Filtered Control Animal')
                           ),
            text = 'Download'
          )),
          
          colReorder = TRUE,
          scrollY = TRUE,
          scrollX=TRUE,
          pageLength = 10,
          #columnDefs = list(list(className = "dt-center", targets = "_all")),
          initComplete = DT::JS(
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

    ###### MI findings table ----

    output$findingsTable <- DT::renderDataTable(server = F,{

      shiny::req(input$STRAIN)
      findings <- MiFindings(animalList(), input$MISPEC)
      findings <- findings %>% dplyr::mutate_if(is.character,as.factor)
      findings_name <- paste0("MI Findings_",input$MISPEC) 
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
          # buttons = c("csv", "excel", "pdf"),
          buttons=list(list(
            extend = 'collection',
            buttons = list(list(extend='csv',
                                filename = findings_name),
                           list(extend='excel',
                                filename = findings_name),
                           list(extend='pdf',
                                pageSize = 'A4',
                                orientation = 'landscape',
                                filename= findings_name)
            ),
            text = 'Download'
          )),
          colReorder = TRUE,
          scrollY = TRUE,
          pageLength = nrow(findings),
          #columnDefs = list(list(className = "dt-left", targets = "_all")),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")
        ))
      findings

    })


    ###### get MI individual records table ----
    MI_subject <- shiny::reactive({
      animal_list <- animalList()
      mi_sub <- sendigR::getSubjData(dbToken = .sendigRenv$dbToken,
                                     domain = 'mi',
                                     animalList =  animal_list)
      mi_sub
    })

    # call MI subject
    # merge with conf/non conf reocrds

    MI_column <- shiny::reactive({
      if (nrow(MI_subject())>0) {
        get_col_name <- colnames(MI_subject())
        order_to_match <- get_col_name[order(match(get_col_name, mi_col_names))]
      } else{
        order_to_match <- mi_col_names
      }
      order_to_match
    })

    table_to_show <- shiny::reactive({
      tabl <- MI_subject()
      tabl <- subset(tabl, select=input$filter_column)
      tabl
    })

    selected_column_to_show <- shiny::reactive({
      col_selected <- intersect(mi_col_names_selected,MI_column())
      col_selected

    })
    ###### MI individual record table UI with hide/show side column ----

    output$mi_indiv_table <- shiny::renderUI({

      if (input$hide_check_column==0) {
        shiny::fluidRow(
          htmltools::br(),
          shiny::column(width = 1,
                        shiny::checkboxGroupInput(inputId = 'filter_column',
                                                  label = "Display Column",
                                                  choices = MI_column(),
                                                  selected = selected_column_to_show())),
          shiny::column(width = 11,
                 DT::dataTableOutput('mi_subj')))
      } else {
        shiny::fluidRow(
          htmltools::br(),
          shiny::column(width = 12,
                 DT::dataTableOutput('mi_subj')))
         }
    })

    #### update selected column in MI individual table
    shiny::observeEvent(input$hide_check_column,{
      shiny::updateCheckboxGroupInput(session = session,
                                      inputId = 'filter_column',
                                      selected = input$filter_column)
    })


    ####### output datatable for MI individual table ----
    output$mi_subj <- DT::renderDataTable(server = F,{
      tab <- table_to_show()
      tab <- tab %>% dplyr::mutate_if(is.character,as.factor)

      tab <- DT::datatable(tab,
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtipB",
                             #buttons = c("csv", "excel", "pdf"),
                             buttons=list(list(
                               extend = 'collection',
                               buttons = list(list(extend='csv',
                                                   filename = 'MI Individual Record Table'),
                                              list(extend='excel',
                                                   filename = 'MI Individual Record Table')
                                              # list(extend='pdf',
                                              #      pageSize = 'A4',
                                              #      orientation = 'landscape',
                                              #      filename= 'MI Individual Table')
                                              ),
                               text = 'Download'
                             )),
                             #colReorder = TRUE,
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             #columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))

      tab

    })

   ###### MI aggregate table ----

    output$mi_agg_tab <- DT::renderDataTable(server = F,{

      animal_list <- animalList()
      mi_sub <- MI_subject()


      # TODO: The columns to display/aggregate
      # should be chosen by the user, however
      # I think the sendigR package always
      # return certain columns, e.g., EPOCH
      grpByCols <- c('SPECIES', 'STRAIN', 'ROUTE', 'SEX',
                     'MISPEC', 'MISTRESC')

      domainData <- merge(animal_list,
                          mi_sub,
                          on = c('STUDYID', 'USUBJID'),
                          allow.cartesian = TRUE)

      # normalize results by the number of
      # animals that have data in the MI domain
      numAnimalsMI <- nrow(unique(domainData[,c('USUBJID', 'STUDYID')]))

      domainData$MISPEC <- toupper(domainData$MISPEC)
      domainData$MISTRESC <- toupper(domainData$MISTRESC)

      # replace Null values with NORMAL
      domainData$MISTRESC[domainData$MISTRESC == ''] <- 'NORMAL'

      # TODO: Do we account for animals that do not have
      # MI (or maybe other domains?) for which there is
      # no record? I know sometimes if result is normal
      # they will not get recorded.  Maybe this could be
      # a flag to toggle.

      tableData <- aggDomain(domainData, grpByCols,
                             includeUncertain=input$INCL_UNCERTAIN)

      # number of animals with observations b MISPEC
      tissueCounts <- domainData %>%
                      dplyr::group_by(MISPEC) %>%
                      dplyr::summarise(Animals.In.MISPEC = dplyr::n_distinct(USUBJID))
      tableData <- merge(tableData, tissueCounts, on='MISPEC')

      tableData['%MISPEC'] <- tableData$N / tableData$Animals.In.MISPEC
      tableData['%MI'] <- tableData$N / numAnimalsMI


      tableData['%MISPEC'] <- sapply(tableData['%MISPEC'],
                                     function(x) scales::percent(x,
                                                                 big.mark = 1,
                                                                 accuracy = 0.2))
      tableData['%MI'] <- sapply(tableData['%MI'],
                                 function(x) scales::percent(x,
                                                             big.mark = 1,
                                                             accuracy = 0.2))


      tableData <- dplyr::select(tableData, -Animals.In.MISPEC)

      tab <- DT::datatable(tableData,
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtipB",
                             # buttons = c("csv", "excel", "pdf"),
                             
                             buttons=list(list(
                               extend = 'collection',
                               buttons = list(list(extend='csv',
                                                   filename = 'MI Aggregate Table'),
                                              list(extend='excel',
                                                   filename = 'MI Aggregate Table')),
                               text = 'Download')),
                             
                             #colReorder = TRUE,
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             #columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))

      tab

    })



    #### LB TAB #######################################

    ###### get LB individual records table ----
    LB_subject <- shiny::reactive({
      animal_list <- animalList()
      lb_sub <- sendigR::getSubjData(dbToken = .sendigRenv$dbToken, domain = 'lb',
                                     animalList =  animal_list)
      lb_sub
    })



    LB_column <- shiny::reactive({
      if (nrow(LB_subject())>0) {
        get_col_name <- colnames(LB_subject())

      }
      get_col_name
    })

    lb_table_to_show <-shiny::reactive({
      tabl <- LB_subject()
      tabl <- subset(tabl, select=input$lb_filter_column)
      tabl
    })


    lb_selected_column_to_show <- shiny::reactive({
      col_selected <- intersect(lb_col_names_selected,LB_column())
      col_selected

    })
    ###### LB individual record table UI with hide/show side column ----

    output$lb_indiv_table <- shiny::renderUI({

      if (input$lb_hide_check_column==0) {
        shiny::fluidRow(
          htmltools::br(),
          shiny::column(width = 1,
                 shiny::checkboxGroupInput(inputId = 'lb_filter_column',
                                           label = "Display Column",
                                           choices = LB_column(),
                                           selected = lb_selected_column_to_show())),
          shiny::column(width = 11,
                 DT::dataTableOutput('lb_subj')))
      } else {
        shiny::fluidRow(
          htmltools::br(),
          shiny::column(width = 12,
                 DT::dataTableOutput('lb_subj')))
      }
    })

    #### update selected column in LB individual table
    shiny::observeEvent(input$lb_hide_check_column, {
      shiny::updateCheckboxGroupInput(
        session = session,
        inputId = 'lb_filter_column',
        selected = input$lb_filter_column
      )
    })


    ###### output datatable for LB individual table ----
    output$lb_subj <- DT::renderDataTable(server = F,{
      tab <- lb_table_to_show()
      tab <- tab %>% dplyr::mutate_if(is.character, as.factor)

      tab <- DT::datatable(
        tab,
        filter = list(position = 'top'),
        options = list(
          dom = "lfrtipB",
          # buttons = c("csv", "excel", "pdf"),
          
          buttons=list(list(
            extend = 'collection',
            buttons = list(list(extend='csv',
                                filename = 'LB Individual Record  Table'),
                           list(extend='excel',
                                filename = 'LB Individual Record Table')),
            text = 'Download')),
          #colReorder = TRUE,
          scrollY = TRUE,
          scrollX = TRUE,
          pageLength = 25,
          #columnDefs = list(list(className = "dt-center", targets = "_all")),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        )
      )

      tab

    })

    # LB displays a histogram
    # and probability density
    # density function given
    # a particular LBTESTCD.
    # Currently, only 4 Liver
    # enzymes are implemented.
    # User has the option
    # of displaying as normal
    # or log-normal distribution.

    output$labTestHist <- shiny::renderPlot({

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
        fun <- stats::dnorm
      } else if (input$dist == 'lnorm') {
        labResults$distribution <- log(labResults$LBSTRESC_TRANS + 1)
        dist <- MASS::fitdistr(labResults$distribution, 'lognormal')
        fun <- stats::dlnorm
      }

      # plot the probability
      # distribution and
      # the pdf

      ggplot2::ggplot(labResults) +
        ggplot2::geom_histogram(ggplot2::aes(x = distribution, y = ..density..),
                       fill = "blue",
                       colour = "grey", alpha=0.6) +
        ggplot2::stat_function(fun = fun,
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
    
    ###### LB aggregate table ----
    
    output$lb_agg_tab <- DT::renderDataTable(server = F,{
      
      animal_list <- animalList()
      lb_sub <- LB_subject()
      
     
      # 
      # df <- sendigR::getFindingsSubjAge(dbToken,findings=lb_sub,animalList = animal_list,
      #                          fromAge = NULL,toAge = NULL,inclUncertain = input$INCL_UNCERTAIN,
      #                          noFilterReportUncertain = TRUE)
      # 
      # df <- sendigR::getSubjSex(dbToken = dbToken, animalList = df,
      #                           sexFilter = NULL,inclUncertain = input$INCL_UNCERTAIN,
      #                           noFilterReportUncertain = TRUE)
      
      domainData <- merge(animal_list, lb_sub, by = c('STUDYID', 'USUBJID'), all=T)
      
      tableData <- aggDomain_bw_lb(domainData = domainData, domain = 'lb', input$INCL_UNCERTAIN)
      
      
      
      tab <- DT::datatable(tableData,
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtipB",
                             # buttons = c("csv", "excel", "pdf"),
                             buttons=list(list(
                               extend = 'collection',
                               buttons = list(list(extend='csv',
                                                   filename = 'LB Aggregate Table'),
                                              list(extend='excel',
                                                   filename = 'LB Aggregate Table')),
                               text = 'Download')),
                             #colReorder = TRUE,
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             #columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))
      tab <- DT::formatRound(table = tab,columns = c(5,6),digits = 2)
      
      tab
      
    })
    
    
    


    #### CL TAB ########################

    ###### get CL individual records table ----
    CL_subject <- shiny::reactive({
      animal_list <- animalList()
      cl_sub <- sendigR::getSubjData(dbToken = .sendigRenv$dbToken,
                                     domain = 'cl',
                                     animalList =  animal_list)
      cl_sub
    })



    CL_column <- shiny::reactive({
      if (nrow(CL_subject())>0) {
        get_col_name <- colnames(CL_subject())

      }
      get_col_name
    })

    cl_table_to_show <- shiny::reactive({
      tabl <- CL_subject()
      tabl <- subset(tabl, select=input$cl_filter_column)
      tabl
    })


    cl_selected_column_to_show <- shiny::reactive({
      col_selected <- intersect(cl_col_names_selected,CL_column())
      col_selected

    })
    ###### CL individual record table UI with hide/show side column ----

    output$cl_indiv_table <- shiny::renderUI({

      if (input$cl_hide_check_column==0) {
        shiny::fluidRow(
          htmltools::br(),
          shiny::column(width = 1,
                 shiny::checkboxGroupInput(inputId = 'cl_filter_column', label = "Display Column",
                                    choices = CL_column(),
                                    selected = cl_selected_column_to_show())),
          shiny::column(width = 11,
                 DT::dataTableOutput('cl_subj')))
      } else {
        shiny::fluidRow(
          htmltools::br(),
          shiny::column(width = 12,
                 DT::dataTableOutput('cl_subj')))
      }
    })

    #### update selected column in cl individual table
    shiny::observeEvent(input$cl_hide_check_column,{
      shiny::updateCheckboxGroupInput(session = session, inputId = 'cl_filter_column',
                               selected = input$cl_filter_column)
    })


    ######  output datatable for CL individual table ----
    output$cl_subj <- DT::renderDataTable(server = F,{
      tab <- cl_table_to_show()
      tab <- tab %>% dplyr::mutate_if(is.character,as.factor)

      tab <- DT::datatable(tab,
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtipB",
                             # buttons = c("csv", "excel", "pdf"),
                             buttons=list(list(
                               extend = 'collection',
                               buttons = list(list(extend='csv',
                                                   filename = 'CL Individual Record Table'),
                                              list(extend='excel',
                                                   filename = 'CL Individual Record Table')),
                               text = 'Download')),
                             #colReorder = TRUE,
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             #columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))

      tab

    })

    #### CL aggregate table


    ##### BW Tab ###############################

    output$bodyWeightTime <- shiny::renderPlot({

      animalList <- GetAnimalList(input$SDESIGN, input$SPECIES)
      bw <- BodyWeight(animalList)


      bw %>%
        dplyr::group_by(USUBJID) %>%
        ggplot2::ggplot( ggplot2::aes(x=days, y=BWSTRESN,  color='black')) +
        ggplot2::geom_line()

    })


    ###### get BW individual records table ----
    BW_subject <- shiny::reactive({
      animal_list <- animalList()
      bw_sub <- sendigR::getSubjData(dbToken = .sendigRenv$dbToken,
                                     domain = 'bw',
                                     animalList =  animal_list)
      bw_sub
    })



    BW_column <- shiny::reactive({
      if (nrow(BW_subject())>0) {
        get_col_name <- colnames(BW_subject())

      }
      get_col_name
    })

    bw_table_to_show <- shiny::reactive({
      tabl <- BW_subject()
      tabl <- subset(tabl, select=input$bw_filter_column)
      tabl
    })


    bw_selected_column_to_show <- shiny::reactive({
      col_selected <- intersect(bw_col_names_selected,BW_column())
      col_selected

    })
    ###### BW individual record table UI with hide/show side column ----

    output$bw_indiv_table <- shiny::renderUI({

      if (input$bw_hide_check_column==0) {
        shiny::fluidRow(
          htmltools::br(),
          shiny::column(width = 1,
                        shiny::checkboxGroupInput(inputId = 'bw_filter_column',
                                                  label = "Display Column",
                                                  choices = BW_column(),
                                                  selected = bw_selected_column_to_show())),
          shiny::column(width = 11,
                        DT::dataTableOutput('bw_subj')))
      } else {
        shiny::fluidRow(
          htmltools::br(),
          shiny::column(width = 12,
                 DT::dataTableOutput('bw_subj')))
      }
    })

    #### update selected column in bw individual table
    shiny::observeEvent(input$bw_hide_check_column, {
      shiny::updateCheckboxGroupInput(
        session = session,
        inputId = 'bw_filter_column',
        selected = input$bw_filter_column
      )
    })


    ###### output datatable for BW individual table ----
    output$bw_subj <- DT::renderDataTable(server = F,{
      tab <- bw_table_to_show()
      tab <- tab %>% dplyr::mutate_if(is.character,as.factor)

      tab <- DT::datatable(tab,
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtipB",
                             # buttons = c("csv", "excel", "pdf"),
                             buttons=list(list(
                               extend = 'collection',
                               buttons = list(list(extend='csv',
                                                   filename = 'BW Individual Record Table'),
                                              list(extend='excel',
                                                   filename = 'BW Individual Record Table')),
                               text = 'Download')),
                             #colReorder = TRUE,
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             #columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))

      tab

    })

    ###### BW aggregate table ----

    output$bw_agg_tab <- DT::renderDataTable(server = F,{
      
      animal_list <- animalList()
      bw_sub <- BW_subject()
      
     
      #get age at finding
      df <- sendigR::getFindingsSubjAge(dbToken = .sendigRenv$dbToken,
                                        findings=bw_sub,
                                        animalList = animal_list,
                                        fromAge = NULL,toAge = NULL,
                                        inclUncertain = input$INCL_UNCERTAIN,
                                        noFilterReportUncertain = TRUE)
      
      # df <- sendigR::getSubjSex(dbToken = dbToken, animalList = df,
      #                           sexFilter = NULL,inclUncertain = input$INCL_UNCERTAIN,
      #                           noFilterReportUncertain = TRUE)
      domainData <- merge(animal_list, df, by = c('STUDYID', 'USUBJID'),
                         all=T, suffixes = c("_Control_animal", "_BW_AGE"))
      
      
tableData <- aggDomain_bw_lb(domainData = domainData, domain = 'bw',
                             includeUncertain = input$INCL_UNCERTAIN)

      
      tab <- DT::datatable(tableData,
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtipB",
                             # buttons = c("csv", "excel", "pdf"),
                             buttons=list(list(
                               extend = 'collection',
                               buttons = list(list(extend='csv',
                                                   filename = 'BW Aggregate Table'),
                                              list(extend='excel',
                                                   filename = 'BW Aggregate Table')),
                               text = 'Download')),
                             #colReorder = TRUE,
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             #columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))
      tab <- DT::formatRound(table = tab,columns = c(3,4),digits = 2)
      
      tab
      
    })
    
    


    ##### eDish #############################

    studyAnimalList <- shiny::reactive({
      studyInfo <- GetStudyTS(input$STUDYID)

      design <- toupper(studyInfo[studyInfo$TSPARMCD == 'SDESIGN',]$TSVAL)
      species <- toupper(studyInfo[studyInfo$TSPARMCD == 'SPECIES',]$TSVAL)

      GetAnimalList(design, species)
    })



    output$eDish <- shiny::renderPlot({


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


      ggplot2::ggplot(data) +
        ggplot2::geom_point(ggplot2::aes(x=LBSTRESC_TRANS.x,
                                         y=LBSTRESC_TRANS.y),
                            color=grDevices::rgb(0, 0, 0, 0.4), size=4) +
        ggplot2::xlab(input$x_axis_LBTESTCD) + ggplot2::ylab(input$y_axis_LBTESTCD) +
        ggplot2::geom_point(data=thisStudyAnimals,
                            mapping=ggplot2::aes(x=LBSTRESC_TRANS.x / xAvg,
                                                 y=LBSTRESC_TRANS.y / yAvg,
                                                 color=SET),
                            size=4)

    })

    # # CLose connection to database at end of execution
    # shiny::onSessionEnded(function() {
    #   sendigR::disconnectDB(dbToken)
    # })

  }

  ##### Run the application ----
  shiny::shinyApp(ui = ui, server = server)

}

################################################################################
#### Avoid  'no visible binding for global variable' notes from check of package: -----
MISPEC <- Animals.In.MISPEC <- days <- BWSTRESN <- NULL
LBSTRESC_TRANS.x <- LBSTRESC_TRANS.y <- SET <- NULL
distribution <- ..density.. <- NULL


############################## END ##################################################