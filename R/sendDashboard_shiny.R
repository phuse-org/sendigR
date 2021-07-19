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

  # # load named vector for all column and description
  #base::load("\\\\FSDKHQ001\\dep402$\\000-4284\\DATA MANAGEMENT\\@Data Science\\BioCelerate\\sendDashboard\\column_toolip.RData")
  base::load("column_toolip.RData")
  # # this will load following named vectors
  # # control_animal_tooltip
  # # MI_tooltip
  # # LB_tooltip
  # # BW_tooltip
  # # MI_agg_tooltip
  # # LB_agg_tooltip
  # # BW_agg_tooltip

  #domain_tooltip is the named vector contain all column name and description
  #datatable is the table will be shown to UI side
  # match_tooltip <- function(domain_tooltip, datatable) {
  #   index <- which(names(domain_tooltip) %in% colnames(datatable))
  #   tooltip_list <- domain_tooltip[index]
  #   return(tooltip_list)
  # }

  #function to create tooltip for column in the table
  #tooltip_list is the list of column description (returned from getTabColLabels function)
  #to show as hover text on column
  tooltipCallback <- function(tooltip_list) {
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      sprintf("  var tooltips = [%s];", toString(paste0("'", tooltip_list, "'"))),
      "  for(var i = 1; i <= tooltips.length; i++){",
      "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
      "  }",
      "}"
    )
    return(headerCallback)
  }

  # JavaScript code for cllick
  click_jscode <- '
Shiny.addCustomMessageHandler("mymessage", function(message) {
  document.getElementById(message).click();
});
'

  # shortcut for find not in
  '%ni%' <- Negate('%in%')

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
                htmltools::br(),
                shiny::actionButton("refreshData_02", "Generate/Update Data",
                  style = "background-color:#FFFFFF;
                  color:#E31616;
                  border-color:#BEBEBE;
                  border-style:solid;
                  border-width:1px;
                  border-radius:5%;
                  font-weight:bold;
                  font-size:18px;"),
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
                shiny::actionButton("refreshData", "Generate/Update Data",
                                    style = "background-color:#FFFFFF;
                                            color:#E31616;
                                            border-color:#BEBEBE;
                                            border-style:solid;
                                            border-width:1px;
                                            border-radius:5%;
                                            font-weight:bold;
                                            font-size:18px;"),
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

      htmltools::tags$head(shiny::tags$script(shiny::HTML(click_jscode))),
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
                                   download_csv_UI('download_filter_animal'),
                                   htmltools::br(),
                                   htmltools::br(),
                                   download_rds_UI('download_filter_animal_rds'),
                                   htmltools::br(),
                                   htmltools::br(),
                                   htmltools::br()
                                   )),
                 shiny::tabPanel("MI", ##### MI ----
                          shiny::tabsetPanel(
                            shiny::tabPanel("MI Findings",
                                            shiny::fluidRow(
                                              htmltools::br(),
                                              shiny::column(width = 3, offset = 1,
                                                            shiny::selectInput("MISPEC",
                                                                               "Select MISPEC:",
                                                                               availableOrgans,
                                                                               selected='KIDNEY'),
                                                            shiny::uiOutput('mi_findings_filter')),

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
                                    download_csv_UI('download_MI_individual'),
                                    htmltools::br(),
                                    htmltools::br(),
                                    download_rds_UI('download_MI_individual_rds'),
                                    htmltools::br()),

                            shiny::tabPanel("Aggregate Table",
                                     DT::dataTableOutput('mi_agg_tab'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     download_csv_UI('download_MI_agg'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     download_rds_UI('download_MI_agg_rds'),
                                     htmltools::br(),
                                     htmltools::br()))),

                 shiny::tabPanel("LB", #####LB ----
                          shiny::tabsetPanel(
                            # shiny::tabPanel("LB Findings",
                            #          shiny::fluidRow(
                            #            htmltools::br(),
                            #            shiny::column(width = 2,
                            #                          shiny::selectInput("LBTESTCD",
                            #                               "Select LBTESTCD:",
                            #                               availableLBTESTCD)),
                            #            shiny::column(width = 2,
                            #                          shiny::radioButtons("dist", "Distribution type:",
                            #                                c("Normal" = "norm",
                            #                                  "Log-normal" = "lnorm"))),
                            #            shiny::column(width = 7,offset = 1,
                            #                          shiny::plotOutput("labTestHist")))
                            #          ),
                            shiny::tabPanel("Individual Records",
                                            shiny::checkboxInput('lb_hide_check_column',
                                                                 label = 'Show Only Table',
                                                                 value = 0),
                                     htmltools::br(),
                                     shiny::uiOutput('lb_indiv_table'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     download_csv_UI('download_LB_individual'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     download_rds_UI('download_LB_individual_rds'),
                                     htmltools::br(),
                                     htmltools::br()),

                            shiny::tabPanel("Aggregate Table",
                                     DT::dataTableOutput('lb_agg_tab_render'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     download_csv_UI('download_LB_agg'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     download_rds_UI('download_LB_agg_rds'),
                                     htmltools::br(),
                                     htmltools::br())

                          )
                          ),
                 # shiny::tabPanel("CL", ##### CL ----
                 #          shiny::tabsetPanel(
                 #            shiny::tabPanel("Individual Records",
                 #                            shiny::checkboxInput("cl_hide_check_column",
                 #                                                 label = "Show Only Table",
                 #                                                 value = 0),
                 #                     htmltools::br(),
                 #                     shiny::uiOutput("cl_indiv_table"),
                 #                     htmltools::br(),
                 #                     htmltools::br(),
                 #                     htmltools::br(),
                 #                     htmltools::br())
                 #            # shiny::tabPanel("Aggregate Table",
                 #            #          DT::dataTableOutput('cl_agg_tab'),
                 #            #          htmltools::br(),
                 #            #          htmltools::br(),
                 #            #          htmltools::br(),
                 #            #          htmltools::br())
                 #            )),
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
                                     download_csv_UI('download_BW'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     download_rds_UI('download_BW_rds'),
                                     htmltools::br(),
                                     htmltools::br()),
                            shiny::tabPanel("Aggregate Table",
                                     DT::dataTableOutput('bw_agg_tab_render'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     download_csv_UI('download_BW_agg'),
                                     htmltools::br(),
                                     htmltools::br(),
                                     download_rds_UI('download_BW_agg_rds'),
                                     htmltools::br(),
                                     htmltools::br()))),
                 shiny::tabPanel("Download",
                                 htmltools::br(),
                                 shiny::downloadButton('download_all', "Download"))

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
    animalList<-shiny::eventReactive(
      input$refreshData, {

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

    # when click on upper left (generate) button, this will click bottom left button
    shiny::observeEvent(input$refreshData_02, {
      session$sendCustomMessage("mymessage", "refreshData")
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

    output$animals <- DT::renderDataTable(server = T,{


      animal_df <- animalList()
      # make last column as date

      #convert character to factor to make filter work
      animal_df <- animal_df %>% dplyr::mutate_if(is.character,as.factor)

      # Associate table header with labels
      headerCallback <- tooltipCallback(tooltip_list = getTabColLabels(animal_df))

      animal_df <- DT::datatable(
        animal_df,
        class = "cell-border stripe",
        filter = list(position = 'top'),
        # extensions = list("Buttons" = NULL,
        #                   "ColReorder" = NULL),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center; font-size: 20px; color: black",
          "Table :", htmltools::strong("Filtered Control Animal")
        ),
        options = list(
          dom = "lfrtip",
          #buttons = c("csv", "excel", "copy", "pdf"),
          # buttons=list(list(
          #   extend = 'collection',
          #   buttons = list(list(extend='csv',
          #                       filename = 'Filtered Control Animal'),
          #                  list(extend='excel',
          #                       filename = 'Filtered Control Animal')
          #                  # list(extend='pdf',
          #                  #      pageSize = 'A4',
          #                  #      orientation = 'landscape',
          #                  #      filename= 'Filtered Control Animal')
          #                  ),
          #   text = 'Download'
          # )),

          #colReorder = TRUE,
          scrollY = TRUE,
          scrollX=TRUE,
          pageLength = 10,
          headerCallback= DT::JS(headerCallback),
          #columnDefs = list(list(className = "dt-center", targets = "_all")),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")
          ))
      animal_df
      })


    ##### Control animal download button ----
    # output$download_filter_animal <- shiny::downloadHandler(
    #   filename = function() {
    #     paste0("Filtered Control Animal", ".csv")
    #   },
    #   content = function(file) {
    #     animal <- animalList()
    #   utils::write.csv(animal, file)
    #   })
    # call module to download csv file


    shiny::callModule(download_csv, id = "download_filter_animal",
                      data = animalList,
                      filename='filtered_Control_Animal')

    # call module to download rds data
    shiny::callModule(download_rds, id = "download_filter_animal_rds",
                      data = animalList,
                      filename='filtered_Control_Animal')




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



    # get Mifindings whole table
   MiFindings_filter_table <- shiny::reactive({
     shiny::req(input$STRAIN)
     df <- MiFindings_table(animalList(), input$MISPEC)
     df
   })


    # render mi findings filter
    output$mi_findings_filter <- shiny::renderUI({

      df <- MiFindings_filter_table()
      df_route <- unique(df[['ROUTE']])
      df_species <- unique(df[['SPECIES']])
      df_strain <- unique(df[['STRAIN']])
      df_sex <- unique(df[['SEX']])

      # addUIDep(shiny::selectizeInput("SPECIES",label='Select Species:',
      #                                choices= GetUniqueSpecies(),
      #                                selected='RAT',
      #                                multiple=TRUE,
      #                                options=list(plugins=list('drag_drop','remove_button'))))

      shiny::fluidRow(


                     addUIDep(shiny::selectizeInput("mi_route",
                                         "Select Route:",
                                         choices=df_route,
                                         selected=df_route,
                                         multiple=TRUE,
                                         options=list(plugins=list('drag_drop','remove_button')
                                         ))),


                    addUIDep(shiny::selectizeInput("mi_species",
                                         "Select Species:",
                                         df_species,
                                         selected=df_species,
                                         multiple=TRUE,
                                         options=list(plugins=list('drag_drop','remove_button'))
                                         )),


                     addUIDep( shiny::selectizeInput("mi_strain",
                                         "Select Strain:",
                                         df_strain,
                                         selected=df_strain,
                                         multiple=TRUE,
                                         options=list(plugins=list('drag_drop','remove_button'))
                                         )
                     ),


                   addUIDep(shiny::selectizeInput("mi_sex",
                                         "Select Sex:",
                                         df_sex,
                                         selected=df_sex,
                                         multiple=TRUE,
                                         options=list(plugins=list('drag_drop','remove_button'))
                                         ))
      )


    })

    findings_table_after_filter <- shiny::reactive({
      shiny::req(input$STRAIN)

      finalFindings <- MiFindings_filter_table()

      finalFindings <- finalFindings %>% dplyr::filter(ROUTE %in% input$mi_route,
                                                       STRAIN %in% input$mi_strain,
                                                       SPECIES %in% input$mi_species,
                                                       SEX %in% input$mi_sex)

      findingsCount <- finalFindings %>%
        dplyr::distinct(STUDYID, USUBJID, MISTRESC) %>% # only one organ, finding per animal (input errors cause duplications)
        dplyr::count(MISTRESC) %>%
        dplyr::arrange(-n)

      # findings = n / total animals * 100
      # round to 2 decimal places and
      # sort by descending.
      findingsCount$Incidence <- (findingsCount$n / length(unique(finalFindings$USUBJID))) * 100
      findingsCount$Incidence <- paste0(round(findingsCount$Incidence, 2), '%')
      findingsCount <- dplyr::select(findingsCount, -n)
      findingsCount

    })


    output$findingsTable <- DT::renderDataTable(server = T,{

      # shiny::req(input$STRAIN)
      # findings <- MiFindings(animalList(), input$MISPEC)
      # findings <- findings %>% dplyr::mutate_if(is.character,as.factor)
      findings <- findings_table_after_filter()
      findings_name <- paste0("MI Findings_",input$MISPEC)
      findings_name_tab <- paste0("MI Findings: ",input$MISPEC)

      findings <- DT::datatable(findings,
        class = "cell-border stripe",
        # filter = list(position = 'top'),
        extensions = list("Buttons" = NULL,
                          "ColReorder" = NULL),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center; font-size: 20px; color: black",
          "Table :", htmltools::strong(findings_name_tab)
        ),
        options = list(
          # list(searching = FALSE),
          dom = "lrtipB",
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
          lengthChange = FALSE,
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
    output$mi_subj <- DT::renderDataTable(server = T,{
      tab <- table_to_show()
      tab <- tab %>% dplyr::mutate_if(is.character,as.factor)

      # Associate table header with labels
      headerCallback <- tooltipCallback(tooltip_list = getTabColLabels(tab))
      tab <- DT::datatable(tab,
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtip",
                             #buttons = c("csv", "excel", "pdf"),
                             # buttons=list(list(
                             #   extend = 'collection',
                             #   buttons = list(list(extend='csv',
                             #                       filename = 'MI Individual Record Table'),
                             #                  list(extend='excel',
                             #                       filename = 'MI Individual Record Table')
                             #                  # list(extend='pdf',
                             #                  #      pageSize = 'A4',
                             #                  #      orientation = 'landscape',
                             #                  #      filename= 'MI Individual Table')
                             #                  ),
                             #   text = 'Download'
                             # )),
                             #colReorder = TRUE,
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             headerCallback= DT::JS(headerCallback),
                             #columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))

      tab

    })


    ####### Download MI individual table csv and rds ----

    shiny::callModule(download_csv, id = "download_MI_individual",
                      data=table_to_show, filename="MI_Individual_Table")

    shiny::callModule(download_rds, id="download_MI_individual_rds",
                      data=table_to_show, filename="MI_Individual_Table")

    MI_agg_table <- shiny::reactive({
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
      #domainData$MISTRESC[domainData$MISTRESC == ''] <- 'NORMAL'
      remove_index <- which(domainData$MISTRESC=='')
      domainData <- domainData[-remove_index,]

      # TODO: Do we account for animals that do not have
      # MI (or maybe other domains?) for which there is
      # no record? I know sometimes if result is normal
      # they will not get recorded.  Maybe this could be
      # a flag to toggle.

      shiny::isolate(tableData <- aggDomain(domainData, grpByCols,
                                            includeUncertain=input$INCL_UNCERTAIN))

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

      tableData
    })



   ###### MI aggregate table ----

    output$mi_agg_tab <- DT::renderDataTable(server = T,{
      tableData <- MI_agg_table()

      # Associate table header with labels
      headerCallback <- tooltipCallback(tooltip_list = getTabColLabels(tableData))

      tab <- DT::datatable(tableData,
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtip",
                             # buttons = c("csv", "excel", "pdf"),
                             #
                             # buttons=list(list(
                             #   extend = 'collection',
                             #   buttons = list(list(extend='csv',
                             #                       filename = 'MI Aggregate Table'),
                             #                  list(extend='excel',
                             #                       filename = 'MI Aggregate Table')),
                             #   text = 'Download')),

                             #colReorder = TRUE,
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             headerCallback= DT::JS(headerCallback),
                             columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))

      tab

    })

    ####### Download MI aggregate table csv and rds ----
    shiny::callModule(download_csv, id = "download_MI_agg",
                      data=MI_agg_table, filename="MI_Aggregate_Table")

    shiny::callModule(download_rds, id="download_MI_agg_rds",
                      data=MI_agg_table, filename="MI_Aggregate_Table")

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
    output$lb_subj <- DT::renderDataTable(server = T,{
      tab <- lb_table_to_show()
      tab <- tab %>% dplyr::mutate_if(is.character, as.factor)

      # Associate table header with labels
      LB_headerCallback <- tooltipCallback(tooltip_list = getTabColLabels(tab))

      tab <- DT::datatable(
        tab,
        filter = list(position = 'top'),
        options = list(
          dom = "lfrtip",
          # buttons = c("csv", "excel", "pdf"),

          # buttons=list(list(
          #   extend = 'collection',
          #   buttons = list(list(extend='csv',
          #                       filename = 'LB Individual Record  Table'),
          #                  list(extend='excel',
          #                       filename = 'LB Individual Record Table')),
          #   text = 'Download')),
          #colReorder = TRUE,
          scrollY = TRUE,
          scrollX = TRUE,
          pageLength = 25,
         headerCallback= DT::JS(LB_headerCallback),
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

    ####### Download LB individual table csv and rds ----
    shiny::callModule(download_csv, id = "download_LB_individual",
                      data=lb_table_to_show, filename="LB_Individual_Table")

    shiny::callModule(download_rds, id="download_LB_individual_rds",
                      data=lb_table_to_show, filename="LB_Individual_Table")


    # LB displays a histogram
    # and probability density
    # density function given
    # a particular LBTESTCD.
    # Currently, only 4 Liver
    # enzymes are implemented.
    # User has the option
    # of displaying as normal
    # or log-normal distribution.


    # output$labTestHist <- shiny::renderPlot({
    #
    #   # LiverFindings() gets
    #   # lab test results for
    #   # a user-defined LBTESTCD
    #
    #   labResults <- LiverFindings(animalList(), input$LBTESTCD)
    #
    #
    #   # change the distbution
    #   # function depending on
    #   # user input
    #
    #   if (input$dist == 'norm') {
    #     labResults$distribution <- labResults$LBSTRESC_TRANS
    #     dist <- MASS::fitdistr(labResults$distribution, 'normal')
    #     fun <- stats::dnorm
    #   } else if (input$dist == 'lnorm') {
    #     labResults$distribution <- log(labResults$LBSTRESC_TRANS + 1)
    #     dist <- MASS::fitdistr(labResults$distribution, 'lognormal')
    #     fun <- stats::dlnorm
    #   }
    #
    #   # plot the probability
    #   # distribution and
    #   # the pdf
    #
    #   ggplot2::ggplot(labResults) +
    #     ggplot2::geom_histogram(ggplot2::aes(x = distribution, y = ..density..),
    #                    fill = "blue",
    #                    colour = "grey", alpha=0.6) +
    #     ggplot2::stat_function(fun = fun,
    #                   args = list(mean = dist$estimate[1], sd = dist$estimate[2], log = F),
    #                   color="grey", lwd=1, alpha=0.6)
    #
    # })


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


    LB_agg_table <- shiny::reactive({
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

      shiny::isolate(tableData <- aggDomain_bw_lb(domainData = domainData, domain = 'lb', input$INCL_UNCERTAIN))

      tableData


    })

    output$lb_agg_tab_render <- DT::renderDataTable(server = T,{
      tableData <- LB_agg_table()

      # Associate table header with labels
      headerCallback <- tooltipCallback(tooltip_list = getTabColLabels(tableData))

      tab <- DT::datatable(tableData,
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtip",
                             # buttons = c("csv", "excel", "pdf"),
                             # buttons=list(list(
                             #   extend = 'collection',
                             #   buttons = list(list(extend='csv',
                             #                       filename = 'LB Aggregate Table'),
                             #                  list(extend='excel',
                             #                       filename = 'LB Aggregate Table')),
                             #   text = 'Download')),
                             #colReorder = TRUE,
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             headerCallback= DT::JS(headerCallback),
                             columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))
      tab <- DT::formatRound(table = tab,columns = c(5,6),digits = 2)

      tab

    })


    ####### Download Lb Aggregate table csv and rds ----

    shiny::callModule(download_csv, id = "download_LB_agg",
                      data=LB_agg_table, filename="LB_Aggregate_Table")

    shiny::callModule(download_rds, id="download_LB_agg_rds",
                      data=LB_agg_table, filename="LB_Aggregate_Table")


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
    output$cl_subj <- DT::renderDataTable(server = T,{
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
    output$bw_subj <- DT::renderDataTable(server = T,{
      tab <- bw_table_to_show()
      tab <- tab %>% dplyr::mutate_if(is.character,as.factor)

      # Associate table header with labels
      headerCallback <- tooltipCallback(tooltip_list = getTabColLabels(tab))

      tab <- DT::datatable(tab,
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtip",
                             # buttons = c("csv", "excel", "pdf"),
                             # buttons=list(list(
                             #   extend = 'collection',
                             #   buttons = list(list(extend='csv',
                             #                       filename = 'BW Individual Record Table'),
                             #                  list(extend='excel',
                             #                       filename = 'BW Individual Record Table')),
                             #   text = 'Download')),
                             #colReorder = TRUE,
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             headerCallback= DT::JS(headerCallback),
                             #columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))

      tab

    })


    ####### Download BW_Individual_Table csv and rds ----

    shiny::callModule(download_csv, id = "download_BW",
                      data=bw_table_to_show, filename="BW_Individual_Table")

    shiny::callModule(download_rds, id="download_BW_rds",
                      data=bw_table_to_show, filename="BW_Individual_Table")

    ###### BW aggregate table ----

    BW_agg_table <- shiny::reactive({
      animal_list <- animalList()
      bw_sub <- BW_subject()


      #get age at finding
      shiny::isolate(df <- sendigR::getFindingsSubjAge(dbToken = .sendigRenv$dbToken,
                                                       findings=bw_sub,
                                                       animalList = animal_list,
                                                       fromAge = NULL,toAge = NULL,
                                                       inclUncertain = input$INCL_UNCERTAIN,
                                                       noFilterReportUncertain = TRUE))

      # df <- sendigR::getSubjSex(dbToken = dbToken, animalList = df,
      #                           sexFilter = NULL,inclUncertain = input$INCL_UNCERTAIN,
      #                           noFilterReportUncertain = TRUE)
      domainData <- merge(animal_list, df, by = c('STUDYID', 'USUBJID'),
                          all=T, suffixes = c("_Control_animal", "_BW_AGE"))
      shiny::isolate(tableData <- aggDomain_bw_lb(domainData = domainData,domain = 'bw',
                                                  includeUncertain =input$INCL_UNCERTAIN))
    })

    ###### BW aggregate table render ----
    output$bw_agg_tab_render <- DT::renderDataTable(server = T,{
      tableData <- BW_agg_table()

      # Associate table header with labels
      headerCallback <- tooltipCallback(tooltip_list = getTabColLabels(tableData))

      tab <- DT::datatable(tableData,
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtip",
                             # buttons = c("csv", "excel", "pdf"),
                             # buttons=list(list(
                             #   extend = 'collection',
                             #   buttons = list(list(extend='csv',
                             #                       filename = 'BW Aggregate Table'),
                             #                  list(extend='excel',
                             #                       filename = 'BW Aggregate Table')),
                             #   text = 'Download')),
                             #colReorder = TRUE,
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             headerCallback= DT::JS(headerCallback),
                             columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))
      tab <- DT::formatRound(table = tab,columns = c(3,4),digits = 2)

      tab

    })


    ####### Download BW_Aggregate_Table csv and rds ----

    shiny::callModule(download_csv, id = "download_BW_agg",
                      data=BW_agg_table, filename="BW_Aggregate_Table")

    shiny::callModule(download_rds, id="download_BW_agg_rds",
                      data=BW_agg_table, filename="BW_Aggregate_Table")



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



    filter_criteria <- shiny::reactive({
      filter_selected <- list(
        From=as.character(input$STSTDTC[1]),
        To=as.character(input$STSTDTC[2]),
        Design=input$SDESIGN,
        Route=input$ROUTE,
        Species=input$SPECIES,
        Strain=input$STRAIN,
        Sex=input$SEX,
        Uncertain=input$INCL_UNCERTAIN
      )
      filter_selected
    })


    # download all data as RData file
    output$download_all <- shiny::downloadHandler(
      filename <- function(){
        paste0("All_Table_", Sys.Date(),".RData")
      },

      content = function(file) {
        Control_Animal <- animalList()
        MI_Findings <- findings_table_after_filter()
        MI_Individual <- table_to_show()
        MI_Aggregate <- MI_agg_table()
        LB_Individual <- lb_table_to_show()
        LB_Aggregate <- LB_agg_table()
        BW_Individual <- bw_table_to_show()
        BW_Aggregate <- BW_agg_table()
        Filtered_option <- filter_criteria()

        save(Control_Animal,
             MI_Findings,
             MI_Individual,
             MI_Aggregate,
             LB_Individual,
             LB_Aggregate,
             BW_Individual,
             BW_Aggregate,
             Filtered_option,
             file = file)
      }
    )

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
