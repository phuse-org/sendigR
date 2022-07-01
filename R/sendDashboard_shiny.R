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

  # get minimum date
  minStudyStartDate <- as.Date(getMinStudyStartDate())
  # get available studies list
  availableStudies <- GetAvailableStudies()
  availableStudies <- as.list(stats::setNames(availableStudies, availableStudies))
  # get unique sex
  availableSex <- GetUniqueSex()
  availableSex <- availableSex[[1]]
  availableSex <- c(availableSex, "All")
  availableSex <- as.list(stats::setNames(availableSex,availableSex))
  # get phase
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
    jqueryUIDep <- htmltools::htmlDependency("jqueryui", "1.10.4",
                                             c(href="shared/jqueryui/1.10.4"),
                                             script = "jquery-ui.min.js",
                                             stylesheet = "jquery-ui.min.css")
    htmltools::attachDependencies(x, c(htmltools::htmlDependencies(x),
                                       list(jqueryUIDep)))
  }

  #reactive value ----
  values <- shiny::reactiveValues()
  values$selected_routes <- NULL

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
  # list of column that by default selected in LB individual records table
  lb_col_names_selected <- c('STUDYID','USUBJID','LBTEST','LBTESTCD','LBORRES',
                             'LBORRESU','LBSTRESC','LBSTRESN','LBSTRESU','LBSPEC')
  # list of column that by default selected in BW individual records table
  bw_col_names_selected <- c('STUDYID','USUBJID','BWTEST','BWSTRESN'
                             ,'BWSTRESU','VISITDY')


  # JavaScript code for click
  click_jscode <- '
Shiny.addCustomMessageHandler("mymessage", function(message) {
  document.getElementById(message).click();
}); 
'
  # shortcut for find not in
  '%ni%' <- Negate('%in%')


  ## get css file
  www_path <- system.file("", package = "sendigR")
  dt_extension <- paste0(www_path, "/www/DT_extension" )
  animate_css_path <- paste0(www_path, "/www")

### tour ----

guide <- cicerone::Cicerone$new()$step(
	".main-header",
	"Need an Introduction? \U270B",
	"If you want a step by step introduction, press next \U1F449. Otherwise hit the close button \U1F447",
	is_id = FALSE
)$step(
	el = "STSTDTC",
	title = "Date Range", 
	description = "Choose the date range that you want to include for control animal."
)$step(
	"SDESIGN-label",
	"Study Design",
	"Select study design. You can only choose one study design from the list."

)$step(
	"ROUTE",
	"Route of Administration",
	"You can select multiple route of administration from drop-down list. 
	All availbale route of administraion will be included if kept blank."
)$step(
	"SPECIES-label",
	"Species",
	"You can select multiple species from drop-down list.
	All availbale species will be included if kept blank."
)$step(
	"STRAIN-label",
	"Strain",
	"You can select multiple strain from drop-down list.
	All availbale strain will be included if kept blank. Available strain list will
	depends on what species you selected in previous step."
)$step(
	"SEX-label",
	"SEX",
	"You can select animal sex from drop-down list."
)$step(
	"INCL_UNCERTAIN",
	"Whether to include uncertain control animal",
	"Check this box if you want to include uncertain control animal."
)$step(
	"refreshData",
	"Extract Control Animal",
	"Depending on filtering criteria you provided, this will extract all the control animal from the database."
)$step(
	"refreshData_02",
	"same as before",
	"This is same as previous button, two generate/update button just for your convenience \U1F604"
)

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

##### Date Range, Design, Route, Species, Strain, Sex, Uncertain ----
                htmltools::br(),
				shiny::actionButton("tour", "want a tour?"),
                shiny::actionButton("refreshData_02", "Generate/Update Data"
                #   style = "background-color:#FFFFFF;
                #   color:#E31616;
                #   border-color:#BEBEBE;
                #   border-style:solid;
                #   border-width:1px;
                #   border-radius:5%;
                #   font-weight:bold;
                #   font-size:18px;"
				  ),
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
                 shiny::uiOutput('ROUTE'),
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

                 shiny::checkboxInput('INCL_UNCERTAIN',
                               'Include uncertain rows',
                               value = FALSE),
                shiny::actionButton("refreshData", "Generate/Update Data"
                                    # style = "background-color:#FFFFFF;
                                    #         color:#E31616;
                                    #         border-color:#BEBEBE;
                                    #         border-style:solid;
                                    #         border-width:1px;
                                    #         border-radius:5%;
                                    #         font-weight:bold;
                                    #         font-size:18px;"
											),
                                            htmltools::br()),
											shinydashboard::menuItem("Project links",
											shinydashboard::menuSubItem(text = "GitHub Link", href = "https://github.com/phuse-org/sendigR"),
											shinydashboard::menuSubItem(text = "Shiny App", href= "https://phuse-org.github.io/sendigR/articles/SendDashboard.html"))

      ),

      # left side scroller
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(".sidebar {height: 94vh; overflow-y: auto;}")
        )
      )
	),

##### Main body ----
    # Main body of the app. Consists
    # of different of different tabs
    # each displaying a domain and
    # analysis/analyses relevant to
    # that particular domain.

    shinydashboard::dashboardBody(

		cicerone::use_cicerone(),

	#   shiny::includeCSS("www/theme.css"),
	  shiny::includeCSS(paste0(www_path, "/www/from_sass_theme.css")),
	  htmltools::htmlDependency(
      "animate.css", "4.1.1",
	  animate_css_path, stylesheet = "animate.min.css"),
	#   shiny::includeCSS("https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
      htmltools::tags$head(shiny::tags$script(shiny::HTML(click_jscode))),
      shiny::tabsetPanel(type = 'tab',
                 shiny::tabPanel('ANIMALS', ##### Animal Tab ----
                          shiny::fluidRow(title = "Filtered control animals",
                                   htmltools::br(),
                                   shinycssloaders::withSpinner(

                                   DT::dataTableOutput("animals"), 	color = "#134585"),
                                   htmltools::br(),
                                   download_csv_UI('download_filter_animal'),
                                   htmltools::br(),htmltools::br(),
                                   download_rds_UI('download_filter_animal_rds'),
                                   htmltools::br(),htmltools::br(),htmltools::br()
                                   )),
                 shiny::tabPanel("MI", ##### MI ----

                                 shiny::fluidRow(

                                   age_unit_input("mi_age_unit"),

                                   shiny::uiOutput("mi_age")),

                                 shiny::fluidRow(
                                   shiny::column(width = 2,
                                                 shiny::actionButton("submit_mi_age", "Update"

											# 						 style="background-color:#FFFFFF;
                                            # color:#E31616;
                                            #                                     border-color:#BEBEBE;
                                            #                                     border-style:solid;
                                            #                                     border-width:1px;
                                            #                                     border-radius:5%;
                                            #                                     font-weight:bold;
                                            #                                     font-size:14px;"
																				))),
                                 htmltools::br(style="line-height: 10px"),
                          shiny::tabsetPanel(
                            shiny::tabPanel("MI Findings",
                                            shiny::fluidRow(
                                              htmltools::br(),
                                              shiny::column(width = 3, offset = 1,
                                                            shiny::selectInput("MISPEC",
                                                                               "Select MISPEC:",
                                                                               availableOrgans,
                                                                               selected='KIDNEY'),
                                                            shiny::uiOutput('mi_findings_filter'),
                                                            shiny::actionButton('mi_finding_update', 'Generate/Update Table'

											# 									style = "background-color:#FFFFFF;
                                            # color:#E31616;
                                            #                                     border-color:#BEBEBE;
                                            #                                     border-style:solid;
                                            #                                     border-width:1px;
                                            #                                     border-radius:5%;
                                            #                                     font-weight:bold;
                                            #                                     font-size:14px;"
																				)),
                                       shiny::column(width = 6, offset = 1,
                                              DT::dataTableOutput("findingsTable"),
                                              htmltools::br(),htmltools::br(),
                                              htmltools::br(),htmltools::br()))),
                            shiny::tabPanel("Individual Records",
                                            shiny::checkboxInput('hide_check_column',
                                                                 label = 'Show Only Table',
                                                                 value = 0),
                                    htmltools::br(),
                                    shiny::uiOutput('mi_indiv_table'),
                                    htmltools::br(),htmltools::br(),
                                    download_csv_UI('download_MI_individual'),
                                    htmltools::br(),htmltools::br(),
                                    download_rds_UI('download_MI_individual_rds'),
                                    htmltools::br()),

                            shiny::tabPanel("Aggregate Table",
                                     DT::DTOutput('mi_agg_tab'),
                                     htmltools::br(),htmltools::br(),
                                     download_csv_UI('download_MI_agg'),
                                     htmltools::br(),htmltools::br(),
                                     download_rds_UI('download_MI_agg_rds'),
                                     htmltools::br(),htmltools::br())
                            )),

                 shiny::tabPanel("LB", #####LB ----

                                 shiny::fluidRow(

                                   age_unit_input("lb_age_unit"),

                                   shiny::uiOutput("lb_age")),

                                 shiny::fluidRow(
                                   shiny::column(width = 2,
                                   shiny::actionButton("submit_lb_age", "Update"

											# 		   style="background-color:#FFFFFF;
                                            # color:#E31616;
                                            #                                     border-color:#BEBEBE;
                                            #                                     border-style:solid;
                                            #                                     border-width:1px;
                                            #                                     border-radius:5%;
                                            #                                     font-weight:bold;
                                            #                                     font-size:14px;"
																				))),
                                 htmltools::br(style="line-height: 10px"),
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
                                     htmltools::br(),htmltools::br(),
                                     download_csv_UI('download_LB_individual'),
                                     htmltools::br(),htmltools::br(),
                                     download_rds_UI('download_LB_individual_rds'),
                                     htmltools::br(),htmltools::br()),

                            shiny::tabPanel("Numeric Aggregate Table",
                                     DT::dataTableOutput('lb_agg_tab_render'),
                                     htmltools::br(),htmltools::br(),
                                     download_csv_UI('download_LB_agg'),
                                     htmltools::br(),htmltools::br(),
                                     download_rds_UI('download_LB_agg_rds'),
                                     htmltools::br(),htmltools::br()),
                            shiny::tabPanel("Categorical Aggregate Table",
                                            DT::dataTableOutput('lb_cat_agg_tab_render'),
                                            htmltools::br(),htmltools::br(),
                                            download_csv_UI('download_LB_cat_agg'),
                                            htmltools::br(),htmltools::br(),
                                            download_rds_UI('download_LB_cat_agg_rds'),
                                            htmltools::br(),htmltools::br()),
							shiny::tabPanel("LB Observation",
					shiny::fluidRow(
						htmltools::br(),
						shiny::column(width = 3, offset = 1,
									
									shiny::uiOutput('lb_findings_filter'),
									shiny::actionButton('lb_finding_update', 'Generate/Update Table'

					# 									style = "background-color:#FFFFFF;
					# color:#E31616;
					#                                     border-color:#BEBEBE;
					#                                     border-style:solid;
					#                                     border-width:1px;
					#                                     border-radius:5%;
					#                                     font-weight:bold;
					#                                     font-size:14px;"
														)),
				shiny::column(width = 6, offset = 1,
						DT::dataTableOutput("lb_findingsTable"),
						htmltools::br(),htmltools::br(),
						htmltools::br(),htmltools::br())))
							# shiny::tabPanel("LB Observation Rate",
							# shiny::uiOutput("lb_obs_rate_filter",
							# htmltools::br(),
							# DT::dataTableOutput("")))

                          )
                          ),
                 shiny::tabPanel("BW",
				  ##### BW ----
                                 shiny::fluidRow(

                                   age_unit_input("bw_age_unit"),

                                   shiny::uiOutput("bw_age")),

                                 shiny::fluidRow(
                                   shiny::column(width = 2,
                                                 shiny::actionButton("submit_bw_age", "Update"

											# 						 style="background-color:#FFFFFF;
                                            # color:#E31616;
                                            #                                     border-color:#BEBEBE;
                                            #                                     border-style:solid;
                                            #                                     border-width:1px;
                                            #                                     border-radius:5%;
                                            #                                     font-weight:bold;
                                            #                                     font-size:14px;"
																				))),
                                 htmltools::br(style="line-height: 10px"),


                          shiny::tabsetPanel(
                            shiny::tabPanel("Individual Records",
                                            shiny::checkboxInput("bw_hide_check_column",
                                                                 label = "Show Only Table",
                                                                 value = 0),
                                     htmltools::br(),
                                     shiny::uiOutput("bw_indiv_table"),
                                     htmltools::br(),htmltools::br(),
                                     download_csv_UI('download_BW'),
                                     htmltools::br(),htmltools::br(),
                                     download_rds_UI('download_BW_rds'),
                                     htmltools::br(),htmltools::br()),
                            shiny::tabPanel("Aggregate Table",
                                     DT::dataTableOutput('bw_agg_tab_render'),
                                     htmltools::br(),htmltools::br(),
                                     download_csv_UI('download_BW_agg'),
                                     htmltools::br(),htmltools::br(),
                                     download_rds_UI('download_BW_agg_rds'),
                                     htmltools::br(),htmltools::br()),
                            shiny::tabPanel("Aggregate Plot",
                                            shiny::fluidRow(shiny::column(width = 4,offset = 1,
                                            shiny::uiOutput("bw_table_filter"),
                                            shiny::actionButton("bw_plot_update", "Generate/Update Plot"

											# 					style = "background-color:#FFFFFF;
                                            # color:#E31616;
                                            # border-color:#BEBEBE;
                                            # border-style:solid;
                                            # border-width:1px;
                                            # border-radius:5%;
                                            # font-weight:bold;
                                            # font-size:14px;"
                                            )),

                                              shiny::column(width = 4,
                                            shiny::sliderInput("age_interval", "Choose Interval",
                                                               min = 1,max = 14, value = 3, step = 1),
                                              shiny::selectInput("bw_plot_type", "Choose Plot Type",
                                                                 choices = c("Line with SD (for Selected Interval)",
                                                                             "Original Data (No Interval)")))),
                                            htmltools::br(),htmltools::br(),
                                            plotly::plotlyOutput("bw_agg_plot", height = "600px")))),
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

      shiny::observeEvent(input$tour, {
		  guide$init()$start()

	  })
	  
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

##### Animal List ----
    # Get the list of studies and animals based on new/changed filter criterion
    animalList<-shiny::eventReactive(
      input$refreshData, {
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

    # ROUTE render
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
      #convert character to factor to make filter work
      animal_df <- animal_df %>% dplyr::mutate_if(is.character,as.factor)
      # Associate table header with labels
      headerCallback <- tooltipCallback(tooltip_list = getTabColLabels(animal_df))
      animal_df <- DT::datatable(
        animal_df,
        class = "cell-border stripe",
        filter = list(position = 'top'),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center; font-size: 20px; color: black",
          "Table :", htmltools::strong("Filtered Control Animal")
        ),
        options = list(
          dom = "lfrtip",
          scrollY = TRUE,
          scrollX=TRUE,
          pageLength = 10,
          headerCallback= DT::JS(headerCallback),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")
          ))
      animal_df
      })

    # call module to download csv data
    shiny::callModule(download_csv, id = "download_filter_animal",
                      data = animalList,
                      filename='filtered_Control_Animal')
    # call module to download rds data
    shiny::callModule(download_rds, id = "download_filter_animal_rds",
                      data = animalList,
                      filename='filtered_Control_Animal')


    ########################### MI TAB #######################################

    #### MI age range ----

    animal_list_age_range_mi <- shiny::reactive({
      animal_list <- animalList()
      age_range <- range(animal_list[["DS_AGEDAYS"]], na.rm = TRUE)
      age_range

    })

    # age filter control
    output$mi_age <- shiny::renderUI({

      age_range <- animal_list_age_range_mi()
      max_range <- age_range[2]
      min_range <- age_range[1]
      if(input$mi_age_unit=="Days") {
        min_range <- min_range
        max_range <- max_range
      } else if (input$mi_age_unit=="Weeks") {
        min_range <- floor(min_range/7)
        max_range <- ceiling(max_range/7)
      } else if (input$mi_age_unit=="Months") {
        min_range <- floor(min_range/(365/12))
        max_range <- ceiling(max_range/(365/12))
}
      shiny::column(width = 4,
                    shiny::sliderInput("mi_age_range", label = "Select Age Range",
                                       min = min_range, max=max_range, value = c(min_range, max_range)))
    })
# get animal from age filter
    MI_subject_list <- shiny::eventReactive(input$submit_mi_age,{
      animal_list <- animalList()
      age_range <- input$mi_age_range
      if(input$mi_age_unit=="Days") {
        age_range <- age_range
      } else if (input$mi_age_unit=="Weeks") {
        age_range <- age_range*7
      } else if (input$mi_age_unit=="Months") {
        age_range <- ceiling(age_range*(365/12))
      }
      range_filter <- animal_list[data.table::between(DS_AGEDAYS, age_range[1], age_range[2])]
      range_filter
    })

    ###### MI findings table ----

    ###### get MI_findings whole table ----
   MiFindings_filter_table <- shiny::reactive({
     df <- MiFindings_table(MI_subject_list(), input$MISPEC)
     df
   })

    # render MI findings filter
    output$mi_findings_filter <- shiny::renderUI({
      df <- MiFindings_filter_table()
      df_route <- unique(df[['ROUTE']])
      df_species <- unique(df[['SPECIES']])
      df_strain <- unique(df[['STRAIN']])
      df_sex <- unique(df[['SEX']])
      shiny::fluidRow(addUIDep(shiny::selectizeInput("mi_route",
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
                                         )),
                      addUIDep(shiny::selectizeInput("mi_sex",
                                         "Select Sex:",
                                         df_sex,
                                         selected=df_sex,
                                         multiple=TRUE,
                                         options=list(plugins=list('drag_drop','remove_button'))
                                         )))
      })


    #### update MI findings selection from choices selected

    # update species and strain when route selected
    shiny::observeEvent(input$mi_route, {
      df <- MiFindings_filter_table()
      df <- df[ROUTE %in% input$mi_route, ]
      df_species <- unique(df[['SPECIES']])
      df_strain <- unique(df[['STRAIN']])
      shiny::updateSelectizeInput(session = session, inputId = "mi_species", choices = df_species )
      shiny::updateSelectizeInput(session = session, inputId = "mi_strain", choices = df_strain)
    })

    # update strain when species selected
    shiny::observeEvent(input$mi_species, {
      df <- MiFindings_filter_table()
      df <- df[ROUTE %in% input$mi_route & SPECIES %in% input$mi_species, ]
      df_strain <- unique(df[['STRAIN']])
      shiny::updateSelectizeInput(session = session, inputId = "mi_strain", choices = df_strain)
    })

    shiny::observeEvent(input$mi_strain, {
      df <- MiFindings_filter_table()
      df <- df[ROUTE %in% input$mi_route & SPECIES %in% input$mi_species & STRAIN %in% input$mi_strain, ]
      df_sex <- unique(df[['SEX']])
      shiny::updateSelectizeInput(session = session, inputId = "mi_sex", choices = df_sex)
    })


    ## MI finding table after filter
    Mi_finding_table_after_filter <- shiny::eventReactive(input$mi_finding_update, {
      df <- MiFindings_filter_table()
      df <- df[ROUTE %in% input$mi_route & STRAIN %in% input$mi_strain & SPECIES %in% input$mi_species & SEX %in% input$mi_sex,]
      df
    })

    ###### MI findings table after filter applied ----
    findings_table_after_filter <- shiny::reactive({
      shiny::req(input$mi_finding_update)
      finalFindings <- Mi_finding_table_after_filter()
      findingsCount <- finalFindings %>%
        dplyr::distinct(STUDYID, USUBJID, MISTRESC) %>%
        dplyr::count(MISTRESC) %>%
        dplyr::arrange(-n)
      findingsCount$Incidence <- (findingsCount$n / length(unique(finalFindings$USUBJID))) * 100
      findingsCount$Incidence <- round(findingsCount$Incidence, 2)
      findingsCount <- dplyr::select(findingsCount, -n)
      findingsCount
      })

    ###### Render  MI findings table ----
    output$findingsTable <- DT::renderDataTable(server = T,{
      findings <- findings_table_after_filter()
      # DT::formatPercentage() function applied later, function multiply incidence by 100,
      # so Incidence divide by 100 here
      findings$Incidence <- findings$Incidence/100
      findings_name <- paste0("MI Findings_",input$MISPEC)
      findings_name_tab <- paste0("MI Findings: ",input$MISPEC)
      findings <- DT::datatable(findings,
        class = "cell-border stripe",
        extensions = list("Buttons" = NULL,
                          "ColReorder" = NULL),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center; font-size: 20px; color: black",
          "Table :", htmltools::strong(findings_name_tab)
        ),
        options = list(
          dom = "lrtipB",
          buttons=list(list(
            extend = 'collection',
            buttons = list(list(extend='csv',
                                filename = findings_name),
                           list(extend='excel',
                                filename = findings_name),
                           list(extend='pdf',
                                pageSize = 'A4',
                                orientation = 'landscape',
                                filename= findings_name)),
            text = 'Download'
          )),
          colReorder = TRUE,
          scrollY = TRUE,
          pageLength = nrow(findings),
          lengthChange = FALSE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")))
      findings <- DT::formatPercentage(findings, "Incidence", 2)
      findings
      })

    ###### get MI individual records table ----
    MI_subject <- shiny::reactive({
      animal_list <- MI_subject_list()
      mi_sub <- sendigR::getSubjData(dbToken = .sendigRenv$dbToken,
                                     domain = 'mi',
                                     animalList =  animal_list)
      mi_sub
    })

    # function to get selected columns in MI Individual table
    MI_column <- shiny::reactive({
      if (nrow(MI_subject())>0) {
        get_col_name <- colnames(MI_subject())
        order_to_match <- get_col_name[order(match(get_col_name, mi_col_names))]
      } else{
        order_to_match <- mi_col_names
      }
      order_to_match
    })
    #
    table_to_show <- shiny::reactive({
      tabl <- MI_subject()
      tabl <- subset(tabl, select=input$filter_column)
      tabl
    })
    #
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
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             headerCallback= DT::JS(headerCallback),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")))
      tab
      })

    ####### Download MI individual table csv and rds ----
    shiny::callModule(download_csv, id = "download_MI_individual",
                      data=table_to_show, filename="MI_Individual_Table")
    shiny::callModule(download_rds, id="download_MI_individual_rds",
                      data=table_to_show, filename="MI_Individual_Table")

    ###### generate MI Aggregate table -------
    MI_agg_table <- shiny::reactive({
      animal_list <- MI_subject_list()
      mi_sub <- MI_subject()
      grpByCols <- c('MISPEC', 'SPECIES', 'STRAIN',  'SEX','ROUTE','MISTRESC')
      domainData <- merge(animal_list,
                          mi_sub,
                          on = c('STUDYID', 'USUBJID'),
                          allow.cartesian = TRUE)
      # make uppercase
      domainData$MISPEC <- toupper(domainData$MISPEC)
      domainData$MISTRESC <- toupper(domainData$MISTRESC)
      # remove missing/null values
      domainData <- domainData[MISTRESC!=""]

      # apply aggDomain function from sendDB_shiny.R file, this count Incidence
      shiny::isolate(tableData <- aggDomain(domainData, grpByCols,
                                            includeUncertain=input$INCL_UNCERTAIN))

      # find number of unique subject grouped by 'MISPEC', 'SPECIES', 'STRAIN',  'SEX','ROUTE'
      tissueCounts <- domainData[, list(Animals.In.MISPEC=length(unique(USUBJID))),
                                 by=c('MISPEC', 'SPECIES', 'STRAIN',  'SEX','ROUTE')]

      # merge incidence count and unique subject number from tableData and tissueCount table
      tableData <- merge(tableData, tissueCounts,
                         by=c('MISPEC', 'SPECIES', 'STRAIN',  'SEX','ROUTE'))
      # add Incidence variable, Divide number of incidence (N) by number of unique subject (Animal.In.MISPEC)
      # then multiply by 100
      tableData[, Incidence:=round(((N/Animals.In.MISPEC)*100),2)]
      #remove Animal.In.MISPEC column from tableData
      tableData[, Animals.In.MISPEC:=NULL]
      return(tableData)
      })

   ###### MI aggregate table ----
    output$mi_agg_tab <- DT::renderDT(server = T,{
      tableData <- MI_agg_table()
      tableData <- tableData %>%
        dplyr::mutate_if(is.character, as.factor)
      # DT::formatPercentage() function applied later, function multiply Incidence by 100,
      # so Incidence divide by 100 here
      tableData$Incidence <- tableData$Incidence/100
      # Associate table header with labels
      headerCallback <- tooltipCallback_agg(tooltip_list = getTabColLabels(tableData))
      tab <- DT::datatable(tableData,
      rownames = FALSE,
      class = "cell-border stripe",
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtip",
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             headerCallback= DT::JS(headerCallback),
                             columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}"),
                               rowsGroup = list(0,1,2,3,4)))

      tab <- DT::formatPercentage(table = tab, columns = "Incidence", digits = 2)
    path <- dt_extension
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0",
      path, script = "dataTables.rowsGroup.js")
    tab$dependencies <- c(tab$dependencies, list(dep))
    tab
      })

    ####### Download MI aggregate table csv and rds ----
    shiny::callModule(download_csv, id = "download_MI_agg",
                      data=MI_agg_table, filename="MI_Aggregate_Table")
    shiny::callModule(download_rds, id="download_MI_agg_rds",
                      data=MI_agg_table, filename="MI_Aggregate_Table")

    #### LB TAB #######################################

    ###### get LB  SUBJECT table ----
    get_lb_subj <- shiny::reactive({
      animal_list <- animalList()
      lb_sub <- sendigR::getSubjData(dbToken = .sendigRenv$dbToken, domain = 'lb',
                                     animalList =  animal_list)
      lb_sub <- sendigR::getFindingsSubjAge(dbToken = .sendigRenv$dbToken,
                                            findings = lb_sub, animalList = animal_list)
      lb_sub
    })

    # get finding age range
    animal_list_age_range_lb <- shiny::reactive({
      animal_list <- get_lb_subj()
      age_range <- range(animal_list[["AGEDAYS"]], na.rm = TRUE)
      age_range

    })

    # age filter control
    output$lb_age <- shiny::renderUI({

      age_range <- animal_list_age_range_lb()
      max_range <- age_range[2]
      min_range <- age_range[1]
      if(input$lb_age_unit=="Days") {
        min_range <- min_range
        max_range <- max_range
      } else if (input$lb_age_unit=="Weeks") {
        min_range <- floor(min_range/7)
        max_range <- ceiling(max_range/7)
      } else if (input$lb_age_unit=="Months") {
        min_range <- floor(min_range/(365/12))
        max_range <- ceiling(max_range/(365/12))
      }
      shiny::column(width = 4,
                    shiny::sliderInput("lb_age_range", label = "Select Age Range",
                                       min = min_range, max=max_range, value = c(min_range, max_range)))

    })


### filter animal ----
    LB_subject <- shiny::eventReactive(input$submit_lb_age,{
      animal_list <- get_lb_subj()
      age_range <- input$lb_age_range
      if(input$lb_age_unit=="Days") {
        age_range <- age_range
      } else if (input$lb_age_unit=="Weeks") {
        age_range <- age_range*7
      } else if (input$lb_age_unit=="Months") {
        age_range <- ceiling(age_range*(365/12))
      }
      range_filter <- animal_list[data.table::between(AGEDAYS, age_range[1], age_range[2])]
      range_filter
    })


    # function to get selected columns in LB Individual table
    LB_column <- shiny::reactive({
      if (nrow(LB_subject())>0) {
        get_col_name <- colnames(LB_subject())
      get_col_name}
    })
    #
    lb_table_to_show <-shiny::reactive({
      tabl <- LB_subject()
      tabl <- subset(tabl, select=input$lb_filter_column)
      tabl
    })
    #
    lb_selected_column_to_show <- shiny::reactive({
      col_selected <- intersect(lb_col_names_selected,LB_column())
      col_selected
      })

    ###### LB individual record table UI with hide/show side column ----

    output$lb_indiv_table <- shiny::renderUI({
      if (nrow(LB_subject())>0) {
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
      }})

    #### update selected column in LB individual table
    shiny::observeEvent(input$lb_hide_check_column, {
      shiny::updateCheckboxGroupInput(
        session = session,
        inputId = 'lb_filter_column',
        selected = input$lb_filter_column
      )
      })

    shiny::observeEvent(input$submit_lb_age, {
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
      tab <- DT::datatable(tab,
        filter = list(position = 'top'),
        options = list(
          dom = "lfrtip",
          scrollY = TRUE,
          scrollX = TRUE,
          pageLength = 25,
         headerCallback= DT::JS(LB_headerCallback),
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )))
      tab
      })

    ####### Download LB individual table csv and rds ----
    shiny::callModule(download_csv, id = "download_LB_individual",
                      data=lb_table_to_show, filename="LB_Individual_Table")
    shiny::callModule(download_rds, id="download_LB_individual_rds",
                      data=lb_table_to_show, filename="LB_Individual_Table")


    ###### LB Numerical aggregate table ----

    LB_agg_table <- shiny::reactive({
      animal_list <- animalList()
      lb_sub <- LB_subject()
      domainData <- merge(animal_list, lb_sub,
                          by = c('STUDYID', 'USUBJID'), all=T)
      domainData <- domainData[LBSTRESN!="", ]
      shiny::isolate(tableData <- aggDomain_bw_lb(domainData = domainData,
                                                  domain = 'lb', input$INCL_UNCERTAIN))
      tableData
      })
    ###### render LB aggregate table -----
    output$lb_agg_tab_render <- DT::renderDataTable(server = T,{
      tableData <- LB_agg_table()
      tableData <- tableData %>%
        dplyr::mutate_if(is.character, as.factor)
      # Associate table header with labels
      headerCallback <- tooltipCallback_agg(tooltip_list = getTabColLabels(tableData))
      tab <- DT::datatable(tableData,
	  rownames = FALSE,
	  class = "cell-border stripe",
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtip",
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             headerCallback= DT::JS(headerCallback),
                             columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}"),
							   rowsGroup = list(0,1,2,3,4)))
      tab <- DT::formatRound(table = tab,columns = c(8,9),digits = 2)
	  path <- dt_extension # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0",
      path, script = "dataTables.rowsGroup.js")
    tab$dependencies <- c(tab$dependencies, list(dep))

      tab
      })

    ####### Download Lb Aggregate table csv and rds ----
    shiny::callModule(download_csv, id = "download_LB_agg",
                      data=LB_agg_table, filename="LB_Aggregate_Table")
    shiny::callModule(download_rds, id="download_LB_agg_rds",
                      data=LB_agg_table, filename="LB_Aggregate_Table")

## domain data for LB categorical aggregation table

lb_domain_data <- shiny::reactive({
	 animal_list <- animalList()
      lb_sub <- LB_subject()

      domainData <- merge(animal_list, lb_sub,
                          by = c('STUDYID', 'USUBJID'), allow.cartesian=TRUE)
      domainData <- domainData[is.na(LBSTRESN), ]

      grpByCols <- c('LBSPEC', 'SPECIES', 'STRAIN',  'SEX','ROUTE','LBCAT', 'LBTEST', 'LBSTRESC')
select_cols <- c(
    "STUDYID",
    "USUBJID",
    "STSTDTC",
    "SDESIGN",
    "TCNTRL",
    "RFSTDTC",
    "DM_AGEDAYS",
    "DSDECOD",
    "DS_AGEDAYS",
    "SEX",
    "SPECIES",
    "STRAIN",
    "ROUTE",
    "NO_AGE_MSG",
    "LBSPEC",
    "LBTESTCD",
    "LBTEST",
    "LBSTRESC",
    "LBCAT"
)
      domainData <- domainData[LBSTRESC!="", ..select_cols]
	  domainData

})

############## LB observation ----- 


output$lb_findings_filter  <- shiny::renderUI({
	df <- lb_domain_data()
	 df_lbspec <- unique(df[["LBSPEC"]])
	 df_lbtestcd <- unique(df[["LBTESTCD"]])
      df_route <- unique(df[['ROUTE']])
      df_species <- unique(df[['SPECIES']])
      df_strain <- unique(df[['STRAIN']])
      df_sex <- unique(df[['SEX']])
      shiny::fluidRow(
		  		  	shiny::selectInput("lb_spec",
                                         "Select Organ Specimen:",
                                         choices=df_lbspec
                                        #  selected="",
                                        #  multiple=FALSE
                                        #  options=list(plugins=list('drag_drop','remove_button')
                                        #  )
										 ),
					shiny::selectInput("lb_lbtestcd",
                                         "Select Test Code:",
                                         choices=df_lbtestcd
                                        #  selected=df_lbtestcd
                                        #  multiple=TRUE,
                                        #  options=list(plugins=list('drag_drop','remove_button')
                                        #  )
										 ),
		  			addUIDep(shiny::selectizeInput("lb_route",
                                         "Select Route:",
                                         choices=df_route,
                                        #  selected=df_route,
                                         multiple=TRUE,
                                         options=list(plugins=list('drag_drop','remove_button')
                                         ))),
                      addUIDep(shiny::selectizeInput("lb_species",
                                         "Select Species:",
                                         df_species,
                                         selected=df_species,
                                         multiple=TRUE,
                                         options=list(plugins=list('drag_drop','remove_button'))
                                         )),
                      addUIDep( shiny::selectizeInput("lb_strain",
                                         "Select Strain:",
                                         df_strain,
                                         selected=df_strain,
                                         multiple=TRUE,
                                         options=list(plugins=list('drag_drop','remove_button'))
                                         )),
                      addUIDep(shiny::selectizeInput("lb_sex",
                                         "Select Sex:",
                                         df_sex,
                                         selected=df_sex,
                                         multiple=TRUE,
                                         options=list(plugins=list('drag_drop','remove_button'))
                                         )))

})

	shiny::observeEvent(input$lb_spec, {
		df <- lb_domain_data()
		df <- df[LBSPEC %in% input$lb_spec,]
		df_lbtestcd <- unique(df[["LBTESTCD"]])
		shiny::updateSelectInput(session = session,
		inputId = "lb_lbtestcd",
		choices = df_lbtestcd)

	})

	shiny::observeEvent(input$lb_lbtestcd, {
		df <- lb_domain_data()
		df <- df[LBTESTCD %in% input$lb_lbtestcd,]
		df_route <- unique(df[["ROUTE"]])
		shiny::updateSelectInput(session = session,
		inputId = "lb_route",
		choices = df_route)

	})
    # update species and strain when route selected
    shiny::observeEvent(input$lb_route, {
      df <- lb_domain_data()
      df <- df[ROUTE %in% input$lb_route, ]
      df_species <- unique(df[['SPECIES']])
      df_strain <- unique(df[['STRAIN']])
      shiny::updateSelectizeInput(session = session,
	   inputId = "lb_species", choices = df_species )
      shiny::updateSelectizeInput(session = session, inputId = "lb_strain", choices = df_strain)
    })

    # update strain when species selected
    shiny::observeEvent(input$lb_species, {
      df <- lb_domain_data()
      df <- df[ROUTE %in% input$lb_route & SPECIES %in% input$lb_species, ]
      df_strain <- unique(df[['STRAIN']])
      shiny::updateSelectizeInput(session = session, inputId = "lb_strain", choices = df_strain)
    })

    shiny::observeEvent(input$lb_strain, {
      df <- lb_domain_data()
      df <- df[ROUTE %in% input$lb_route & SPECIES %in% input$lb_species & STRAIN %in% input$lb_strain, ]
      df_sex <- unique(df[['SEX']])
      shiny::updateSelectizeInput(session = session, inputId = "lb_sex", choices = df_sex)
    })


    ## MI finding table after filter
    lb_finding_table_after_filter <- shiny::eventReactive(input$lb_finding_update, {
      df <- lb_domain_data()
	  df <- df[LBSPEC %in% input$lb_spec & LBTESTCD %in% input$lb_lbtestcd]
      df <- df[ROUTE %in% input$lb_route & STRAIN %in% input$lb_strain & SPECIES %in% input$lb_species & SEX %in% input$lb_sex,]
      df
    })



get_lb_observation_count <- shiny::eventReactive(input$lb_finding_update,{
	df <- lb_finding_table_after_filter()
	df <- create_lb_cat_agg_table(df)
	df <- df[!duplicated(LBSTRESC), .(LBSTRESC, Incidence)]
	df
})

output$lb_findingsTable  <- DT::renderDataTable({
	df <- get_lb_observation_count()  
	 tab <- DT::datatable(df,
	 						extensions = list("Buttons" = NULL),
                        #    filter = list(position = 'top'),
                           options = list(
                             dom = "lrtipB",
							   buttons=list(list(
            					extend = 'collection',
								buttons = list(list(extend='csv',
													filename = "LB categorical Incidence"),
											list(extend='excel',
													filename = "LB categorical Incidence"),
											list(extend='pdf',
													pageSize = 'A4',
													orientation = 'landscape',
													filename= "LB categorical Incidence")),
								text = 'Download'
							)),

                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                            #  headerCallback= DT::JS(headerCallback),
                             columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))
		tab <- DT::formatPercentage(table = tab, columns = "Incidence", digits = 2)
	tab
})




    ####### LB categorical agg table #####




    LB_cat_agg_table <- shiny::reactive({
      df <- lb_domain_data()

    #   grpByCols <- c('LBSPEC', 'SPECIES', 'STRAIN',  'SEX','ROUTE','LBCAT', 'LBTEST', 'LBSTRESC')
	  group_by_cols <- c("SPECIES", "STRAIN", "SEX", "ROUTE", "LBSPEC",'LBCAT', "LBTESTCD")

      df <- df[LBSTRESC!=""]

      get_table <- df %>% dplyr::group_by_at(group_by_cols) %>%
	   dplyr::group_modify(~ create_lb_cat_agg_table(.x))

	   get_table_col <- c("LBSPEC", "SPECIES", "STRAIN", "ROUTE", "SEX",
	   "LBCAT" ,"LBTESTCD","LBTEST","LBSTRESC", "Incidence", "Animal_Count")
		get_table <- data.table::as.data.table(get_table)
	
	   
	   get_table <- get_table[, ..get_table_col]
	   get_table <- get_table[!duplicated(get_table)]



      # apply aggDomain function from sendDB_shiny.R file, this count Incidence
    #   shiny::isolate(tableData <- aggDomain(domainData, grpByCols,
    #                                         includeUncertain=input$INCL_UNCERTAIN))

     
    #   tissueCounts <- domainData[, list(Animals.In.LBSPEC=length(unique(USUBJID))),
    #                              by=c('LBSPEC', 'SPECIES', 'STRAIN',  'SEX','ROUTE','LBCAT', 'LBTEST')]

   
    #   tableData <- merge(tableData, tissueCounts,
    #                      by=c('LBSPEC', 'SPECIES', 'STRAIN',  'SEX','ROUTE','LBCAT', 'LBTEST'))
     
    #   tableData[, Incidence:=round(((N/Animals.In.LBSPEC)*100),2)]
    #   tableData[, Animals.In.LBSPEC:=NULL]
      get_table
    })


    ###### Render LB cat agg table ####

    output$lb_cat_agg_tab_render <- DT::renderDataTable(server = T,{
      tableData <- LB_cat_agg_table()
      tableData <- tableData %>%
        dplyr::mutate_if(is.character, as.factor)
      
      # Associate table header with labels
      headerCallback <- tooltipCallback_agg(tooltip_list = getTabColLabels(tableData))
      tab <- DT::datatable(tableData,
	  				rownames = FALSE,
					  class = "cell-border stripe",
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtip",
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             headerCallback= DT::JS(headerCallback),
                             columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}"),
							  rowsGroup = list(0,1,2,3,4,5,6,7)))
      tab <- DT::formatPercentage(table = tab, columns = "Incidence", digits = 2)
	  path <- dt_extension
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0",
      path, script = "dataTables.rowsGroup.js")
    tab$dependencies <- c(tab$dependencies, list(dep))
      tab
    })

    ####### Download LB cat aggregate table csv and rds ----
    shiny::callModule(download_csv, id = "download_LB_cat_agg",
                      data=LB_cat_agg_table, filename="LB_cat_Aggregate_Table")
    shiny::callModule(download_rds, id="download_LB_cat_agg_rds",
                      data=LB_cat_agg_table, filename="LB_cat_Aggregate_Table")
#############


    ###### get BW individual records table ----
    get_bw_subj <- shiny::reactive({
      animal_list <- animalList()
      bw_sub <- sendigR::getSubjData(dbToken = .sendigRenv$dbToken,
                                     domain = 'bw',
                                     animalList =  animal_list)
      bw_sub <- sendigR::getFindingsSubjAge(dbToken = .sendigRenv$dbToken,
                                            findings = bw_sub, animalList = animal_list)
      bw_sub
      })


    # get finding age range
    animal_list_age_range_bw <- shiny::reactive({
      animal_list <- get_bw_subj()
      age_range <- range(animal_list[["AGEDAYS"]], na.rm = TRUE)
      age_range

    })

    # age filter control
    output$bw_age <- shiny::renderUI({

      age_range <- animal_list_age_range_bw()
      max_range <- age_range[2]
      min_range <- age_range[1]
      if(input$bw_age_unit=="Days") {
        min_range <- min_range
        max_range <- max_range
      } else if (input$bw_age_unit=="Weeks") {
        min_range <- floor(min_range/7)
        max_range <- ceiling(max_range/7)
      } else if (input$bw_age_unit=="Months") {
        min_range <- floor(min_range/(365/12))
        max_range <- ceiling(max_range/(365/12))

      }
      shiny::column(width = 4,
                    shiny::sliderInput("bw_age_range", label = "Select Age Range",
                                       min = min_range, max=max_range, value = c(min_range, max_range)))
    })


# get filtered animal
    BW_subject <- shiny::eventReactive(input$submit_bw_age,{
      animal_list <- get_bw_subj()
      age_range <- input$bw_age_range
      if(input$bw_age_unit=="Days") {
        age_range <- age_range
      } else if (input$bw_age_unit=="Weeks") {
        age_range <- age_range*7
      } else if (input$bw_age_unit=="Months") {
        age_range <- ceiling(age_range*(365/12))
      }
      range_filter <- animal_list[data.table::between(AGEDAYS, age_range[1], age_range[2])]
      range_filter
    })

    # function to get selected columns in BW Individual table
    BW_column <- shiny::reactive({
      if (nrow(BW_subject())>0) {
        get_col_name <- colnames(BW_subject())
      get_col_name}
    })
    #
    bw_table_to_show <- shiny::reactive({
      tabl <- BW_subject()
      tabl <- subset(tabl, select=input$bw_filter_column)
      tabl
    })
    #
    bw_selected_column_to_show <- shiny::reactive({
      col_selected <- intersect(bw_col_names_selected,BW_column())
      col_selected})

    ###### BW individual record table UI with hide/show side column ----

    output$bw_indiv_table <- shiny::renderUI({
      if (nrow(BW_subject())>0) {
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
    }})

    #### update selected column in bw individual table
    shiny::observeEvent(input$bw_hide_check_column, {
      shiny::updateCheckboxGroupInput(
        session = session,
        inputId = 'bw_filter_column',
        selected = input$bw_filter_column
      )
    })

    shiny::observeEvent(input$submit_bw_age, {
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
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             headerCallback= DT::JS(headerCallback),
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
      # remover terminal body weight
      bw_sub <- bw_sub[BWTESTCD!="TERMBW", ]
      domainData <- merge(animal_list, bw_sub, by = c('STUDYID', 'USUBJID'),
                          all=T, suffixes = c("_Control_animal", "_BW_AGE"))
      shiny::isolate(tableData <- aggDomain_bw_lb(domainData = domainData,domain = 'bw',
                                                  includeUncertain =input$INCL_UNCERTAIN))
      })

    ###### BW aggregate table render ----
    output$bw_agg_tab_render <- DT::renderDataTable(server = T,{
      tableData <- BW_agg_table()
      tableData <- tableData %>%
        dplyr::mutate_if(is.character, as.factor)
	  tableData <- dplyr::relocate(tableData, AGEDAYS, .after = SEX)
	  if (input$SEX == "All") {
		  rowgroup <- list(0,1,2)
	  } else {
		  rowgroup  <- list(0,1,2,3)
	  }
      # Associate table header with labels
      headerCallback <- tooltipCallback_agg(tooltip_list = getTabColLabels(tableData))
      tab <- DT::datatable(tableData,
	  rownames = FALSE,
	  class = "cell-border stripe",
                           filter = list(position = 'top'),
                           options = list(
                             dom = "lfrtip",
                             scrollY = TRUE,
                             scrollX=TRUE,
                             pageLength = 25,
                             headerCallback= DT::JS(headerCallback),
                             columnDefs = list(list(className = "dt-center", targets = "_all")),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}"),
							   rowsGroup = rowgroup))
      tab <- DT::formatRound(table = tab,columns = c(6,7),digits = 2)
	  path <- dt_extension #folder containing dataTables.rowsGroup.js
      dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0",
      path, script = "dataTables.rowsGroup.js")
      tab$dependencies <- c(tab$dependencies, list(dep))
      tab
      })

    ####### Download BW_Aggregate_Table csv and rds ----
    shiny::callModule(download_csv, id = "download_BW_agg",
                      data=BW_agg_table, filename="BW_Aggregate_Table")
    shiny::callModule(download_rds, id="download_BW_agg_rds",
                      data=BW_agg_table, filename="BW_Aggregate_Table")


    ###### BW Aggregate Plot -----

    output$bw_table_filter <- shiny::renderUI({
      df <- BW_agg_table()
      df_route <- unique(df[['ROUTE']])
      df_species <- unique(df[['SPECIES']])
      df_strain <- unique(df[['STRAIN']])
      df_sex <- unique(df[['SEX']])

      shiny::fluidRow(addUIDep(shiny::selectizeInput("bw_route",
                                                     "Select Route:",
                                                     choices=df_route,
                                                     selected=df_route,
                                                     multiple=TRUE,
                                                     options=list(plugins=list('drag_drop','remove_button')
                                                     ))),
                      addUIDep(shiny::selectizeInput("bw_species",
                                                     "Select Species:",
                                                     df_species,
                                                     selected=df_species,
                                                     multiple=TRUE,
                                                     options=list(plugins=list('drag_drop','remove_button'))
                      )),
                      addUIDep( shiny::selectizeInput("bw_strain",
                                                      "Select Strain:",
                                                      df_strain,
                                                      selected=df_strain,
                                                      multiple=TRUE,
                                                      options=list(plugins=list('drag_drop','remove_button'))
                      )),
                      addUIDep(shiny::selectizeInput("bw_sex",
                                                     "Select Sex:",
                                                     df_sex,
                                                     selected=df_sex,
                                                     multiple=TRUE,
                                                     options=list(plugins=list('drag_drop','remove_button'))
                      ))
                      )
    })

    # update selection in BW aggregate plot ----

    shiny::observeEvent(input$bw_route, {
      df <- BW_agg_table()
      df <- df[ROUTE %in% input$bw_route, ]
      df_species <- unique(df[['SPECIES']])
      df_strain <- unique(df[['STRAIN']])
      df_sex <- unique(df[['SEX']])
      shiny::updateSelectizeInput(session = session, inputId = "bw_species", choices = df_species )
      shiny::updateSelectizeInput(session = session, inputId = "bw_strain", choices = df_strain)
      shiny::updateSelectizeInput(session = session, inputId = 'bw_sex', choices = df_sex)
    })

    shiny::observeEvent(input$bw_species, {
      df <- BW_agg_table()
      df <- df[ROUTE %in% input$bw_route & SPECIES %in% input$bw_species, ]
      df_strain <- unique(df[['STRAIN']])
      df_sex <- unique(df[['SEX']])
      shiny::updateSelectizeInput(session = session, inputId = "bw_strain", choices = df_strain)
      shiny::updateSelectizeInput(session = session, inputId = 'bw_sex', choices = df_sex)

    })

    shiny::observeEvent(input$bw_strain, {
      df <- BW_agg_table()
      df <- df[ROUTE %in% input$bw_route & SPECIES %in% input$bw_species & STRAIN %in% input$bw_strain, ]
      df_sex <- unique(df[['SEX']])
      shiny::updateSelectizeInput(session = session, inputId = 'bw_sex', choices = df_sex)
    })

    ###### get filter data for plot ----
    bw_agg_table_after_filter <- shiny::eventReactive(input$bw_plot_update,{
      df <- BW_agg_table()
      df <- df %>% dplyr::filter(ROUTE %in% input$bw_route,
                                 STRAIN %in% input$bw_strain,
                                 SPECIES %in% input$bw_species,
                                 SEX %in% input$bw_sex)
      df

    })

    ###### Render BW aggregate plot ----

    output$bw_agg_plot <- plotly::renderPlotly({
      shiny::req(input$bw_plot_update)
      df <- bw_agg_table_after_filter()
      df <- df[, list(AGEDAYS, SEX,Mean_BWSTRESN,SD_BWSTRESN,N)]
      df <- na.omit(df, cols=c('AGEDAYS','Mean_BWSTRESN')) # DROP NA VALUES
      df_org <- df

      df_m <- df[SEX=='M']
      df_f <- df[SEX=='F']

      interval <- input$age_interval
      # calculate male
      age_interval <- make_interval(df_m[['AGEDAYS']], interval)
      mean_interval <- meanEveryNth(df_m[['Mean_BWSTRESN']], df_m[['SD_BWSTRESN']], df_m[['N']], interval)
      index <- mean_interval[['Index']]
      Age <- age_interval[index]
      sex <- rep("M", length(Age))
      df_plot_m <- cbind(mean_interval, Age, sex)
      # count female
      age_interval_f <- make_interval(df_f[['AGEDAYS']], interval)
      mean_interval_f <- meanEveryNth(df_f[['Mean_BWSTRESN']], df_f[['SD_BWSTRESN']], df_f[['N']], interval)
      index_f <- mean_interval_f[['Index']]
      Age_f <- age_interval_f[index_f]
      sex_f <- rep("F", length(Age_f))
      df_plot_f <- cbind(mean_interval_f, Age_f,sex_f)
      names(df_plot_f) <- names(df_plot_m)
      df_plot <- rbind(df_plot_m, df_plot_f)
	  print(interval)
	  title_error <- paste0("Mean Body Weight: ",  interval, " AGEDAYS Interval Selected")

      if (input$bw_plot_type=="Line with SD (for Selected Interval)") {
        g <- ggplot2::ggplot(data = df_plot, ggplot2::aes(x=Age, y=Mean, color=sex))+
          ggplot2::geom_line()+
          ggplot2::geom_point()+
          ggplot2::geom_ribbon(ggplot2::aes(ymax=Mean+Weighted_SD, ymin=Mean-Weighted_SD), alpha =.2)+
		  ggplot2::labs(title = title_error, x = "AGEDAYS", y = "Mean BW")+
		  ggplot2::theme_minimal()+
          ggplot2::theme(
			plot.title = ggplot2::element_text(size = 16L,hjust = 0.5),
            axis.title = ggplot2::element_text(size = 14, face = 'bold'),
            axis.text = ggplot2::element_text(size = 14),
            legend.title = ggplot2::element_text(size = 14),
            legend.text = ggplot2::element_text(size = 14)
          )
      } else {
        g <- ggplot2::ggplot(data = df_org, ggplot2::aes(x=AGEDAYS, y=Mean_BWSTRESN, color=SEX))+
          ggplot2::geom_point()+
		  ggplot2::labs(title = "Mean of Body Weight", x = "AGEDAYS", y = "Mean BW")+
		  ggplot2::theme_minimal()+
          ggplot2::theme(
			plot.title = ggplot2::element_text(size = 16,hjust = 0.5),
            axis.title = ggplot2::element_text(size = 14, face = 'bold'),
            axis.text = ggplot2::element_text(size = 14),
            legend.title = ggplot2::element_text(size = 14),
            legend.text = ggplot2::element_text(size = 14)
          )
      }
      plotly::ggplotly(g)
    })

  ###  get filter criteria from sidebar

    filter_criteria <- shiny::reactive({
      #get routes
      if (length(input$ROUTE) != 0) {
        route <- input$ROUTE
      }
      else {
        route <- GetUniqueRoutes()
      }
      # get species
      if (length(input$SPECIES) != 0) {
        species <- input$SPECIES
      }
      else {
        species <- GetUniqueSpecies()
      }
      # get strain
      if (length(input$SPECIES) == 0) {
        Uspecies <- GetUniqueSpecies()
        strain <- GetUniqueStrains(Uspecies)
      }
      else if ( length(input$SPECIES)!=0 & length(input$STRAIN) !=0){
        strain <- input$STRAIN
      } else {
        strain <- GetUniqueStrains(input$SPECIES)
      }

      # make list
      filter_selected <- list(
        From=as.character(input$STSTDTC[1]),
        To=as.character(input$STSTDTC[2]),
        Design=input$SDESIGN,
        Route=route,
        Species=species,
        Strain=strain,
        Sex=input$SEX,
        Uncertain=input$INCL_UNCERTAIN
      )
      print(filter_selected)
      filter_selected
    })


    ##### Download all data as RData file ----
    output$download_all <- shiny::downloadHandler(
        filename <- function() {
            paste0("All_Table_", Sys.Date(), ".RData")
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

            save(Control_Animal, MI_Findings, MI_Individual, MI_Aggregate,
                LB_Individual, LB_Aggregate,
                BW_Individual, BW_Aggregate, Filtered_option,
                file = file
            )
        }
    )



    # CLose connection to database at end of execution
    shiny::onSessionEnded(function() {
      sendigR::disconnectDB(dbToken)
    })

  }

  ##### Run the application ----
  shiny::shinyApp(ui = ui, server = server)

}

################################################################################
#### Avoid  'no visible binding for global variable' notes from check of package:
MISPEC <- Animals.In.MISPEC <- days <- BWSTRESN <- Mean_BWSTRESN <- Incidence <- NULL
BWTESTCD <- SD_BWSTRESN <- LBSTRESC_TRANS.x <- LBSTRESC_TRANS.y <- SET <- Mean <- NULL
Weighted_SD <- distribution <- ..density.. <- Incidence <- NULL
LBSTRESN <- Animals.In.LBSPEC <- NULL
############################## END ##################################################
