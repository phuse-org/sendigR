################################################################################
## Shiny Module for downloading data
##
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2021-05-20   Yousuf Ali,           Initial version
##
##
################################################################################


# module for downloaing CSV data
# UI function
download_csv_UI <- function(id) {
    ns <- shiny::NS(id)
    shiny::conditionalPanel(
        condition = "input.refreshData!=0",
            shiny::downloadButton(ns("data_download"), label = "Download .CSV")
        )
}

# shiny::downloadButton(ns("data_download"), label = "Download .CSV", class = "btn-primary")

# server function
download_csv <- function(input, output, session, data, filename) {

    output$data_download <- shiny::downloadHandler(
        filename = function() {
            paste0(filename, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
            data <- data()
            utils::write.csv(data, file)
        }
    )
}
#####################################################################################

# module for downloading rds data
# UI function
download_rds_UI <- function(id) {
    ns <- shiny::NS(id)
    shiny::conditionalPanel(
        condition = "input.refreshData!=0",
        shiny::downloadButton(ns("data_download"), label = "Download .rds")
    )
}

# server function
download_rds <- function(input, output, session, data, filename) {

    output$data_download <- shiny::downloadHandler(
        filename = function() {
            paste0(filename, "_", Sys.Date(), ".rds")
        },
        content = function(file) {
            data <- data()
            saveRDS(data, file) # add parentheses to data arg if reactive
        }
    )
}
######################################################################################

#function for slider input for age units
age_unit_input <- function(id) {
    shiny::column(width = 2,
                  shiny::selectInput(id, label = "Select the Unit",
                                     choices = c("Days","Weeks", "Months"),
                                     selected = "Days"))
}


####################################################################################

# module for age range filter in all three domain

# age_filter_UI <- function(id) {
#     ns <- shiny::NS(id)
#     shiny::uiOutput(ns("age_filter"))
# }
#
# age_filter_server <- function(input, output, session, data, domain,age_range) {
#
#
#     output$age_filter <- shiny::renderUI({
#         ns <- session$ns
#
#
#         age_range <- age_range()
#
#
#
# shiny::column(width = 4,
#               shiny::sliderInput(ns("mi_age_filter_range"), label = "Select Age Range",
#                                  min = age_range[1], max=age_range[2], value = age_range, step = 1))
#
#       }
#     )
#     reactive(input$mi_age_filter_range)
#
# }

####################################################################################




