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
    ns <- NS(id)
    
   shiny::downloadButton(ns("data_download"), label = "Download .CSV", class = "btn-primary")
}

# server function
download_csv <- function(input, output, session, data, filename) {
    
    output$data_download <- shiny::downloadHandler(
        filename = function() {
            paste0(filename, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
            data <- data()
            utils::write.csv(data, file) # add parentheses to data arg if reactive
        }
    )
}


# module for downloaing rds data
# UI function
download_rds_UI <- function(id) {
    ns <- NS(id)
    
    shiny::downloadButton(ns("data_download"), label = "Download .rds", class = "btn-primary")
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