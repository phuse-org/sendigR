##################################################################################
# Save definitions of SEND tables and columns used internally by package functions
##################################################################################
library(data.table)
library(readxl)

## Metadata for the valid types of databases
validDbTypes <- as.data.table(read.table(file= "data-raw/valid_db_types.txt", header = TRUE))

## SEND IG metadata
sendIGtables <- as.data.table(read.table(file= "data-raw/send_tables.txt", header = TRUE))
sendIGcolumns <- as.data.table(read.table(file= "data-raw/send_columns.txt", header = TRUE))

## Default CDISC SEND CT
# Import content from worksheet named SEND<sep>Terminology<something>
# - include relevant columns and all rows
ctFile <- "data-raw/SEND Terminology.xls"
ctSheets <- readxl::excel_sheets(ctFile)
ctAll <- as.data.table(readxl::read_xls(ctFile,
                                        sheet=ctSheets[grepl('send[_ ]terminology',
                                                             tolower(ctSheets) )]))[,c("Code", "Codelist Code", "CDISC Submission Value")]
setnames(ctAll, c("Codelist Code","CDISC Submission Value"),
                     c("CodelistCode","CDISCSubmissionValue"))
# Extract code list names for code lists used in package
ctUsedCodeLists <- c("DESIGN", "ROUTE", "SEX", "SPECIES", "STRAIN")
CDISCctCodeLists = ctAll[is.na(CodelistCode) &
                           CDISCSubmissionValue %in% ctUsedCodeLists,
                         c('Code', 'CDISCSubmissionValue')]
setnames(CDISCctCodeLists, c("Code","CDISCSubmissionValue"),
                           c("CodelistCode","CodeList"))
# Extract code list values for extracted code lists
CDISCctCodeValues = merge(ctAll[!is.na(CodelistCode),
                                c('CodelistCode','CDISCSubmissionValue')],
                          CDISCctCodeLists[,.(CodelistCode)],
                          by = 'CodelistCode')

## Save data in R/sysdata.rda
usethis::use_data(validDbTypes,
                  sendIGcolumns, sendIGtables,
                  CDISCctCodeLists, CDISCctCodeValues,
                  internal = TRUE,
                  overwrite = TRUE)

