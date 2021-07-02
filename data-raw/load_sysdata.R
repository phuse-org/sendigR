##################################################################################
# Save definitions of SEND tables and columns used internally by package functions
##################################################################################
library(data.table)
library(readxl)

## Metadata for the valid types of databases
validDbTypes <- as.data.table(read.table(file= "data-raw/valid_db_types.txt", header = TRUE))

## SEND IG metadata
sendIGtables <- as.data.table(readxl::read_xls("data-raw/send_tables.xls",
                                                 sheet='send_tables'))
sendIGcolumns <- as.data.table(readxl::read_xls("data-raw/send_columns.xls",
                                                 sheet='send_columns'))


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

## Labels for columns not part of SEND IG added to tables by the package

additionalColumns <- data.table::rbindlist(list(
  # TS/TX parameters
  merge(setnames(ctAll[CodelistCode == ctAll[CDISCSubmissionValue == 'STSPRMCD']$Code, list(Code, CDISCSubmissionValue)], 'CDISCSubmissionValue', 'COLUMN_NAME'),
        setnames(ctAll[CodelistCode == ctAll[CDISCSubmissionValue == 'STSPRM']$Code, list(Code, CDISCSubmissionValue)], 'CDISCSubmissionValue', 'LABEL'),
        by = 'Code')[,`:=` (Code = NULL)],
  # Additional sendigR specific columns
  as.data.table(readxl::read_xls("data-raw/sendigR_columns.xls",
                                 sheet='sendigR_columns'))),
 use.names = TRUE, fill = TRUE)


## Save data in R/sysdata.rda
usethis::use_data(validDbTypes,
                  sendIGcolumns, sendIGtables,
                  additionalColumns,
                  CDISCctCodeLists, CDISCctCodeValues,
                  internal = TRUE,
                  overwrite = TRUE)

