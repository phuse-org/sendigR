# Save definitions of SEND tables and columns used internally by package functions
library(data.table)

sendIGtables <- as.data.table(read.table(file= "data-raw/send_tables.txt", header = TRUE))
sendIGcolumns <- as.data.table(read.table(file= "data-raw/send_columns.txt", header = TRUE))
validDbTypes <- as.data.table(read.table(file= "data-raw/valid_db_types.txt", header = TRUE))

usethis::use_data(sendIGcolumns, sendIGtables, validDbTypes,
                  internal = TRUE,
                  overwrite = TRUE)

