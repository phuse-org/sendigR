# Save definitions of SEND tables and columns used internally by package functions

sendIGtables <- as.data.table(read.table(file= "data-raw/send_tables.txt", header = TRUE))
sendIGcolumns <- as.data.table(read.table(file= "data-raw/send_columns.txt", header = TRUE))

usethis::use_data(sendIGcolumns, sendIGtables, internal = TRUE, overwrite = TRUE)
