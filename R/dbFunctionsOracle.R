################################################################################
## Database functions specific for Oracle
##  - helper functions called by the published database functions
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2020-12-04   Bo Larsen             Initial version
################################################################################

## Connect function
connectDB_oracle<-function(dbPath, dbUser, dbPwd) {
  return(ROracle::dbConnect(DBI::dbDriver("Oracle"), username=dbUser, password=dbPwd, dbname=dbPath))
}

## Disconnect function
disconnectDB_oracle<-function(dbHandle) {
  return(ROracle::dbDisconnect(dbHandle))
}

## Execute a generic query
# Result data set always returned as data table
#  ## ADD
#  # POSIBILITY FOR MULTIPLE QUERY PARAMS
genericQuery_oracle<-function(dbHandle, queryString, queryParams=NULL) {
  # SQL clause to replace 'in (:nn)' clause to make Oracle possible to execute an 'in' statement
  # using a bind variable with values for the in-list
  inStrReplacement <- "in (select regexp_substr(colval\\1, '[^,]+', 1, column_value) colval\\1
                            from (select :\\1 as colval\\1 from dual) t,
                            table(cast(multiset(select level from dual
                                                connect by level <= regexp_count(colval\\1, ',') + 1
                            ) as sys.odcinumberlist)))"
  # TO BE CHANGED TO JOIN INSTEAD (if it's not an 'not in' or not part of a 'or' construction....):
  #join ( select regexp_substr(colval1, '[^,]+', 1, column_value) colval\\1
  #                          from (select :\\1 as colval\\1
  #                                  from dual) t,
  #                                             table(cast(multiset(select level
  #                                                                   from dual
  #                                                                   connect by level <= regexp_count(colval\\1, ',') + 1 ) as sys.odcinumberlist ) )) bindtab\\1
  #    on bindtab\\1.colval\\1 = <column referenced in clause>;

  if (is.null(queryParams)){
    cur <- ROracle::dbSendQuery(dbHandle, queryString)
  } else {
    # Check if statements contains in statements
    if (grepl('in \\(:\\d+\\)', queryString, ignore.case = TRUE)) {
      # Replace the in statement with correct Oracle syntax
      stmt <- stringr::str_replace_all(queryString,
                                       stringr::regex('in \\(:(\\d+)\\)', ignore_case = TRUE),
                                       inStrReplacement)
      # Ensure in the in-list is a comma separated string
      bindVarVal <- paste0(unlist(list(queryParams)), collapse=',')
    }
    else {
      stmt <- queryString
      bindVarVal <- unname(unlist(list(queryParams)))
    }

    # Create a data frame with the bind var value included for each
    # instance of bind var reference in statement
    # TO BE USED WHEN SUPPORT FOR MULTIPLE QUERY PARAMS IS ADDED:
    #      sort(unique(str_extract_all(stmt, ':\\d*')[[1]]))
    n <- 0
    for (v in stringr::str_extract_all(stmt, ':\\d+')[[1]]) {
      n <- n + 1
      if (n==1)
        bindVarDF <- data.frame(bindVarVal)
      else
        bindVarDF <- cbind(bindVarDF, bindVarVal)
    }
    cur <- ROracle::dbSendQuery(dbHandle, stmt, bindVarDF)
  }

  # Fetch all rows, clear buffer and return data
  queryResult<-data.table::as.data.table(ROracle::fetch(cur))
  ROracle::dbClearResult(cur)

  return(queryResult)
}


################################################################################
## Check if specified table exists in database
#  Returns boolean
dbExistsTable_oracle <- function(dbHandle, dbSchema, table) {
  return(ROracle::dbExistsTable(dbHandle, table, schema = dbSchema))
}

################################################################################
##  Return list of columns in specified database table
dbListFields_oracle <- function(dbHandle, dbSchema, table) {
  return(ROracle::dbListFields(dbHandle, table, schema = dbSchema))
}
