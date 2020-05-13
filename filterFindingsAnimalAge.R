###################################################################################
# Script name   : filterFindingsAnimalAge.R
# Date Created  : 31-Mar-2020
# Documentation : <if relevant, reference to specification document>
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Extrat a set of findings within a specified range of age of 
#                 animals at the time of finding
#
# Description   : A function filterFindingsAnimalAge is defined:
#                 Returns a data table with rows extratced from an input data table
#                 with a set of findings (input parameter 'findings') where the AGE 
#                 falls with a specifige interval (input parameters 'fromAge'/'toAge').
#                 If both a start and end input date are specified - all the rows
#                 from input table where value of AGE is within the interval of the 
#                 specifed start/end age interval are extracted and returned 
#                 - including the values equal to the start/end age values. 
#                 If only a start age is specified - all the rows from input table 
#                 where value of AGE is equal to or greater than the input age are 
#                 extracted and returned.
#                 If only an end age is specified - all the rows from input table 
#                 where value of AGE is equal to or less than the input age are 
#                 extracted and returned.
#                 If specified (input parameter 'inclUncertain') - rows with a 
#                 missing AGE value are included in the set of extracted/returned
#                 rows.
#                 The format of 'fromAge'/'toAge' input parameters are
#                   '<value><age unit>' where unit is one of
#                     - d, day, days
#                     - w, week, wkeeks
#                     - m, month, months
#                     - y, year, years
#                 The unit is case-insensitive. It's allowed with space(s) between
#                 age value and unit.
#                 The input age value(s) is/are converted to days before extraction
#                 of rows from the input data tables using the input value(s) as
#                 filter - using this conversion:
#                     - DAYS 
#                     - WEEKS   : value * 7
#                     - MONTHS  : value * 365/12
#                     - YEARS   : value * 365  
#
# Input         : A data table containing the input set of findings (input parameter 
#                 'findings') - at least yhis variable must be included in the table:
#                   - AGE   - integer (age a time of finding in days)
#
# Output        : A data table containing the rows where the value of AGE is within 
#                 the specified range of ages
#                 If the input parameter 'inclUncertain' is specified as TRUE, the
#                 set of rows where the AGE is missing is included too.
#                 The data table contains the same variables as the input data table.
#
# Parameters    : The function filterFindingsAnimalAge is defined with four input 
#                 parameters:
#                   findings: Manadatory, data table
#                             The table with the findings to filter
#                   fromAge:  Optional (either or both of fromAge and toAge must be filled).
#                             The start of age interval to extract.
#                             Must be in a string in a valid format (see description).
#                   fromAge:  Optional (either or both of fromAge and toAge must be filled).
#                             The end ofage interval to extract.
#                             Must be in a string in a valid format (see description).
#                   inclUncertain:
#                             Optional, boolean (TRUE or FALSE), default: FALSE 
#                             Indicates whether rows where AGE is missing 
#                             shall be included or not in the output data table
#                   
#
# Usage notes   : Examples:
#                   Get rows where AGE is between 12 and 14 weeks:
#                     GetStudyListSTSTDTC(findTab, "12W","14W")
#                   Get rows where AGE is 2 years or younger:
#                     GetStudyListSTSTDTC(findTab,toAge="2 y")
#                   Get rows where AGE is 6 monts or older - include rows missing AGE:
#                     GetStudyListSTSTDTC(findTab,fromAge="6 months", inclUncertain=TRUE)
#
###################################################################################

library(data.table)
library(stringi)

filterFindingsAnimalAge<-function(findings, fromAge=NULL, toAge=NULL, inclUncertain=FALSE)
{
  if ((is.null(fromAge) | isTRUE(is.na(fromAge)) | isTRUE(fromAge=="")) & (is.null(toAge) | isTRUE(is.na(toAge)) | isTRUE(toAge==""))) {
    print("ERROR: A start and/or an end age must be specified")
  } 
  else if (!(inclUncertain %in% c(TRUE,FALSE))) {
    print("ERROR: Parameter Exclusively must be either TRUE or FALSE")
  }
  else {
    # Regular expr patterns for a valid from/to age input values
    agePattern<-"^\\d+"
    ageUnitPattern<-"(d|day|days|w|week|weeks|m|month|months|y|year|years)"
    validPattern<-paste(paste(paste(agePattern, "\\s*", sep=""), ageUnitPattern, sep=""), "{1}\\s*$", sep="")
    
    # Evaluate age input string and calculate age in days - if possible
    calcAgeDays<-function(ageStr) {
      if (!(is.null(ageStr) | isTRUE(is.na(ageStr)) | isTRUE(ageStr==""))) {
        if (grepl(validPattern, ageStr, ignore.case = TRUE )) {
          # Valid age string specified - convert to days an rturn value
          ageVal<-as.numeric(str_extract(ageStr,agePattern))
          ageUnit<-tolower(substr(as.character(stri_extract_all_regex(ageStr,ageUnitPattern, opts_regex = list(case_insensitive = TRUE))),1,1))
          if (ageUnit == 'd') { ageVal}
          else if (ageUnit == 'w') {ageVal * 7}
          else if (ageUnit == 'm') {round(ageVal * 365/12)}
          else if (ageUnit == 'y') {ageVal * 365}
        }
        else {
          # Invalid age string specified - return NULL to indicate that
          NULL
        }
      }
      else {
        # No age string specified - return NA to indicate that
        fromAgeDays<-NA
      }
    }
    
    fromAgeDays<-calcAgeDays(fromAge)
    toAgeDays<-calcAgeDays(toAge)
    
    if (is.null(fromAgeDays) | is.null(fromAgeDays)) {
      print("ERROR: Wrong format of specified start and/or an end age")
    }
    else {
      # Construct filter to use for extraction
      ageFilter<-NA
      if (!(is.na(fromAgeDays))) {
        # The filter condition for the fromAge
        ageFilter<-"AGE >= fromAgeDays"
      }
      if (!(is.na(toAgeDays))) {
        if (is.na(ageFilter)) {
          # only toAge filter part
          ageFilter<-"AGE <= toAgeDays"
        }
        else {
          # Add this filter part to the frodmAge filter part
          ageFilter<-paste(ageFilter," & AGE <= toAgeDays",sep="")
        }
      }
      # Build statement to execute
      stmt=paste(paste("findings[", ageFilter, sep=""),"]", sep="")
      # Execute extraction and return rows
      eval(parse(text=stmt))
    }
  }
}
