###################################################################################
# Script name   : filterMIFindings.R
# Date Created  : 12-Mar-2020
# Documentation : <if relevant, reference to specification document>
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Extract a set of MI findings for specified MISTRESC and/or MISPEC
#                 value(S)
#
# Description   : The function FilterMIFindings extracts and return a set of 
#                 MI findings from a input set of MI findings which matches the 
#                 criterions specified in in the input parameters mistrescVal and/or
#                 mispecVal.
#                 If a value is specified for input parameter mistrescVal, the set of
#                 rows in the input table is extracted where MISTRESC matches the specified 
#                 parameter value.
#                 If a value is specified for input parameter mispecVal, the set of
#                 rows in the input table is extracted where MISPEC matches the specified 
#                 parameter value.
#                 If both of these input parameters are specified, the set of rows in
#                 the input table is extracted where both MISTRESC and MISPEC match
#                 the respectively input parameter values.
#                 If the specified input value contains one or more *, the filtering of
#                 the variable is done as a regular expression search where each * 
#                 represents zero or more characters - else the filtereing is done 
#                 as a simple 'equal to'.
#                 The filtering of MISTRESC/MISPEC values is done case insensitive in 
#                 all cases.
#                 If the value of input parameter mistrescVal.negate/mispecVal.negate
#                 is TRUE, the filteing using the condition in mistrescVal/mispecVal
#                 is negated - i.e. the rows where MISTRESC/MISPEC doesn't match the 
#                 condition are extracted.
#
# Input         : A data table containing the input set of MI findings (input parameter 
#                 findings). It is expected that it contains all MI variables - but this 
#                 are the required minimun set of variables (used by function) in the table:
#                   - MISTRESC      - character
#                   - MISPEC        - character
#
# Output        : A data table containing the rows matching the specified input values
#                 of MISTRESC and/or MISPEC in the input data table.
#                 The data table contains the same variables as the input data table.
#
# Parameters    : The functionis defined with these input parameters:
#                   mi:           Mandatory, data table
#                                 The data table with the MI findings to filter
#                   mistrescVal:  Optional *), character
#                                 The MISTRESC value to filter for
#                   mispecVal:    Optional *), character
#                                 The MISPEC value to filter for
#                   mistrescVal.negate:  
#                                 Optional, boolean, default=FALSE
#                                 Specifies whether the condition specified in mistrescVal
#                                 shall be negated or not
#                   mispecVal.negate:  
#                                 Optional, boolean, default=FALSE
#                                 Specifies whether the condition specified in mispecVal
#                                 shall be negated or not
#                   *) Either mistrescVal, mispecVal or both must be specified
#
# Usage notes   : Examples:
#                 - Get findings for specimen 'BRAIN': 
#                     FilterMIFindings(dosingMI, mispecVal="brain")
#                 - Get findings where specimen starts with 'GLAND, SALIVARY' and MISTRESC is different from 'NORMAL':
#                     FilterMIFindings(dosingMI, mistrescVal="normal", mispecVal="gland, SALIVARY*", , mistrescVal.negate=TRUE)
#                 - Get findings where value of MISTRESC starts with 'increased':
#                     FilterMIFindings(dosingMI, mistrescVal="increased*")
#
# MISSING: 
#   - Handling of pooled data
###################################################################################


library(data.table)
library(stringr)

FilterMIFindings<-function(mi,
                           mistrescVal=NULL, mistrescVal.negate=FALSE, 
                           mispecVal=NULL, mispecVal.negate=FALSE) {
  
  if (is.null(mistrescVal) & is.null(mispecVal)) {
    print("ERROR: A mistrescVal and/or mispecVal to filter for must be specified")
  } 
  else {
    if (!is.null(mispecVal)) {
      # Build expression for search condition for MISPEC
      if (grepl("\\*", mispecVal)) {
        # Wildcard included in condition - create regular expression to search for (case insensitive)
        mispecExpr<-paste(paste("^", str_replace_all(mispecVal, "\\*", "\\.*"), sep=""), "$", sep="")
        mispecExecStr<-'grepl(mispecExpr, MISPEC, ignore.case = TRUE)'
        if (mispecVal.negate) {
          # Invert the search condition
          mispecExecStr<-paste('!', mispecExecStr, sep='')
        }
      }
      else {
        # Search for exact match (case insensitive)
        mispecExecStr<-paste(paste('tolower(MISPEC) ', ifelse(mispecVal.negate,'!=','=='), sep=''), 'tolower(mispecVal)',sep='')
      }
    }
    if (!is.null(mistrescVal)) {
      # Build expression for search condition for MISTRESC
      if (grepl("\\*", mistrescVal)) {
        # Wildcard included in condition - create regular expression to search for (case insensitive)
        mistrescExpr<-paste(paste("^", str_replace_all(mistrescVal, "\\*", "\\.*"), sep=""), "$", sep="")
        mistrescExecStr<-'grepl(mistrescExpr, MISTRESC, ignore.case = TRUE)'
        if (mistrescVal.negate) {
          # Invert the search condition
          mistrecExecStr<-paste('!', mistrecExecStr, sep='')
        }
      }
      else {
        # Search for exact match (case insensitive)
        mistrescExecStr<-paste(paste('tolower(MISTRESC) ', ifelse(mistrescVal.negate,'!=','=='), sep=''), 'tolower(mistrescVal)',sep='')
      }
    }
    if (is.null(mistrescVal) | is.null(mispecVal)) { 
      # Create the final filter expression based on conditions for both MISPEC and MISTRESC
      ExecStr<-paste(paste('mi[', ifelse(is.null(mistrescVal), mispecExecStr, mistrescExecStr), sep=''), ']', sep='')
    }
    else {
      # Create the final filter expression based on condition for either MISPEC or MISTRESC
      ExecStr<-paste(paste(paste(paste('mi[', mispecExecStr, sep=''), ' & ', sep=''), mistrescExecStr, sep=''), ']', sep='')
    }
    # Execute the filtering and return extracted rows 
    eval(parse(text=ExecStr))
  }
}

######################################################################################################################################
#
# Alternative version of the function using agrep to do approximate matches
# Examples:
#                 - Get findings for specimen 'BRAIN': 
#                     FilterMIFindingsApprox(dosingMI, mispecVal="brain")
#                 - Get findings where specimen starts with 'GLAND, SALIVARY' and MISTRESC is different from 'NORMAL':
#                     FilterMIFindingsApprox(dosingMI, mistrescVal="normal", mispecVal="gland, SALIVARY*", , mistrescVal.negate=TRUE)
#                 - Get findings where value of MISTRESC starts with 'increased':
#                     FilterMIFindingsApprox(dosingMI, mistrescVal="increased*")
########################################################################################################################################

FilterMIFindingsApprox<-function(mi,
                                 mistrescVal=NULL, mistrescVal.negate=FALSE, 
                                 mispecVal=NULL, mispecVal.negate=FALSE) {
  
  if (is.null(mistrescVal) & is.null(mispecVal)) {
    print("ERROR: A mistrescVal and/or mispecVal to filter for must be specified")
  } 
  else {
    if (!is.null(mispecVal)) {
      # Build expression for search condition for MISPEC - include start/end of line indications to ensure we 
      # don't return too many rows macth the intended condition
      mispecExecStr<-paste(paste('agrepl("^', 
                                  ifelse(grepl("\\*", mispecVal),
                                  # Wildcard included in condition - create regular expression to search for
                                  str_replace_all(mispecVal, "\\*", "\\.*"), 
                                  # Search for fixed match
                                  mispecVal), sep=''),
                                 '$", MISPEC, ignore.case = TRUE, fixed = FALSE)', sep='')
      if (mispecVal.negate) {
        # Invert the search condition
        mispecExecStr<-paste('!', mispecExecStr, sep='')
      }
    }
    if (!is.null(mistrescVal)) {
      # Build expression for search condition for MISTRESC - include start/end of line indications to ensure we 
      # don't return too many rows macth the intended condition
      mistrescExecStr<-paste(paste('agrepl("^', 
                                 ifelse(grepl("\\*", mistrescVal),
                                        # Wildcard included in condition - create regular expression to search for
                                        str_replace_all(mistrescVal, "\\*", "\\.*"), 
                                        # Search for fixed match
                                        mistrescVal), sep=''),
                           '$", MISTRESC, ignore.case = TRUE, fixed = FALSE)', sep='')
      if (mistrescVal.negate) {
        # Invert the search condition
        mistrescExecStr<-paste('!', mistrescExecStr, sep='')
      }
    }
    if (is.null(mistrescVal) | is.null(mispecVal)) { 
      # Create the final filter expression based on conditions for both MISPEC and MISTRESC
      ExecStr<-paste(paste('mi[', ifelse(is.null(mistrescVal), mispecExecStr, mistrescExecStr), sep=''), ']', sep='')
    }
    else {
      # Create the final filter expression based on condition for either MISPEC or MISTRESC
      ExecStr<-paste(paste(paste(paste('mi[', mispecExecStr, sep=''), ' & ', sep=''), mistrescExecStr, sep=''), ']', sep='')
    }
    # Execute the filtering and return extracted rows 
    eval(parse(text=ExecStr))
  }
}

