% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFindingsSubjAge.R
\name{getFindingsSubjAge}
\alias{getFindingsSubjAge}
\title{Add the subject age at finding time - and optionally extract the set of
findings within a specified range of age.}
\usage{
getFindingsSubjAge(
  dbToken,
  findings,
  animalList,
  fromAge = NULL,
  toAge = NULL,
  inclUncertain = FALSE,
  noFilterReportUncertain = TRUE
)
}
\arguments{
\item{dbToken}{Mandatory\cr
Token for the open database connection (see \code{\link{initEnvironment}}).}

\item{findings}{Mandatory, data.table.\cr
A table with the set of input finding rows to process.\cr
The table must include at least columns named
\itemize{
\item \code{STUDYID}
\item \code{USUBJID}
\item \code{DOMAIN}
\item \code{[domain]SEQ}
\item \code{[domain]DY}
\item \code{[domain]DTC}
}
where \code{[domain]} is the name of the actual findings domain - e.g. \code{LBSEQ}, \code{LBDY}
and \code{LBDTC}}

\item{animalList}{Mandatory, data.table.\cr
A data with the set of animals included in the \code{findings} table
(may contain more animals than included in \code{findings}).\cr
The data set must contain at least these columns returned by the function \link{getControlSubj}
\itemize{
\item \code{STUDYID}
\item \code{USUBJID}
\item \code{RFSTDTC}
\item \code{DM_AGEDAYS}
\item \code{NO_AGE_MSG}
}}

\item{fromAge}{Optional, character \cr
The start of age interval to extract.\cr
Must be in a string in this format:\cr
\code{[value][age unit]} where \code{[age unit]} is one of
\itemize{
\item \code{d, day, days}
\item \code{w, week, weeks}
\item \code{m, month, months}
\item \code{y, year, years}
}
The unit is case-insensitive, space(s) between age value and unit is
allowed.}

\item{toAge}{Optional. character \cr
The start of age interval to extract.\cr
Must be in a string in in the same format as described for \code{fromAge}.}

\item{inclUncertain}{Mandatory, boolean.\cr
Only relevant if the \code{fromAge} and/or \code{toAge} is/are not empty.\cr
Indicates whether finding rows for which the age at finding time cannot be
confidently identified, shall be included or not in the output data table.}

\item{noFilterReportUncertain}{Optional, boolean.\cr
Only relevant if the \code{fromAge} and \code{toAge} are empty.\cr
Indicates if the reason should be included if the age at finding time cannot
be confidently decided for an animal.}
}
\value{
The function returns a data.table with columns in this order:
\itemize{
\item All columns contained in the \code{findings} input table (original
order except optional \code{UNCERTAIN_MSG} and \code{NOT_VALID_MSG})
\item \code{AGEDAYS}        (character)\cr
The subject age at finding time calculated in days. Is \code{NA} if thge age
cannot be confidently calculated.
\item \code{UNCERTAIN_MSG}  (character)\cr
Included when parameter \code{inclUncertain=TRUE}.\cr
In case the age at finding time cannot be confidently matched during the
filtering of data, the column contains an indication of the reason.\cr
If any uncertainties have been identified for individual subjects included in
pools for pooled finding rows, one  message for is reported per pool/finding.\cr
Is NA for rows where the age at finding time can be confidently matched.\cr
A non-empty \code{UNCERTAIN_MSG} value generated by this function is merged with
non-empty \code{UNCERTAIN_MSG} values which may exist in the input set of findings
specified in \code{findings} - separated by '|'.
\item \code{NOT_VALID_MSG} (character)\cr
Included when parameter \code{noFilterReportUncertain=TRUE}.\cr
In case the age at finding time cannot be confidently calculated, the column
contains an indication of the reason.\cr
Is NA for rows where age at finding time can be confidently calculated.\cr
A non-empty \code{NOT_VALID_MSG} value generated by this function is merged with
non-empty \code{NOT_VALID_MSG} values which may exist in the input set of findings
\code{findings} - separated by '|'.
}
}
\description{
Returns a data table with the set of findings rows included in the
\code{findings} where the age of subjects at finding time is within the
interval specified in \code{fromAge} to \code{fromAge}.\cr
If the \code{fromAge} and \code{fromAge} are empty (null, na or empty
string), all rows from \code{findings} are returned.
}
\details{
In both situation, the subject age at finding time is calculated into an
additional column \code{AGEDAYS} for each row in \code{findings} combined
with the the additional input data.table \code{animalList} using this
algorithm:
\itemize{
\item Determine the number of study days between study start and findings
\itemize{
\item if \code{findings.[domain]DY} is populated
\itemize{
\item If \code{findings.[domain]DY > 0} then use
\code{findings.[domain]DY - 1}
\item Else use \code{findings.[domain]DY}
}
\item Else If \code{findings.[domain]DTC} is populated compute
\code{animalList.RFSTDTC – findings.[domain]DTC} in days\cr where
\code{animalList.RFSTDTC} is each subject's reference start date
(\code{DM.RFSTDTC})
}
\item Animal age at time of finding is then calculated as
\code{animalList.AGEDAYS + [study days between study start and
        findings]}\cr
where \code{animalList.AGEDAYS} is the  subject age at reference
start date(calculated during extraction of control subjects in
\code{\link{getControlSubj}}.
\item For pooled findings rows - i.e. POOLID is populated instead of
USUBJID - the animal age at time of finding is calculated per animal
included in the each pool and finding.
\itemize{
\item If all calculated ages are equal within a pool and finding,
the calculated age is populated for this pool/finding.
\item If all calculated ages are within the same time internal
(2 days) within a pool and finding, the minimum calculated age
plus 1 day is populated for this pool/finding.
}
}

If both \code{fromAge} and \code{toAge} values are specified - all the rows
from the input table \code{findings} where value of the calculated
\code{AGEDYAS} is within the interval of the specified start/end age interval
are returned - including the values equal to the start/end age values.\cr
If only a \code{fromAge} value is specified - all the rows from the input
table \code{findings} where value of \code{AGEDYAS} equal to or greater than
the input age are returned.\cr
If only a \code{toAge} value is specified - all the rows from input table
\code{findings} where value of AGEDAYS is equal to or less than the input age
are extracted and returned.
The input age value(s) is/are converted to days before extraction of rows
from the input data tables using the input value(s) as filter - using this
conversion:
\itemize{
\item \code{DAYS}
\item \code{WEEKS   : value * 7}
\item \code{MONTHS  : value * 365/12}
\item \code{YEARS   : value * 365}
}

If input parameter \code{inclUncertain=TRUE}, findings rows where the age at
finding time cannot be confidently identified are included in the output set.
These uncertain situations are identified and reported (in column
UNCERTAIN_MSG):
\itemize{
\item No age at reference time  has been calculated for subject
(\code{animalList.AGEDAYS})
\item Reference start time  is missing or contains invalid ISO8601 date
value for subject (\code{animalList.RFSTDTC}).
\item Missing \code{[domain]DY} value and missing or invalid ISO8601 date
\code{[domain]DTC} value for finding
\item For pooled findings:
\itemize{
\item More than two days between minimum and maximum of
\code{animalList.AGEDAYS} for the set of animals in a pool.
\item Different values in \code{animalList.RFSTDTC} for the set of
animals in a pool.
}
}
The same checks are performed and reported in column \code{NOT_VALID_MSG} if
\code{fromAge} and \code{fromAge} are empty and \code{noFilterReportUncertain
= TRUE}.
}
\examples{
\dontrun{
# Extract LB rows for the animals at age between 8 and 12 weeks at finding
# time - include uncertain rows
getFindingsSubjAge(dbToken = db,
                   findings = lb,
                   animalList = animals,
                   fromAge = '8w',
                   toAge = '12w',
                   inclUncertain = TRUE)
# No filtering, just add AGEDAYS to FW rows - do not include messages when
# the AGEDAYS cannot be confidently identified
getFindingsSubjAge(dbToken = db, findings = fw,  animalList = animals,
                   noFilterReportUncertain = FALSE)
}

}
