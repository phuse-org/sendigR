% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFindingsPhase.R
\name{getFindingsPhase}
\alias{getFindingsPhase}
\title{Extract a set of findings for a specified study phase - or just add phase
for each animal.}
\usage{
getFindingsPhase(
  dbToken,
  findings,
  phaseFilter = NULL,
  inclUncertain = FALSE,
  noFilterReportUncertain = TRUE
)
}
\arguments{
\item{dbToken}{Mandatory\cr
Token for the open database connection (see \code{\link{initEnvironment}}).}

\item{findings}{Mandatory, data.table.\cr
A data.table with the set of finding rows to process.\cr
The table must include at least columns named
\itemize{
\item STUDYID
\item USUBJID
\item DOMAIN
\item domainSEQ
\item domainDTC
}
where domain is the name of the actual findings domain - e.g. LBSEQ and
LBDTC}

\item{phaseFilter}{Optional, character.\cr
The phase value criterion to be used for filtering of the list of animals.\cr
It can be a single string, a vector or a list of multiple strings.}

\item{inclUncertain}{Mandatory, boolean.\cr
Only relevant if the \code{phaseFilter} is not empty.\cr
Indicates whether finding rows for which the phase cannot be confidently
identified shall be included or not in the output data table.}

\item{noFilterReportUncertain}{Mandatory, boolean.\cr
Only relevant if the \code{phaseFilter} is empty.\cr
Indicates if the reason should be included if the phase cannot be
confidently decided for an animal.}
}
\value{
The function returns a data.table with columns in this order:
\itemize{
\item All columns contained in the \code{findings} input table (original
order except optional UNCERTAIN_MSG and NOT_VALID_MSG)
\item PHASE          (character)
\item UNCERTAIN_MSG  (character)\cr
Included when parameter \code{inclUncertain=TRUE}.\cr
In case the phase cannot be confidently matched during the
filtering of data, the column contains an indication of the reason.\cr
If any uncertainties have been identified for individual subjects included in
pools for pooled finding rows, all messages for subjects per pool/findings
are merged together and reported as one message per pool/finding.\cr
Is NA for rows where phase can be confidently matched.\cr
A non-empty UNCERTAIN_MSG value generated by this function is merged with
non-empty UNCERTAIN_MSG values which may exist in the input set of findings
specified in \code{findings} - separated by '|'.
\item NOT_VALID_MSG (character)\cr
Included when parameter \code{noFilterReportUncertain=TRUE}.\cr
In case the phase cannot be confidently decided, the column
contains an indication of the reason.\cr
Is NA for rows where phase can be confidently decided.\cr
A non-empty NOT_VALID_MSG value generated by this function is merged with
non-empty NOT_VALID_MSG values which may exist in the input set of findings
\code{findings} - separated by '|'.
}
}
\description{
Returns a data table with the set of findings rows included in the
\code{findings} of the phase(s) specified in the \code{phaseFilter}.\cr
If the \code{phaseFilter} is empty (null, na or empty string), all rows from
\code{findings} are returned with the an additional PHASE column.
}
\details{
The logic for the extraction is based on the subject elements and the trial
design domains - for each finding row:
\itemize{
\item The related subject element is found in SE as the row where the
value of domainDTC is within the interval from SESTDTC to SEENDTC
\item The actual EPOCH is found in TA in the row matching the found
element (via the ETCD value)
\item The actual study phase is derived from the EPOCH value matching at
set of text patterns
}
For pooled findings rows - i.e. POOLID is populated instead of USUBJID - the
phase is identified per animal included in the each pool and finding, and if all
identified phases are equal per pool and finding, the identified phase are
returned per pool and finding.

The populated value of a phase is one of:
\itemize{
\item 'Screening'\cr
If TA.EPOCH fulfills one:
\itemize{
\item contains 'pre' followed by one of
['treat','trt','dos',test','study','exposure']
\item contains one of
['acclimat','screen','baseline','allocat','random']
}
\item 'Recovery'\cr
If TA.EPOCH doesn't fulfill the pattern for 'Screening' and fulfills one
of:
\itemize{
\item contains 'recovery'
\item contains 'post' followed by one of
['treat','trt','dos','test','study','exposure']
}
\item 'Treatment'\cr
If TA.EPOCH doesn't fulfill the patterns for 'Screening' or 'Recovery'
and fulfills both:
\itemize{
\item contains one of
['treat','trt','dos','test','exposure']
\item does not contain any of
['off','non'|','free'|','holiday']
}
\item 'Uncertain'\cr
If the TA.EPOCH is empty or does not fulfills any of the requirements
described for the three phases above.
}

If input parameter \code{inclUncertain=TRUE}, findings rows where the phase
cannot be confidently identified are included in the output set. These
uncertain situations are identified and reported (in column UNCERTAIN_MSG):
\itemize{
\item One of the date/time values SESTDTC, SEENDTC or domainDTC is empty
or contains an invalid ISO 8601 value
\item The value of domainDTC is included in more then one SESTDTC/SEENDTC
interval
\item The EPOCH value does not match any of the patterns identifying the
set of possible study phases.
\item Different phases have been identified for individual subjects in a
pool for a given finding
}
The same checks are performed and reported in column NOT_VALID_MSG if
\code{phaseFilter} is empty and \code{noFilterReportUncertain=TRUE}.
}
\examples{
\dontrun{
# Extract LB rows for the Treatment phase - include uncertain rows
getFindingsPhase(dbToken, lb,
                 phaseFilter = 'Treatment',
                 inclUncertain = TRUE)
# No filtering, just add PHASE to FW rows - do not include messages when
# the phase cannot be confidently identified
getFindingsPhase(dbToken, fw,
                 noFilterReportUncertain = FALSE)
}
}
