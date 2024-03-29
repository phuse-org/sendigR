% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initSENDFunctions.R
\name{getTabColLabels}
\alias{getTabColLabels}
\title{Get labels for columns in a data.table}
\usage{
getTabColLabels(table)
}
\arguments{
\item{table}{Mandatory\cr
The data.table to get column labels for}
}
\value{
A named vector with each column/label pair. If a column have no
defined label, the label is 'na'
}
\description{
Get labels for columns in a data.table
}
\examples{

\dontrun{
colLabels = getTabColLabels(controlAnimalsAll)
}
}
