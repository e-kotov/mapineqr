% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/source-filters.R
\name{mi_source_filters}
\alias{mi_source_filters}
\title{Get column values for filtering a source}
\usage{
mi_source_filters(source_name, year, level, filters = list(), limit = 2500)
}
\arguments{
\item{source_name}{A \code{character} string specifying the source name (f_resource).}

\item{year}{A \code{character} or \code{integer} specifying the year.}

\item{level}{A \code{character} string specifying the NUTS level ("0", "1", "2", or "3").}

\item{filters}{A \verb{named list} where the names are the filter fields and
the values are the selected values for those fields. Default is an empty list.}

\item{limit}{An \code{integer} specifying the maximum number of results to return. Default is 2500. This default should be enough for most uses, as it is well above the number of NUTS 3 regions in the EU. The maximum limited by the API is 10000.}
}
\value{
A \code{tibble} with the fields, labels, and their possible values for filtering.
}
\description{
Fetches the possible filtering values for a given source, year, and NUTS level.
}
\examples{
\donttest{
mi_source_filters(
  source_name = "DEMO_R_FIND2",
  year = 2020,
  level = "2",
  filters = list(unit = "YR")
)
}
}
