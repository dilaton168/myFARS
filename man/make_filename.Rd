% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{make_filename}
\usage{
make_filename(year)
}
\arguments{
\item{year}{A number, presumably a 4-digit year.}
}
\value{
A string in the form "accident_#.csv.bz2".
}
\description{
Attach a prefix and suffice to a number (year) in a form of "accident_#.csv.bz2".
}
\examples{
make_filename(year = '2013')

\dontrun{
make_filename(year = 'two thousand eighteen') #  error
}

}
