% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{as_numeric}
\alias{as_numeric}
\title{Convert to numeric while suppressing warnings.}
\usage{
as_numeric(x, default = NA_real_)
}
\arguments{
\item{x}{A vector.}

\item{default}{A numeric scalar.}
}
\value{
Numeric vector the same length as x.
}
\description{
Convert to numeric while suppressing warnings.
}
\examples{
as_numeric(c("1", "NA", NA, "b"))
}
