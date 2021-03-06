% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{allequal}
\alias{allequal}
\title{Determine whether two objects are equal.}
\usage{
allequal(x, y)
}
\arguments{
\item{x}{An object.}

\item{y}{Another object.}
}
\value{
Logical.
}
\description{
Determine whether two objects are equal.
}
\examples{
allequal(data.frame(y = 1:2), data.frame(y = c(2, 4)))
}
