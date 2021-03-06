% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ladders.R
\name{view}
\alias{view}
\alias{inspect}
\title{View defined ladders in a pretty-printed manner}
\usage{
inspect(id.spec = NULL, .scalade = NULL)
}
\arguments{
\item{id.spec}{Specified ladder ID to inspect. If not specified shows
all available ladders with their associated packages.}

\item{.scalade}{Location of the ladder definition file. Default is NULL.
If NULL (e.g. not set), it first searches for a .Rscalade file in the
project root (as defined by there \code{here} package), and failing that, will
search the user's home directory ("~/.Rscalade").}
}
\value{
Invisibly returns the tibble of ladder definitions. In the
printed output, a checkmark notes that the package is already installed
on your host computer, while a cross signifies the package is not
currently available.
}
\description{
This will pretty-print ladders (all by default, or specific ones)
that have already been defined in ~/.Rscalade.
}
