% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ladders.R
\name{load}
\alias{load}
\alias{climb}
\title{Attaches packages specified by ladders}
\usage{
climb(id.spec, report_conflicts = FALSE, .scalade = NULL)
}
\arguments{
\item{id.spec}{Specified ladder ID to inspect. If not specified shows
all available ladders with their associated packages.}

\item{report_conflicts}{Whether to show the function conflicts that
arise from the loaded packages. Currently set by default to FALSE
as it can be quite noisy.}

\item{.scalade}{Location of the ladder definition file. Default is NULL.
If NULL (e.g. not set), it first searches for a .Rscalade file in the
project root (as defined by there \code{here} package), and failing that, will
search the user's home directory ("~/.Rscalade").}
}
\description{
Loads up the family of packages associated with the designated
ladders.
}
