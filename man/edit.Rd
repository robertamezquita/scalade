% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ladders.R
\name{construct}
\alias{construct}
\alias{edit}
\alias{burn}
\title{Edit existing ladder definitions via construction and destruction}
\usage{
construct(id.spec, repo, packages, .scalade = NULL)

burn(id.spec = NULL, overwrite = FALSE, .scalade = NULL)
}
\arguments{
\item{id.spec}{The ID to assign to the ladder.}

\item{repo}{The repo where the packages live (one of "CRAN", "Bioconductor", or "Remote" (Github))}

\item{packages}{The list of packages to associate with the new ladder.}

\item{.scalade}{The path to an existing or new .Rscalade file.}
}
\description{
Add, remove, and edit existing ladder definitions programmatically. \code{scalade::build()} will
take arguments to build a new ladder to append to an existing .Rscalade file (or create a new one
if it doesn't exist). \code{scalade::burn()} will destroy a ladder from an existing .Rscalade file.
}
