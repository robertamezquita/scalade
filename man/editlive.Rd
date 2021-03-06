% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ladders.R
\name{editlive}
\alias{editlive}
\alias{liveedit}
\title{Edit existing ladder definitions interactively}
\usage{
liveedit(.scalade = NULL)
}
\arguments{
\item{.scalade}{A .Rscalade ladder definition file to live edit.}

\item{repo}{}
}
\value{
None; called for the side effect of editing the ~/.Rscalade file.
}
\description{
Add, remove, and edit existing ladder definitions interactively. \code{scalade::liveedit()} will
pop open an interactive editor (as set by $EDITOR).
}
\details{
Repos
Note that for packages that are stored in remote (Github) repositories (\code{repo = "Remote"})
the package name must be in a format compatible with \code{remotes::install_remote},
e.g. \verb{<gh-username>/<repo>}.
}
