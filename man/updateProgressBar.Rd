% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/progressbar.r
\docType{methods}
\name{updateProgressBar}
\alias{updateProgressBar}
\alias{updateProgressBar,ProgressBar,numeric-method}
\title{Updates `ProgressBar` with `value`}
\usage{
updateProgressBar(x, value, label)

\S4method{updateProgressBar}{ProgressBar,numeric}(x, value, label = "")
}
\arguments{
\item{x}{`ProgressBar` instance}

\item{value}{current state of the `ProgressBar` to be updated}

\item{label}{optional label to be printed with the `ProgressBar`, defaults
to empty string ("")}
}
\description{
`value` has to `min<= value <= max` with `min` and `max` values
of the slots
}
\details{
`ProgressBar` tries to evaluate an ETA and prints it.
}
