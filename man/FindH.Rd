% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FindH.R
\name{FindH}
\alias{FindH}
\title{Find bandwidth h.}
\usage{
FindH(p, n, htype)
}
\arguments{
\item{p}{dimension of data. The number of variables.}

\item{n}{the number of observations.}

\item{htype}{methods to calculate h. The valid options are (case insensitive) "amise" or "rot".}
}
\value{
bandwidth h.
}
\description{
Find bandwidth h from the number of observations n and the dimension p.
}
