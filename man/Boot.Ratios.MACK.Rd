% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Boot.Ratios.MACK.R
\name{Boot.Ratios.MACK}
\alias{Boot.Ratios.MACK}
\title{Boot.Ratios.MACK}
\usage{
Boot.Ratios.MACK(Cumulative.T, Resid.Resampled, Scale.Param, Dev.Factors.AI)
}
\arguments{
\item{Cumulative.T}{Cumulative triangle.}

\item{Resid.Resampled}{MackNet residuals resampled.}

\item{Scale.Param}{MackNet scale parameter.}

\item{Dev.Factors.AI}{Predictive development factors obtained from the ensemble of RNNs.}
}
\value{
Link ratios resampled randomly.
}
\description{
It generates the set of link ratios resampled by using the resampled set of MackNet residuals.
}
