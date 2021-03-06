% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cumulative.T.Resampled.MACK.R
\name{Cumulative.T.Resampled.MACK}
\alias{Cumulative.T.Resampled.MACK}
\title{Cumulative.T.Resampled.MACK}
\usage{
Cumulative.T.Resampled.MACK(
  Cumulative.T,
  Dev.Fact.Boot,
  Scale.Param,
  Residuals.Boot,
  Method
)
}
\arguments{
\item{Cumulative.T}{Cumulative triangle.}

\item{Dev.Fact.Boot}{Predictive development factors resampled randomly.}

\item{Scale.Param}{Predictive scale parameter estimated by the MackNet model.}

\item{Residuals.Boot}{Residuals resampled in order to include the process variance to the lower triangle}

\item{Method}{If this variable is set to "Resampled", the process variance is fully based on the bootstrap method and, thus, the model is distribution-free. The option to rely on "Normal" and "Gamma" distribution to generate the process variance is included.}
}
\value{
Resampled triangle including the process variance.
}
\description{
It adds the process variance to the lower triangle.
}
