% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pearson.Bias.Adj.MACK.Full.R
\name{Pearson.Bias.Adj.MACK.Full}
\alias{Pearson.Bias.Adj.MACK.Full}
\title{Pearson.Bias.Adj.MACK.Full}
\usage{
Pearson.Bias.Adj.MACK.Full(Pearson.Resid, Constant.Param)
}
\arguments{
\item{Pearson.Resid}{Non-scaled residuals.}

\item{Constant.Param}{If this variable is set to 0, the bias adjustment suggested by England y Verrall (2006) in "Predictive Distributions of Outstanding Liabilities in General Insurance" is applied, this means that residuals are multiplied by N/(N-p). In case this variable is set to 1, the adjustment suggested by Mack (1993) in "Distribution-free calculation of the standard error of chain ladder reserve estimates" is applied, this means that residuals are multiplied by n(i)/(n(i)-1). Finally, if this variable is set to 2, no adjustment is applied.}
}
\value{
Bias adjusted residuals of the MackNet model. These residuals are used within the boostrap procedure.
}
\description{
It generates the bias adjusted residuals needed for the boostrap implementation of MackNet
}
