% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_code.R
\name{G_month}
\alias{G_month}
\title{Soil heat flux for monthly periods}
\usage{
G_month(Ti, Ti_m1, Ti_p1)
}
\arguments{
\item{Ti}{Mean temperature in current month.}

\item{Ti_m1}{Mean temperature in previous month.}

\item{Ti_p1}{Mean temperature in posterior month.}
}
\value{
Soil heat flux (MJ/m²/day).
}
\description{
Calculate the soil heat flux for monthly periods.
}
\examples{
G_month(26,25)
G_month(Ti_m1=25,Ti_p1=32)
}
