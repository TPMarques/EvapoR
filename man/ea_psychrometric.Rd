% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_code.R
\name{ea_psychrometric}
\alias{ea_psychrometric}
\title{Actual Vapor Pressure (psychrometric)}
\usage{
ea_psychrometric(Twet, Tdry, Patm, gamma_psy)
}
\arguments{
\item{Twet}{Wet bulb temperature.}

\item{Tdry}{Dry bulb temperature.}

\item{Patm}{Atmospheric pressure.}

\item{gamma_psy}{Psychrometric constant of the psychrometer at a given atmospheric pressure.}
}
\value{
Actual vapor pressure in kPa.
}
\description{
Estimate the actual vapor pressure using psychrometric data.
}
\examples{
ea_psychrometric(25,27,gamma_psy=set_gamma_psy("ventilated",100))
ea_psychrometric(25,27,gamma_psy=set_gamma_psy("natural",100))
ea_psychrometric(25,27,gamma_psy=set_gamma_psy("non-ventilated",100))
ea_psychrometric(25,27,gamma_psy=0.12)

}
