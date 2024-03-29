% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_code.R
\name{PenmanMonteith_FAO56}
\alias{PenmanMonteith_FAO56}
\title{Penman-Monteith FAO56}
\usage{
PenmanMonteith_FAO56(
  Rn,
  Patm,
  Temp,
  Tmax,
  Tmin,
  Wind_Velocity_2m,
  Actual_Vapor_Pressure,
  delta = slope(Temp),
  gamma = psychrometric(Patm),
  Saturation_Vapor_Pressure = es(c(Tmax, Tmin)),
  G = 0
)
}
\arguments{
\item{Rn}{Net radiation.}

\item{Patm}{Atmospheric pressure.}

\item{Temp}{Average daily temperature in celsius degrees (taken at 2 m height).}

\item{Tmax}{Maximum temperature.}

\item{Tmin}{Minimum temperature.}

\item{Wind_Velocity_2m}{Wind velocity (taken at 2 m height).}

\item{Actual_Vapor_Pressure}{Actual vapor pressure (kPa).}

\item{delta}{Slope of the vapor saturation pressure curve (kPa/C).}

\item{gamma}{Psychrometric coefficient.}

\item{Saturation_Vapor_Pressure}{Saturation vapor pressure (kPa).}

\item{G}{Soil heat flux (for daily periods it approaches 0).}
}
\value{
Reference evapotranspiration (mm/day).
}
\description{
Calculate the reference evapotranspiration using Penman-Monteith FAO56 method.
}
\examples{
PenmanMonteith_FAO56(26,98,26,30,22,4,3.2,0.2,0.07,3.4)
}
