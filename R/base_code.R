## Functions to calculate PenmanMonteith FAO 56

#Function Patmz to calculate local atmospheric pressure based on local elevation (m)
#Function input
#z: Local elevation (m)
#################
#Function output
#Patm: Local atmospheric pressure kPa
################
#' Atmospheric Pressure
#' @description
#' Estimate atmospheric pressure based on the elevation z of a location.
#'
#' @param z Elevation of the location
#'
#' @return The estimated atmospheric pressure in kPa on the location at elevation z.
#'
#' @examples
#' Patmz(100)
#'
#' @export
Patmz<-function(z){
  Patm<-101.3*((293-0.0065*z)/293)^5.26
  return(Patm)
}
################

#Function psychrometric to calculate psychrometric coefficient
#Function input
#Patm: Local atmospheric pressure kPa
#################
#Function output
#gamma:psychrometric coefficient
################
#' Psychrometric Coefficient
#' @description
#' Returns the psychrometric coefficient based on the atmospheric pressure in a location.
#'
#' @param Patm Atmospheric pressure in kPa.
#'
#' @return Psychrometric coefficient at the location.
#'
#' @examples
#' psychrometric(100)
#'
#' @export
psychrometric<-function(Patm){
  gamma<-0.665e-3*Patm
  return(gamma)
}
################

#Function to calculate saturation vapor pressure based on temperature
#Function input
#Temperature: Temperature in celsius
#################
#Function output
#esT: vapor saturation pressure(kPa)
################
#' Vapor Saturation Pressure
#' @description
#' Returns the vapor saturation pressure in kPa for a given temperature in Celsius.
#'
#' @param Temperature Temperature in Celsius.
#'
#' @return vapor saturation pressure in kPa.
#'
#' @examples
#' esT(20)
#'
#' @export
esT<-function(Temperature){
  esT<-0.6108*exp((17.27*Temperature)/(Temperature+237.3))
  return(esT)
}


#Function to calculate mean saturation vapor pressure
#Function input
#temperatures: vector with temperatures or a single temperature
#################
#Function output
#es: Mean vapor saturation pressure(kPa)
################

#' Mean Vapor Saturation Pressure
#' @description
#' Returns the mean vapor saturation pressure in kPa across a set of temperatures in Celsius.
#'
#' @note
#' The FAO56 recommendation is to use the mean vapor saturation pressure of the values estimated at the maximum and minimum temperature in a day instead of measuring only at mean temperature. To satisfy both uses (with mean daily temperature and with maximum and minimum temperatures), which are common, and cases that evapotranspiration is estimated for longer periods, the following function was created.
#'
#' @param temperatures A single temperature or a vector of temperatures in Celsius.
#'
#' @return Mean vapor saturation pressure in kPa across the given temperatures.
#'
#' @examples
#' es(c(20,25))
#'
#' @export
es<-function(temperatures){
  mean(esT(temperatures))
}
################
#Notes:
#For evapotranspiration estimates for longer periods (week, month, year, decade)
#it must be used the average of mean temperature, or the average of maximum and
#minimum daily temperatures.
#This function is used as standard as there is a possibility to calculate the following
#evapotranspiration using maximum and minimum temperatures or the average daily temperature.
#To calculate with mean temperature a single value must be included.
#To calculate with maximum and minimim temperatures both should be included in a vector.
###############

#Function slope to calculate slope of the vapor saturation pressure curve
#Function input
#Temperature: Average daily temperature in celsius
#################
#Function output
#delta: slope of the vapor saturation pressure curve (kPa/C)
################

#' Vapor Saturation Curve Slope
#' @description
#' Returns the slope of the vapor saturation curve at a given temperature in Celsius.
#'
#' @param Temperature Temperature in Celsius.
#'
#' @return Slope of vapor saturation pressure at a given temperature.
#'
#' @examples
#' slope(20)
#'
#' @export
slope<-function(Temperature){
  delta<-4098*((esT(Temperature))/((Temperature+237.3)^2))
  return(delta)
}
################

#Functions to calculate actual vapor pressure

#Case 1: from dewpoint temperature

#' Actual Vapor Pressure (dew point)
#' @description
#' Returns the actual vapor pressure in kPa considering the dew point.
#'
#' @param Tdew Dew point temperature in Celsius.
#'
#' @return Actual vapor pressure in kPa.
#'
#' @examples
#' ea_dewpoint(20)
#'
#' @export
ea_dewpoint<-function(Tdew){
  ea<-es(Tdew)
  return(ea)
}

#Case 2: from psychrometric data

#' Psychrometer Psychrometric Constant
#' @description
#' Set the psychrometric constant of the psychrometer based upon its ventilation method (ventilated, natural, non-ventilated) or according a value of the instrument constant set by the user and the local atmospheric pressure.
#'
#' @param a_psy Must be one of the following charaters: "ventilated", indicating that the psychrometer is ventilated (Asmann type), with an air movement of some 5 m/s (constant value 0.000662); "natural" for natural ventilated psychrometers (about 1 m/s, constant value 0.000800); "non-ventilated" for non-ventilated psychrometers installed indoors (constant of 0.001200); or a numeric indicating the constant of the psychrometer of the weather station.
#'
#' @param Patm Atmospheric pressure in kPa.
#'
#' @return Psychrometric constant of the psychrometer at a given atmospheric pressure.
#'
#' @examples
#' set_gamma_psy(Patm=100)
#' set_gamma_psy("ventilated",100)
#' set_gamma_psy("natural",100)
#' set_gamma_psy("non-ventilated",100)
#' set_gamma_psy(0.000700,100)
#' @export
set_gamma_psy<-function(a_psy="ventilated",Patm){
  if(a_psy=="ventilated"){
    gamma_psy=0.000662*Patm
  }else if(a_psy=="natural"){
    gamma_psy=0.000800*Patm
  }else if(a_psy=="non-ventilated"){
    gamma_psy=0.001200*Patm
  }else if(is.numeric(a_psy)){
    gamma_psy=a_psy*Patm
  }else{
    gamma_psy<-"Set a predifined type of psychromoter on a_psy: ventilated (also know as Asmann type); natural; or non-ventilated. Alternatively set the corresponding constant related to psychrometric equipment ventilation."
  }
  return(gamma_psy)
}

#' Actual Vapor Pressure (psychrometric)
#' @description
#' Estimate the actual vapor pressure using psychrometric data.
#'
#' @param Twet Wet bulb temperature.
#' @param Tdry Dry bulb temperature.
#' @param Patm Atmospheric pressure.
#' @param gamma_psy Psychrometric constant of the psychrometer at a given atmospheric pressure.
#'
#' @return Actual vapor pressure in kPa.
#'
#' @examples
#' ea_psychrometric(25,27,gamma_psy=set_gamma_psy("ventilated",100))
#' ea_psychrometric(25,27,gamma_psy=set_gamma_psy("natural",100))
#' ea_psychrometric(25,27,gamma_psy=set_gamma_psy("non-ventilated",100))
#' ea_psychrometric(25,27,gamma_psy=0.12)
#'
#' @export
ea_psychrometric<-function(Twet,Tdry,Patm,gamma_psy){
  ea<-es(Twet)-gamma_psy*(Tdry-Twet)
  return(ea)
}

#Case 3: from relative humidity

#' Actual Vapor Pressure (relative humidity)
#' @description
#' Estimate the actual vapor pressure using relative humidity.
#'
#' @param Tmax Maximum temperature in Celcius.
#' @param Tmin Minimum temperature in Celcius.
#' @param RHmax Maximum relative humidity.
#' @param RHmin Minimim relative humidity.
#'
#' @return Actual vapor pressure in kPa.
#'
#' @examples
#' ea_RH(25,32,100,80)
#'
#' @export
ea_RH<-function(Tmax,Tmin,RHmax,RHmin){
  ea<-(es(Tmin)*RHmax/100+es(Tmax)*RHmin/100)/2
  return(ea)
}

#' Actual Vapor Pressure (maximum relative humidity)
#' @description
#' Estimate the actual vapor pressure with only maximum relative humidity.
#'
#' @param Tmin Minimum temperature in Celcius.
#' @param RHmax Maximum relative humidity.
#'
#' @return Actual vapor pressure in kPa.
#'
#' @examples
#' ea_RHmax(25,100)
#'
#' @export
ea_RHmax<-function(Tmin,RHmax){
  ea<-es(Tmin)*RHmax/100
  return(ea)
}

#' Actual Vapor Pressure (mean relative humidity)
#' @description
#' Estimate the actual vapor pressure with only mean relative humidity.
#'
#' @note
#' The FAO56 recommendation is to use the mean vapor saturation pressure of the values estimated at the maximum and minimum temperature in a day instead of measuring only at mean temperature. As the following actual vapor pressure estimation requires the estimation of vapor saturation pressure, to satisfy both uses (with mean daily temperature and with maximum and minimum temperatures), which are common, and cases that evapotranspiration is estimated for longer periods, this function was defined on such way.
#'
#' @param Temperatures Single temperature or a vector of temperature in Celcius.
#' @param RHmean Mean relative humidity.
#'
#' @return Actual vapor pressure in kPa.
#'
#' @examples
#' ea_RHmean(c(25,32),90)
#'
#' @export
ea_RHmean<-function(Temperatures,RHmean){
  ea<-es(Temperatures)*RHmean/100
  return(ea)
}

#Extraterresterial radiation

#' Day of Year
#' @description
#' By specifying a date with day, month and year it returns the number of the day in that respective year.
#'
#' @param day Day of the respective date.
#' @param month Numeric value from 1 to 12 representing the month of the respective date.
#' @param year Year of the respective date.
#'
#' @return The number of the day in the given year.
#'
#' @examples
#' Day_of_Year(7,9,1822)
#'
#' @export
Day_of_Year<-function(day,month,year){
  Day_of_Year<-floor(275*month/9-30+day)-2
  if(month<3){
    Day_of_Year=Day_of_Year+2
  }
  if(year%%4==0&month>2){
    Day_of_Year=Day_of_Year+1
  }
  return(Day_of_Year)
}

#' Inverse Relative Distance from Earth to Sun
#' @description
#' By specifying the date with day, month and year or the day of the year it returns the inverse relative distance from earth to sun in the respective day of the year.
#'
#' @param day Day of the respective date.
#' @param month Numeric value from 1 to 12 representing the month of the respective date.
#' @param year Year of the respective date.
#' @param day_of_year The number of the day in a given year.
#'
#' @return The inverse relative distance from earth to sun in a given day of the year.
#'
#' @examples
#' inverse_distance_earth_sun(7,9,1822)
#' inverse_distance_earth_sun(day_of_year=250)
#' @export
inverse_distance_earth_sun<-function(day,month,year,day_of_year){
  if(missing(day_of_year)){
    inverse_distance_earth_sun<-1+0.033*cos((2*pi/365)*Day_of_Year(day,month,year))
  }else{
    inverse_distance_earth_sun<-1+0.033*cos((2*pi/365)*day_of_year)
  }
  return(inverse_distance_earth_sun)
}

#' Solar Decimation
#' @description
#' By specifying the date with day, month and year or the day of the year it returns the solar decimation in the given day of the year.
#'
#' @param day Day of the respective date.
#' @param month Numeric value from 1 to 12 representing the month of the respective date.
#' @param year Year of the respective date.
#' @param day_of_year The number of the day in a given year.
#'
#' @return The solar decimation in radians in a given day of the year.
#'
#' @examples
#' solar_decimation(7,9,1822)
#' solar_decimation(day_of_year=250)
#' @export
solar_decimation<-function(day,month,year,day_of_year){
  if(missing(day_of_year)){
    solar_decimation<-0.409*sin((2*pi/365)*Day_of_Year(day,month,year)-1.39)
  }else{
    solar_decimation<-0.409*sin((2*pi/365)*day_of_year-1.39)
  }
  return(solar_decimation)
}

#' Latitude in Radians
#' @description
#' By specifying the degrees and minutes of a latitude, and specifying if the refeered location is in or not in the north hemisphere it converts the latitude from degrees and minutes to radians.
#'
#' @param degrees Degrees of the latitude.
#' @param minutes Minutes of the latitude.
#' @param north Boolean indicating if the refeered location is in the north hemisphere. Default is north=TRUE.
#'
#' @return Latitude in radians.
#'
#' @examples
#' latitude_radians(22,45)
#' latitude_radians(22,45,FALSE)
#' latitude_radians(22,45,TRUE)
#' @export
latitude_radians<-function(degrees,minutes,north=TRUE){
  latitude_radians<-(pi/180)*(degrees+minutes/60)
  if(north==FALSE){
    latitude_radians<--1*latitude_radians
  }
  return(latitude_radians)
}


#' Sunset Hour Angle
#' @description
#' Returns sunset hour angle by solar decimation and latitude in radians.
#'
#' @param lat_radians Latitude in radians.
#' @param solar_decimation Solar decimation in radians.
#' @return Hour angle at sunset.
#'
#' @examples
#' sunset_hour_angle(0.3970624,0.09246253)
#' sunset_hour_angle(latitude_radians(22,45,TRUE),solar_decimation(7,9,1822))
#' @export
sunset_hour_angle<-function(lat_radians,solar_decimation){
  sunset_hour_angle<-acos(-tan(lat_radians)*tan(solar_decimation))
  return(sunset_hour_angle)
}

#' Extraterresterial Radiation
#' @description
#' Calculate extraterresterial radiation in a specific day of year and latitude.
#'
#' @note It is only necessary to specify the day, month and year of the day or the respective day_of_year, if both are specified day_of_year will be used. Also, it is only necessary to specify lat_degrees and lat_minutes or lat_radians, if both are specified lat_radians will be used.
#'
#' @param day Day of the respective date.
#' @param month Numeric value from 1 to 12 representing the month of the respective date.
#' @param year Year of the respective date.
#' @param day_of_year The number of the day in a given year.
#' @param lat_degrees Degrees of the latitude.
#' @param lat_minutes Minutes of the latitude.
#' @param north Boolean indicating if the refeered location is in north hemisphere. Default is north=TRUE.
#' @param lat_radians Latitude in radians.
#'
#' @return Extraterresterial radiation.
#'
#' @examples
#' Ra(7,9,1822,22,45,TRUE)
#' Ra(day_of_year=250,lat_radians=0.3970624)
#' @export
Ra<-function(day,month,year,lat_degrees,lat_minutes,north=TRUE,day_of_year,lat_radians){
  if(missing(day_of_year)){
    inverse_distance_earth_sun<-inverse_distance_earth_sun(day,month,year)
    solar_decimation<-solar_decimation(day,month,year)
  }else{
    inverse_distance_earth_sun<-inverse_distance_earth_sun(day_of_year=day_of_year)
    solar_decimation<-solar_decimation(day_of_year=day_of_year)
  }
  if(missing(lat_radians)){
    lat_radians<-latitude_radians(lat_degrees,lat_minutes,north)
  }
  sunset_hour_angle<-sunset_hour_angle(lat_radians,solar_decimation)
  Gsc<-0.0820
  Ra<-(24*60/pi)*Gsc*inverse_distance_earth_sun*(sunset_hour_angle*sin(lat_radians)*sin(solar_decimation)+cos(lat_radians)*cos(solar_decimation)*sin(sunset_hour_angle))
  return(Ra)
}

#Solar radiation

#Function to estimate solar radiation with Angstrom formula
#as: fraction of extraterrestrial radiation reaching the earth on overcast days
#as + bs: fraction of extraterrestrial radiation reaching earth on clear days
#Sunshine_Duration: actual duration of sunshine (hours)
#Daylight_Hours: maximum possible duration of sunshine (hours)

#####################################################
#Note: without calibration, use as=0.25 and bs=0.50.#
#####################################################

#' Daylight Hours
#' @description
#' Calculate maximum sunshine duration from sunset hour angle.
#'
#' @param sunset_hour_angle Hour angle at sunset in radians.
#'
#' @return Maximum sunshine duration.
#'
#' @examples
#' Daylight_Hours(sunset_hour_angle(latitude_radians(22,45,north=TRUE),solar_decimation(07,09,1882)))
#' Daylight_Hours(1.60969)
#' @export
Daylight_Hours<-function(sunset_hour_angle){
  Daylight_Hours<-(24/pi)*sunset_hour_angle
  return(Daylight_Hours)
}

#' Angstrom Formula
#' @description
#' Calculate solar radiation using Angstrom formula using the fraction of radiation reaching the earth in overcast and clear days, and the actual and potential sunshine hours.
#'
#' @param Sunshine_Duration Actual sunshine duration in hours.
#' @param Daylight_Hours Maximum sunshine duration in hours.
#' @param Ra Extraterresterial radiation.
#' @param as Fraction of extraterresterial radiation reaching the earth on overcast days.
#' @param bs as + bs is the fraction of extraterresterial radiation reaching earth on clear days.
#'
#' @return Solar radiation calculated from Angstrom formula.
#'
#' @examples
#' Rs_Angstrom(12,12.29713,36.16717)
#' @export
Rs_Angstrom<-function(Sunshine_Duration,Daylight_Hours,Ra,as=0.25,bs=0.50){
  Rs_Angstrom<-(as+bs*(Sunshine_Duration/Daylight_Hours))*Ra
  return(Rs_Angstrom)
}

#' Solar Radiation on Clean Sky
#' @description
#' Calculate solar radiation on clean sky using extraterresterial radiation, elevation above sea level when as and bs are not provided or with as and bs values.
#'
#' @param Ra Extraterresterial radiation.
#' @param z Elevation above sea level of weather station.
#' @param as Fraction of extraterrestrial radiation reaching the earth on overcast days.
#' @param bs as + bs is the fraction of extraterrestrial radiation reaching earth on clear days.
#'
#' @return Solar radiation calculated from Angstrom formula.
#'
#' @examples
#' Rso(Ra(7,9,1822,22,45,TRUE),0)
#' Rso(Ra(7,9,1822,22,45,TRUE),100)
#' Rso(36.16717,as=0.3,bs=0.55)
#' @export
Rso<-function(Ra,z,as,bs){
  if(missing(as)|missing(bs)){
    Rso<-(0.75+2e-5*z)*Ra
  }else{
    Rso<-(as+bs)*Ra
  }
  return(Rso)
}

#' Net Solar or Net Shortwave Radiation
#' @description
#' Calculate net solar (shortwave) radiation resulting from the balance among incoming and reflected radiation using albedo and the estimated incoming solar radiation.
#'
#' @param Rs Incoming solar radiation.
#' @param alpha Albedo. Default value is 0.23, representing the reference grass crop.
#'
#' @return Net solar (shortwave) radiation resulting from the balance among incoming and reflected solar radiation.
#'
#' @examples
#' Rns(Rs_Angstrom(12,12.79,Ra(7,9,1882,22,45,TRUE)))
#' Rns(32)
#' Rns(Rs_Angstrom(12,12.79,Ra(7,9,1882,22,45,TRUE)),alpha=0.17)
#' Rns(32,alpha=0.17)
#' @export
Rns<-function(Rs,alpha=0.23){
  Rns<-(1-alpha)*Rs
  return(Rns)
}

#' Net Longwave Radiation (Rnl)
#' @description
#' Calculate net outcoming longwave radiation from maximum and minimum temperatures, actual vapor pressure, incoming solar radiation and incoming solar radiation at clean sky.
#'
#' @param Tmax Maximum temperature in Celsius.
#' @param Tmin Minimum temperature in Celsius.
#' @param ea Actual vapor pressure.
#' @param Rs Incoming solar radiation.
#' @param Rso Incoming solar radiation at clean sky.
#'
#' @return Net solar longwave radiation.
#'
#' @examples
#' Rnl(30,22,2.410449,27,30)
#' @export
Rnl<-function(Tmax,Tmin,ea,Rs,Rso){
  sigma<-4.903e-9
  Tmax_k<-Tmax+273.16
  Tmin_k<-Tmin+273.16
  Rnl<-sigma*((Tmax_k^4+Tmin_k^4)/2)*(0.34-0.14*sqrt(ea))*(1.35*(Rs/Rso)-0.35)
  return(Rnl)
}

#' Net Radiation
#' @description
#'Calculate the difference among incoming shortwave radiation and outgoing longwave radiation.
#' @param Rns Net shortwave radiation.
#' @param Rnl Net longwave radiation.
#'
#' @return Net radiation.
#'
#' @examples
#' Rn(26,9e-7)
#' @export
Rn<-function(Rns,Rnl){
  Rn<-Rns-Rnl
  return(Rn)
}

#Correct wind velocity if it is measured in a different height than 2 m
#z: is the height where the wind velocity is measured
#uz: wind velocity at height z
#' Estimated Wind Velocity at 2 m Height
#' @description
#' Estimate the wind velocity at 2 m when it is measured at a different height from soil.
#' @param z Height in meters where wind velocity is measured.
#' @param uz Wind velocity at height z.
#'
#' @return Estimated wind velocity at the height of 2 m
#'
#' @examples
#' u2(10,5)
#' @export
u2<-function(z,uz){
  u2<-uz*(4.87/log(67.8*z-5.42))
  return(u2)
}

#' Soil heat flux for monthly periods
#' @description
#' Calculate the soil heat flux for monthly periods.
#' @param Ti Mean temperature in current month.
#' @param Ti_m1 Mean temperature in previous month.
#' @param Ti_p1 Mean temperature in posterior month.
#'
#' @return Soil heat flux (MJ/m²/day).
#'
#' @examples
#' G_month(26,25)
#' G_month(Ti_m1=25,Ti_p1=32)
#' @export
G_month<-function(Ti,Ti_m1,Ti_p1){
  if(missing(Ti_p1)){
    G<-0.14*(Ti-Ti_m1)
  }else{
    G<-0.07*(Ti_p1-Ti_m1)
  }
  return(G)
}

#Function Eto to calculate evapotranspiration by Penman-Monteith FAO 56 method
##########################
#Function inputs#
#Rn: Net radiation
#gamma: psychrometric coefficient
#Temp: Average daily temperature in celsius degrees (taken at 2 m height)
#u2: Wind velocity (taken at 2 m height)
#es: saturation vapor pressure (kPa)
#ea: actual vapor pressure (kPa)
#delta: slope of the vapor saturation pressure curve (kPa/C)
#G: Soil heat flux (for daily periods it approaches 0)
##########################
#Function output#
#Eto: reference evapotranspiration (mm/day)
##########################
#' Penman-Monteith FAO56
#' @description
#' Calculate the reference evapotranspiration using Penman-Monteith FAO56 method.
#' @param Rn Net radiation.
#' @param Patm Atmospheric pressure.
#' @param Temp Average daily temperature in celsius degrees (taken at 2 m height).
#' @param Tmax Maximum temperature.
#' @param Tmin Minimum temperature.
#' @param Wind_Velocity_2m Wind velocity (taken at 2 m height).
#' @param Saturation_Vapor_Pressure Saturation vapor pressure (kPa).
#' @param gamma Psychrometric coefficient.
#' @param Actual_Vapor_Pressure Actual vapor pressure (kPa).
#' @param delta Slope of the vapor saturation pressure curve (kPa/C).
#' @param G Soil heat flux (for daily periods it approaches 0).
#'
#' @return Reference evapotranspiration (mm/day).
#'
#' @examples
#' PenmanMonteith_FAO56(26,98,26,30,22,4,3.2,0.2,0.07,3.4)
#' @export
PenmanMonteith_FAO56<-function(Rn,Patm,
              Temp,Tmax,Tmin,Wind_Velocity_2m,
              Actual_Vapor_Pressure,
              delta=slope(Temp),gamma=psychrometric(Patm),Saturation_Vapor_Pressure=es(c(Tmax,Tmin)),G=0){
  PenmanMonteith_FAO56<-(0.408*delta*(Rn-G)+gamma*(900/(Temp+273))*Wind_Velocity_2m*(Saturation_Vapor_Pressure-Actual_Vapor_Pressure ))/(delta+gamma*(1+0.34*Wind_Velocity_2m))
  return(PenmanMonteith_FAO56)
}
##########################

#Rs_Angstrom(12,Daylight_Hours(sunset_hour_angle(latitude_radians(22,45,north=TRUE),solar_decimation(07,09,1882))),Ra(7,9,1822,22,45,TRUE))
#Rnl(30,22,ea_dewpoint(26),Rs_Angstrom(12,12.79,Ra(7,9,1882,22,45,TRUE)),Rso(Ra(7,9,1882,22,45,TRUE),300,0.3,0.6))
#Rn(Rns(Rs_Angstrom(12,12.79,Ra(7,9,1882,22,45,TRUE))),Rnl(30,22,ea_dewpoint(26),Rs_Angstrom(12,12.79,Ra(7,9,1882,22,45,TRUE)),Rso(Ra(7,9,1882,22,45,TRUE),300,0.3,0.6)))
#PenmanMonteith_FAO56(Rn(Rns(Rs_Angstrom(12,12.79,Ra(7,9,1882,22,45,TRUE))),Rnl(30,22,ea_dewpoint(26),Rs_Angstrom(12,12.79,Ra(7,9,1882,22,45,TRUE)),Rso(Ra(7,9,1882,22,45,TRUE),300,0.3,0.6))),Patmz(300),26,30,22,u2(10,5),ea_dewpoint(26))
