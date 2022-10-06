#' Functions to calculate ICAO standard atmospheric properties: temperature, 
#' density, and pressure.
#'
#' @param alt the altitude (above mean sea level). If excluded, sea level is assumed [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units.
#' @param ret_units If set to TRUE the value(s) returned are of class \code{units} with
#' units attached to the value. [Default is FALSE]
#'
#' @return the temperature of air for the standard atmosphere for the
#'   atmtemp function [\eqn{^{\circ}C}{C} or \eqn{^{\circ}F}{F}]
#'
#' @return the absolute pressure of air for the standard atmosphere for the
#'   atmpres function [\eqn{N m^{-2}}{N/m^2} or \eqn{lbf ft^{-2}}{lbf/ft^2}]
#'
#' @return the density of air for the standard atmosphere for the
#'   atmdens function [\eqn{{kg}\,{m^{-3}}}{kg/m^3} or \eqn{{slug}\,{ft^{-3}}}{slug/ft^3}]
#'
#' @author Ed Maurer
#'
#' @examples
#'
#' #Find standard atmospheric temperature at altitude 8000 m
#' atmtemp(alt = 8000, units = 'SI')
#'
#' #Find standard atmospheric pressure assuming default altitude of zero (sea-level)
#' atmpres(units = 'Eng')
#'
#' #Find standard atmospheric density at altitude 15000 ft 
#' atmdens(alt = 15000, units = 'Eng')
#'
#' @name atmosprops
NULL
# 

#function to perform calculations based on altitude
atmos <- function(alt = NULL) {
  if (min(alt) < 0 | max(alt) > 86000) {
    stop("\nAltitude outside range for valid values.\n")
  }
  #Approximation from Public Domain Aeronautical Software (PDAS)
  #adapted from https://www.pdas.com/programs/atmos.py
  #Compute temperature, density, and pressure in standard atmosphere.
  #Correct to 86 km.  Only approximate thereafter.
  #Input:
	#alt	geometric altitude
  #  Return: (sigma, delta, theta)
	#sigma	density/sea-level standard density
	#delta	pressure/sea-level standard pressure
	#theta	temperature/sea-level std. temperature

  REARTH <- 6369.0		# radius of the Earth (km)
  GMR <- 34.163195
  SLP <- 101.325      #sea-level pressure in kPa abs
  ATMDENS <- 1.225    #atmospheric density at sea level, kg/m^3

  #heights in km
  htab = c( 0.0,  11.0, 20.0, 32.0, 47.0,
           51.0, 71.0, 84.852 )
  #temps in Kelvin (C+273.15)
  ttab = c( 288.15, 216.65, 216.65, 228.65, 270.65,
           270.65, 214.65, 186.946 )
  #pressures relative to h=0
  ptab = c( 1.0, 2.2336110E-1, 5.4032950E-2, 8.5666784E-3, 1.0945601E-3,
           6.6063531E-4, 3.9046834E-5, 3.68501E-6 )
  #gradient of temperature for layer beginning at corresponding htab
  gtab = c( -6.5, 0.0, 1.0, 2.8, 0, -2.8, -2.0, 0.0 )
  
  #need to use alt in km here -- h is also in km
  h = (alt/1000)*REARTH/((alt/1000) + REARTH)	# geometric to geopotential altitude
  
  if (alt/1000 >= htab[length(htab)]) {
    i <- length(htab)
  } else if (alt/1000 <= htab[1]) {
    i <- 1
  } else {
    i <- max(which(htab < h))
  }
  tgrad = gtab[i]		# temp. gradient of local layer
  tbase = ttab[i]		# base  temp. of local layer
  deltah = h-htab[i]		# height above local base
  tlocal = tbase+tgrad*deltah	# local temperature
  theta = tlocal/ttab[1]	# temperature ratio
  
  if ( tgrad == 0.0 ){
    delta=ptab[i]*exp(-GMR*deltah/tbase)
  } else {
    delta=ptab[i]*(tbase/tlocal)^(GMR/tgrad)
  }
  sigma = delta/theta
  return ( atm_dens=sigma*ATMDENS, p_abs=delta*SLP, t_atm=theta+ttab[1]-273.15 )
}

#' @export
#' @rdname atmosprops
atmtemp <- function(alt = NULL, units = c("SI", "Eng"), ret_units = FALSE ) {
  # check to make sure that alt is given
  if( inherits(alt, "units") ) alt <- units::drop_units(alt)
  checks <- c(alt)
  units <- units
  if (length(checks) < 1) {
    if (units == "SI") {
      message("\nAltitude not given.\nAssuming alt = 0 m\n")
      alt = 0
    } else if (units == "Eng") {
      message("\nAltitude not given.\nAssuming alt = 0 ft\n")
      alt = 0
    } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
      stop("Incorrect unit system. Specify either SI or Eng.")
    }
  }
  if (units == "Eng") {
    # convert alt from ft to m if necessary
    alt = alt / 3.28084
  }
  #call atmos function for three parameters and to check input
  atmtmp() <- atmos(alt = alt)$t_atm
  if (units == "Eng") {
    # for Eng units, convert to F
    atmtmp <- ( atmtmp * 5/9) + 32
  }
  if( ret_units ) {
    if (units == "Eng") atmtmp <- units::set_units(atmtmp,"F")
    if (units == "SI") atmtmp <- units::set_units(atmtmp,"C")
  }
  return(atmtmp)
}
#' @export
#' @rdname atmosprops
atmpres <- function(alt = NULL, units = c("SI", "Eng"), ret_units = FALSE ) {
  #call atmos function for three parameters and to check input
  # check to make sure that alt is given
  if( inherits(alt, "units") ) alt <- units::drop_units(alt)
  checks <- c(alt)
  units <- units
  if (length(checks) < 1) {
    if (units == "SI") {
      message("\nAltitude not given.\nAssuming alt = 0 m\n")
      alt = 0
    } else if (units == "Eng") {
      message("\nAltitude not given.\nAssuming alt = 0 ft\n")
      alt = 0
    } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
      stop("Incorrect unit system. Specify either SI or Eng.")
    }
  }
  if (units == "Eng") {
    # convert alt from ft to m if necessary
    alt = alt / 3.28084
  }
  p_atm() <- atmos(alt = alt)$p_abs
  if (units == "Eng") {
    # for Eng units, convert kPa to psf
    p_atm <-  p_atm * 20.88543
  }
  if (units == "SI") {
    # for to Pa for consistency with other functions
    p_atm <-  p_atm * 1000.0
  }
  if( ret_units ) {
    if (units == "Eng") p_atm <- units::set_units(p_atm,"lbf/ft^2")
    if (units == "SI") p_atm <- units::set_units(p_atm,"Pa")
  }
  return(p_atm)
}

#' @export
#' @rdname atmosprops
atmdens <- function(alt = NULL, units = c("SI", "Eng"), ret_units = FALSE ) {
  #call atmos function for three parameters and to check input
  # check to make sure that alt is given
  if( inherits(alt, "units") ) alt <- units::drop_units(alt)
  checks <- c(alt)
  units <- units
  if (length(checks) < 1) {
    if (units == "SI") {
      message("\nAltitude not given.\nAssuming alt = 0 m\n")
      alt = 0
    } else if (units == "Eng") {
      message("\nAltitude not given.\nAssuming alt = 0 ft\n")
      alt = 0
    } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
      stop("Incorrect unit system. Specify either SI or Eng.")
    }
  }
  if (units == "Eng") {
    # convert alt from ft to m if necessary
    alt = alt / 3.28084
  }
  dens_atm() <- atmos(alt = alt)$atm_dens
  if (units == "Eng") {
    # for Eng units, convert kg/m^3 to slug/ft^3
    dens_atm <-  dens_atm * 0.062427960841 / 32.2
  }
  if( ret_units ) {
    if (units == "Eng") dens_atm <- units::set_units(dens_atm,"slug/ft^3")
    if (units == "SI") dens_atm <- units::set_units(dens_atm,"kg/m^3")
  }
  return(dens_atm)
}
