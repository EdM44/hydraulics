#' Functions to calculate water properties: density, dynamic and kinematic 
#' viscosity, saturation vapor pressure, surface tension, and bulk modulus.
#'
#' This function calculates water properties that are used in other functions.
#'
#' @param T the water temperature [\eqn{^{\circ}C}{C} or \eqn{^{\circ}F}{F}]
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units. This is used for compatibility with iemisc package
#' @param ret_units If set to TRUE the value(s) returned are of class \code{units} with
#' units attached to the value. [Default is FALSE]
#'
#' @return rho, the density of water for the
#'   dens function [\eqn{{kg}\,{m^{-3}}}{kg/m^3} or \eqn{{slug}\,{ft^{-3}}}{slug/ft^3}]
#'
#' @return mu, the dynamic viscosity of water for the
#'   dvisc function [\eqn{{N}\,{s}\,{m^{-2}}}{N s/m^2} or \eqn{{lbf}\,{s}\,{ft^{-2}}}{lbf s/ft^2}]
#'
#' @return nu, the kinematic viscosity of water for the
#'   kvisc function [\eqn{m^2 s^{-1}}{m^2/s} or \eqn{ft^2 s^{-1}}{ft^2/s}].
#'
#' @return svp, the saturation vapor pressure of water for the
#'   svp function [\eqn{N m^{-2}}{N/m^2} or \eqn{lbf ft^{-2}}{lbf/ft^2}].
#'
#' @return surf_tension, the surface tension of water for the
#'   surf_tension function [\eqn{N m^{-1}}{N/m} or \eqn{lbf ft^{-1}}{lbf/ft}].
#'
#' @return Ev, the bulk modulus of elasticity of water for the
#'   Ev function [\eqn{N m^{-2}}{N/m^2} or \eqn{lbf ft^{-2}}{lbf/ft^2}].
#'
#' @author Ed Maurer
#'
#' @examples
#'
#' #Find kinematic viscocity for water temperature of 55 F
#' nu = kvisc(T = 55, units = 'Eng')
#'
#' #Find kinematic viscocity assuming default water temperature of 68 F
#' nu = kvisc(units = 'Eng')
#'
#' #Find water density for water temperature of 25 C
#' rho = dens(T = 25, units = 'SI')
#' 
#' #Find saturation vapor pressure for water temperature of 10 C
#' vps = svp(T = 10, units = 'SI')
#'
#' #Find surface tension for water temperature of 10 C
#' s_tens = surf_tension(T = 10, units = 'SI')
#'
#' @name waterprops
NULL
# dynamic viscocity
#' @export
#' @rdname waterprops
dvisc <- function(T = NULL, units = c("SI", "Eng"), ret_units = FALSE ) {
  # check to make sure that T is given
  if( class(T) == "units" ) T <- units::drop_units(T)
  checks <- c(T)
  units <- units
  if (length(checks) < 1) {
    if (units == "SI") {
      message("\nTemperature not given.\nAssuming T = 20 C\n")
      T = 20
    } else if (units == "Eng") {
      message("\nTemperature not given.\nAssuming T = 68 F\n")
      T = 68
    } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
      stop("Incorrect unit system. Specify either SI or Eng.")
    }
  }
  if (units == "Eng") {
    # convert F to C if necessary
    T = (T - 32) * 5/9
  }
  if (min(T) < 0 | max(T) > 100) {
    stop("\nTemperature outside range for liquid water.\n")
  }
  # Approximation from Seeton, Christopher J. (2006),
  # 'Viscosity-temperature correlation for liquids', Tribology Letters,
  # 22: 67–78
  A <- 1.856e-11
  B <- 4209
  C <- 0.04527
  D <- -3.376e-05
  T <- T + 273  #T must be in K for approximation
  visc <- A * exp(B/T + C * T + D * T^2)/1000  #1000 converts from  mPa·s to Pa-s (N s m-2)
  
  if (units == "Eng") {
    # for Eng units, convert to lbf s ft-2
    visc <- visc * 0.02088543
  }
  if( ret_units ) {
    if (units == "Eng") visc <- units::set_units(visc,"lbf*s/ft^2")
    if (units == "SI") visc <- units::set_units(visc,"N*s/m^2")
  }
  return(visc)
}
#' @export
#' @rdname waterprops
dens <- function(T = NULL, units = c("SI", "Eng"), ret_units = FALSE) {
  # check to make sure that T is given
  if( class(T) == "units" ) T <- units::drop_units(T)
  checks <- c(T)
  units <- units
  if (length(checks) < 1) {
    if (units == "SI") {
      message("\nTemperature not given.\nAssuming T = 20 C\n")
      T = 20
    } else if (units == "Eng") {
      message("\nTemperature not given.\nAssuming T = 68 F\n")
      T = 68
    } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
      stop("Incorrect unit system. Specify either SI or Eng.")
    }
  }
  if (units == "Eng") {
    # convert F to C if necessary
    T = (T - 32) * 5/9
  }
  if (min(T) < 0 | max(T) > 100) {
    stop("\nTemperature outside range for liquid water.\n")
  }
  # fresh water density - taken from water.density.R in rLakeAnalyzer
  # from Martin, J.L., McCutcheon, S.C., 1999. Hydrodynamics and
  # Transport for Water Quality Modeling.
  rho <- (1000 * (1 - (T + 288.9414) * (T - 3.9863)^2/(508929.2 * (T +
                                                                     68.12963))))
  if (units == "Eng") {
    # for Eng units, convert from kg m-3 to lbf ft-3
    rho <- rho * 0.062427960841
  }
  if( ret_units ) {
    if (units == "Eng") rho <- units::set_units(rho,"slug/ft^3")
    if (units == "SI") rho <- units::set_units(rho,"kg/m^3")
  }
  return(rho)
}
#' @export
#' @rdname waterprops
kvisc <- function(T = NULL, units = c("SI", "Eng"), ret_units = FALSE) {
  # check to make sure that T is given
  if( class(T) == "units" ) T <- units::drop_units(T)
  checks <- c(T)
  units <- units
  if (length(checks) < 1) {
    if (units == "SI") {
      message("\nTemperature not given.\nAssuming T = 20 C\n")
      T = 20
    } else if (units == "Eng") {
      message("\nTemperature not given.\nAssuming T = 68 F\n")
      T = 68
    } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
      stop("Incorrect unit system. Specify either SI or Eng.")
    }
  }
  if (units == "Eng") {
    # convert units if necessary
    T = (T - 32) * 5/9
  }
  if (min(T) < 0 | max(T) > 100) {
    stop("\nTemperature outside range for liquid water.\n")
  }
  kvisc <- dvisc(T = T, units = "SI")/dens(T = T, units = "SI")
  if (units == "Eng") {
    # for Eng units, convert from m2 s-1 to ft2 s-1
    kvisc <- kvisc * 10.8
  }
  if( ret_units ) {
    if (units == "Eng") kvisc <- units::set_units(kvisc,"ft^2/s")
    if (units == "SI") kvisc <- units::set_units(kvisc,"m^2/s")
  }
  return(kvisc)
}
#' @export
#' @rdname waterprops
svp <- function(T = NULL, units = c("SI", "Eng"), ret_units = FALSE) {
  # check to make sure that T is given
  if( class(T) == "units" ) T <- units::drop_units(T)
  checks <- c(T)
  units <- units
  if (length(checks) < 1) {
    if (units == "SI") {
      message("\nTemperature not given.\nAssuming T = 20 C\n")
      T = 20
    } else if (units == "Eng") {
      message("\nTemperature not given.\nAssuming T = 68 F\n")
      T = 68
    } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
      stop("Incorrect unit system. Specify either SI or Eng.")
    }
  }
  if (units == "Eng") {
    # convert units if necessary
    T = (T - 32) * 5/9
  }
  if (min(T) < 0 | max(T) > 100) {
    stop("\nTemperature outside range for liquid water.\n")
  }
  #Use eq 17 from Huang, 2018, A Simple Accurate Formula for Calculating 
  #Saturation Vapor Pressure of Water and Ice, Journal of Applied Meteorology 
  #and Climatology, 57(6), 1265-1272
  svp <- exp(34.494-4924.99/(T+237.1))/((T+105)^1.57) #result in Pa

  if (units == "Eng") {
  # for Eng units, convert from Pa to lbf/ft^2
  svp <- svp * 0.0208854
  }
  if( ret_units ) {
    if (units == "Eng") svp <- units::set_units(svp,"lbf/ft^2")
    if (units == "SI") svp <- units::set_units(svp,"Pa")
  }
  return(svp)
}
#' @export
#' @rdname waterprops
surf_tension <- function(T = NULL, units = c("SI", "Eng"), ret_units = FALSE) {
  # check to make sure that T is given
  if( class(T) == "units" ) T <- units::drop_units(T)
  checks <- c(T)
  units <- units
  if (length(checks) < 1) {
    if (units == "SI") {
      message("\nTemperature not given.\nAssuming T = 20 C\n")
      T = 20
    } else if (units == "Eng") {
      message("\nTemperature not given.\nAssuming T = 68 F\n")
      T = 68
    } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
      stop("Incorrect unit system. Specify either SI or Eng.")
    }
  }
  if (units == "Eng") {
    # convert units if necessary
    T = (T - 32) * 5/9
  }
  if (min(T) < 0 | max(T) > 100) {
    stop("\nTemperature outside range for liquid water.\n")
  }
  #Use approximation from Vargaftic, Volkov & Voljak, International Tables of
  #the Surface Tension of Water, J. Phys. Chemn. Ref. Data 12(3), 1983.
  TC <- 647.15
  surf_tension <- 235.8e-3 * ((TC-(T+273.15))/TC)^1.256*(1-0.625*(TC-(T+273.15))/TC)
  #results in N/m  
  if (units == "Eng") {
    # for Eng units, convert from N/m to lbf/ft
    surf_tension <- surf_tension * 0.06852176
  }
  if( ret_units ) {
    if (units == "Eng") surf_tension <- units::set_units(surf_tension,"lbf/ft")
    if (units == "SI") surf_tension <- units::set_units(surf_tension,"N/m")
  }
  return(surf_tension)
}
#' @export
#' @rdname waterprops
Ev <- function(T = NULL, units = c("SI", "Eng"), ret_units = FALSE) {
  # check to make sure that T is given
  if( class(T) == "units" ) T <- units::drop_units(T)
  checks <- c(T)
  units <- units
  if (length(checks) < 1) {
    if (units == "SI") {
      message("\nTemperature not given.\nAssuming T = 20 C\n")
      T = 20
    } else if (units == "Eng") {
      message("\nTemperature not given.\nAssuming T = 68 F\n")
      T = 68
    } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
      stop("Incorrect unit system. Specify either SI or Eng.")
    }
  }
  if (units == "Eng") {
    # convert units if necessary
    T = (T - 32) * 5/9
  }
  if (min(T) < 0 | max(T) > 100) {
    stop("\nTemperature outside range for liquid water.\n")
  }
  #Bulk Modulus of Elasticity at sea-level atmospheric pressure from 
  #Finnemore and Franzini, Fluid Mechanics with Engineering Applications, 
  #10th edition.
  x <- c(0, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100)
  y <- c(2.02,2.06,2.1,2.14,2.18,2.22,2.25,2.28,2.29,2.28,2.25,2.20,2.14,2.07)
  #because bulk modulus is slowly varying, linear interpolation works fine
  Ev <- stats::approx(x,y,T)$y * 10^9   #result in N/m2

  if (units == "Eng") {
    # for Eng units, convert from N/m2 to lbf/ft2
    Ev <- Ev * 0.020885
  }
  if( ret_units ) {
    if (units == "Eng") Ev <- units::set_units(Ev,"lbf/ft^2")
    if (units == "SI") Ev <- units::set_units(Ev,"Pa")
  }
  return(Ev)
}
