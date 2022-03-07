#' Calculates the Darcy-Weisbach Friction Factor f
#'
#' This function calculates the Darcy-Weisbach friction factor
#' and is only provided in this package for use with water in circular pipes
#' while the equation is technically valid for any liquid or channel.
#' As with many parts of this package, techniques and formatting
#' were drawn from Irucka Embry's iemisc package, which includes some
#' methods with similar functionality. Two utility functions are included for velocity 
#' and Reynolds Number.
#'
#' @param ks numeric vector that contains the 'equivalent sand roughness height
#'   sand roughness height. Units should be consistent with other input [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param V numeric vector that contains the average Velocity of flow in the pipe,
#'   equal to flow divided by area, \eqn{\frac{Q}{A}}{Q/A} [\eqn{m\,s^{-1}}{m/s} or \eqn{ft\,s^{-1}}{ft/s}]
#' @param D numeric vector that contains the pipe diameter [\eqn{m}{m} or \eqn{ft}{ft}]
#'   which should be D >=0.0025 m (0.0082 ft).
#' @param nu numeric vector that contains the kinematic viscosity of water,
#'  [\eqn{m^2 s^{-1}}{m^2/s} or \eqn{ft^2 s^{-1}}{ft^2/s}]. Computed with a utility function in water_properties.R:
#'  kvisc(T=T, units=['SI' or 'Eng'])
#' @param Q (for velocity function only) numeric vector that contains the flow rate
#'  [\eqn{m^3 s^{-1}}{m^3/s} or \eqn{ft^3 s^{-1}}{ft^3/s}]
#'
#' @return f Returns a numeric vector containing the Darcy-Weisbach friction
#'   factor
#'
#' @details The Colebrook-White equation was developed to estimate the Darcy-Weisbach friction factor
#' for commercial pipes under turbulent flow conditions. It is recommended for pipe diameters greater 
#' than 2.5 mm (0.1 inch).  The equation is: \deqn{\frac{1}{\sqrt{f}} = -2\log\left(\frac{\frac{ks}{D}}{3.7} + \frac{2.51}{Re\sqrt{f}}\right)}
#' where \eqn{Re = \frac{VD}{nu}} is the unitless Reynolds Number.
#' 
#' @author Ed Maurer
#'
#' @seealso \code{\link{kvisc}} for kinematic viscosity, \code{\link{velocity}} for
#' calculating \eqn{V=\frac{Q}{A}}{V=Q/A}, \code{\link{reynolds_number}} for Reynolds number
#'
#' @examples
#'
#' # A Type 1 problem (solve for hf): US units
#' D <- 20/12   #diameter of 20 inches
#' Q <- 4       #flow in ft^3/s
#' T <- 60      #water temperature in F
#' ks <- 0.0005 #pipe roughness in ft
#'
#' f <- colebrook(ks=ks,V=velocity(D,Q), D=D, nu=kvisc(T=T, units="Eng"))
#' 
#' @importFrom utils tail
#' @importFrom stats uniroot
#'
#' @name colebrook
NULL

# velocity function - consistent units needed
#' @export
#' @rdname colebrook
velocity <- function(D = NULL, Q = NULL) {
  if (D <= 0.0) {
    stop("\nPositive value needed for diameter\n")
  }
  velocity <- Q/(0.25 * pi * D^2)
  return(velocity)
}
# Reynolds Number Function - consistent units needed
#' @export
#' @rdname colebrook
reynolds_number <- function(V = NULL, D = NULL, nu = NULL) {
  checks <- c(V, D, nu)
  
  #check if any values have class 'units' and change to numeric if necessary
  for( i  in c("V", "D", "nu") ) {
    v <- get(i)
    if(inherits(v, "units")) assign(i, units::drop_units(v))
  }
  
  if (length(checks) < 3) {
    stop("\nFunction requires V, D and nu as input.\n")
  }
  if (D == 0 | nu == 0) {
    stop("Either D, or nu is 0. Neither can be 0. Try again.\n")
  } else {
    reynolds_number <- abs(V) * D/nu
    return(reynolds_number)
  }
}
# friction factor calculated using the Colebrook equation
colebrookfcn <- function(f, ks, D, Re) {
  -2 * log10((ks/D)/3.7 + 2.51/(Re * (f^0.5))) - 1/(f^0.5)
}
# ks and D in same units system, so units unneeded as argument
#' @export
#' @rdname colebrook
colebrook <- function(ks, V, D, nu) {

  #check if any values have class 'units' and change to numeric if necessary
  for( i  in c("ks", "V", "D", "nu") ) {
    v <- get(i)
    if(inherits(v, "units")) assign(i, units::drop_units(v))
  }
  
  if (ks/D > 0.06) {
    stop(sprintf("ks/D: %.4f value > 0.06, outside applicable range\n", ks/D))
  }
  Re <- reynolds_number(V = V, D = D, nu = nu)
  if (Re < 2300) {
    stop(sprintf("Reynolds number: %.4f is below 2300, outside applicable range\n", Re))
  }
  f_colebrook <- uniroot(colebrookfcn, interval = c(0.01, 0.08), ks,
                         D, Re)
  # return results
  #cat(sprintf("Colebrook f: %.4f\n", f_colebrook$root))
  return(f_colebrook$root)
}
