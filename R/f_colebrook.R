#' Calculates the Darcy-Weisbach Friction Factor f
#'
#' This function calculates the Darcy-Weisbach friction factor
#' and is only provided in this package for use with water in circular pipes
#' while the equation is technically valid for any liquid.
#' As with many parts of this package, techniques and formatting
#' were drawn from Irucka Embry's iemisc package, which includes some
#' methods with similar functionality.
#'
#' @param ks numeric vector that contains the 'equivalent sand roughness height
#'   sand roughness height. Units should be consistent with other input [m or ft]
#' @param V numeric vector that contains the average Velocity of flow in the pipe,
#'   equal to flow divided by area, Q/A [m/s or ft/s]
#' @param D numeric vector that contains the pipe diameter [m or ft]
#'   which should be D >=0.0025 m (0.0082 ft).
#' @param nu numeric vector that contains the kinematic viscocity of water,
#'  [m2 s-1 or ft2 s-1]. Computed with a utility function in water_properties.R:
#'  kvisc(T=T, units=['SI' or 'Eng'])
#' @param Q (for velocity function only) numeric vector that contains the flow rate
#'  [m^3/s or ft^3/s]
#'
#' @return f Returns a numeric vector containing the Darcy-Weisbach friction
#'   factor
#'
#' @author Ed Maurer
#'
#' @seealso \code{\link{kvisc}} for kinematic viscosity, \code{\link{velocity}} for
#' calculating V=Q/A, \code{\link{reynolds_number}} for Reynolds number
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
#' @name colebrook_f
NULL

# velocity function - consistent units needed
#' @export
#' @rdname colebrook_f
velocity <- function(D = NULL, Q = NULL) {
  if (D <= 0.0) {
    stop("\nPositive value needed for diameter\n")
  }
  velocity <- Q/(0.25 * pi * D^2)
  return(velocity)
}
# Reynolds Number Function - consistent units needed
#' @export
#' @rdname colebrook_f
reynolds_number <- function(V = NULL, D = NULL, nu = NULL) {
  checks <- c(V, D, nu)
  if (length(checks) < 3) {
    stop("\nFunction requires V, D and nu as input.\n")
  }
  if (any(checks == 0)) {
    stop("Either V, D, or nu is 0. None of the variables can be 0. Try again.\n")
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
#' @rdname colebrook_f
colebrook <- function(ks, V, D, nu) {

  if (ks/D > 0.01) {
    cat(sprintf("ks/D: %.4f\n", ks/D))
    stop("\nUnitless ks/D value too large, outside applicable range\n")
  }
  Re <- reynolds_number(V = V, D = D, nu = nu)
  if (Re < 2300) {
    cat(sprintf("Reynolds number: %.4f\n", Re))
    stop("\nReynolds number is below 2300, outside applicable range\n")
  }
  f_colebrook <- uniroot(colebrookfcn, interval = c(0.01, 0.08), ks,
                         D, Re)
  # print results
  cat(sprintf("Colebrook f: %.4f\n", f_colebrook$root))
  return(f_colebrook$root)
}
