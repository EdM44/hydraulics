#' Solves the Darcy-Weisbach Equation for the either head loss (hf),
#' flow rate (Q), or diameter (D).
#'
#' This function solves the Darcy-Weisbach friction loss equation
#' for with water in circular pipes. the function solves for
#' either head loss (hf), flow rate (Q), or diameter (D),
#' whichever is missing (not included as an argument).
#' As with many parts of this package, techniques and formatting
#' were drawn from Irucka Embry's iemisc package, which includes some
#' methods with similar functionality.
#'
#' @param Q numeric vector that contains the flow rate [m^3/s or or ft^3/s]
#' @param D numeric vector that contains the pipe diameter [m or ft]
#' @param hf numeric vector that contains the head loss through the pipe section [m or ft]
#' @param L numeric vector that contains the pipe length [m or ft],
#' @param ks numeric vector that contains the 'equivalent sand roughness height
#'   sand roughness height. Units should be consistent with other input [m or ft]
#' @param nu numeric vector that contains the kinematic viscosity of water,
#'  [m2 s-1 or ft2 s-1].
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units. This is used for compatibility with iemisc package
#'
#' @return Returns a list including the missing parameter (hf, Q, or D):
#' \itemize{
#'   \item Q - flow rate.
#'   \item V - flow velocity.
#'   \item L - pipe length.
#'   \item hf - head loss due to friction
#'   \item f - Darcy-Weisbach friction factor
#'   \item Re - Reynolds number
#' }
#'
#' @examples
#'
#' #Type 2 (solving for flow rate, Q): SI Units
#' D <- .5
#' L <- 10
#' hf <- 0.006*L
#' T <- 20
#' ks <- 0.000046
#' darcyweisbach(D = D, hf = hf, L = L, ks = ks, nu = kvisc(T=T, units='SI'), units = c('SI'))
#'
#' #Type 3 (solving for diameter, D): Eng (US) units
#' Q <- 37.5     #flow in ft^3/s
#' L <- 8000     #pipe length in ft
#' hf <- 215     #head loss due to friction, in ft
#' T <- 68       #water temperature, F
#' ks <- 0.0008  #pipe roughness, ft
#' darcyweisbach(Q = Q, hf = hf, L = L, ks = ks, nu = kvisc(T=T, units='Eng'), units = c('Eng'))
#'
#' @name darcyweisbach
NULL
type3_fcn <- function(D, Q, hf, L, ks, nu, g) {
  # -2.0*sqrt((2*D*g*hf)/L)*log((ks/D)/3.7 +
  # (2.51*(nu/D)*sqrt(L/(g*D*hf)))) - 4*Q/(pi * D^2)
  -0.965 * D^2 * sqrt((D * g * hf)/L) * log((ks/D)/3.7 + (1.784 * (nu/D)/sqrt((g * D * hf/L)))) - Q
}
#' @export
#' @rdname darcyweisbach
darcyweisbach <- function(Q = NULL, D = NULL, hf = NULL, L = NULL, ks = NULL,
                          nu = NULL, units = c("SI", "Eng")) {
  checks <- c(Q, D, hf, L, ks, nu)
  units <- units
  if (length(checks) < 5) {
    stop("There are not at least 5 known variables. Try again with at least 5 known variables.")
  }
  if (length(c(Q, D, hf)) != 2) {
    stop("One of the three variables Q, D, hf must be missing.")
  }
  if (any(c(Q, D, hf, L, nu) == 0)) {
    stop("Either Q, D, hf, L, nu is 0. None of the variables can be 0. Try again.")
  }
  if (units == "SI") {
    g <- 9.80665  # m / s^2
    dmin <- 0.00025  #m smallest pipe size
    dmax <- 3.5  #m maximum pipe size
  } else if (units == "Eng") {
    g <- 32.2  #ft / s^2
    dmin <- 0.0082  #ft
    dmax <- 12  #ft
  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Try again.")
  }
  if (missing(hf)) {
    message(sprintf("hf missing: solving a Type 1 problem\n"))
    if (D < dmin | D > dmax) {
      stop(sprintf("Diameter: %.5f. outside allowable range: %.5f, %.5f\n",D, dmin,dmax))
    }
    V <- velocity(D, Q)
    Re <- reynolds_number(V = V, D = D, nu = nu)
    f <- colebrook(ks = ks, V = V, D = D, nu = nu)
    hf <- f * L * V^2/(D * 2 * g)
    return(list(Q = Q, V = V, L = L, D = D, hf = hf, f = f, Re = Re))
  }
  if (missing(Q)) {
    message(sprintf("Q missing: solving a Type 2 problem\n"))
    if (D < dmin | D > dmax) {
      stop(sprintf("Diameter: %.5f. outside allowable range: %.5f, %.5f\n",D, dmin,dmax))
    }
    # solve with explicit equation for velocity - any consistent units
    pt1 <- (ks/D)/3.7 + (2.51 * (nu/D) * sqrt(L/(g * D * hf)))
    V <- -2 * sqrt((2 * D * g * hf)/L) * log10(pt1)
    Q <- V * pi * (D/2)^2
    Re <- reynolds_number(V = V, D = D, nu = nu)
    f <- colebrook(ks = ks, V = V, D = D, nu = nu)
    return(list(Q = Q, V = V, L = L, D = D, hf = hf, f = f, Re = Re))
  }
  if (missing(D)) {
    message(sprintf("D missing: solving a Type 3 problem\n"))
    D <- uniroot(type3_fcn, interval = c(dmin, dmax), Q, hf, L, ks, nu, g)$root
    Re <- reynolds_number(V = velocity(D = D, Q = Q), D = D, nu = nu)
    f <- colebrook(ks = ks, V = velocity(D = D, Q = Q), D = D, nu = nu)
    return(list(Q = Q, V = velocity(D = D, Q = Q), L = L, D = D, hf = hf,
                f = f, Re = Re))
  }
}
