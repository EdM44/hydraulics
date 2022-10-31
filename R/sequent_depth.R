#' Solves the Momentum Equation for sequent (or conjugate) depth in a trapezoidal channel
#'
#' This function solves the Momentum equation for water flow in an open 
#' channel with a trapezoidal shape and determines the sequent (conjugate) depth.
#' This is the flow depth either upstream or downstream of a hydraulic jump, 
#' whichever is not provided as input.
#'
#' @param Q numeric vector that contains the flow rate [\eqn{m^3 s^{-1}}{m^3/s} or \eqn{ft^3 s^{-1}}{ft^3/s}]
#' @param b numeric vector that contains the channel bottom width [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param y numeric vector that contains the water depth [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param m numeric vector that contains the side slope of the channel (m:1 H:V) [unitless]
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units. This is used for compatibility with iemisc package.
#' @param ret_units If set to TRUE the value(s) returned are of class \code{units} with
#'   units attached to the value. [Default is FALSE]

#'
#' @return Returns a list including:
#' \itemize{
#'   \item y - input depth
#'   \item y_seq - sequent depth
#'   \item yc - critical depth
#'   \item Fr - Froude number for input depth
#'   \item Fr_seq - Froude number for sequent depth
#'   \item E - specific energy for input depth
#'   \item E_seq - specific energy for sequent depth
#' }
#'
#' @author Ed Maurer
#'
#' @details The Momentum equation for open channel flow conditions in a trapezoidal channel: 
#' \deqn{M = \frac{by^2}{2}+\frac{my^3}{3}+\frac{Q^2}{gy\left(b+my\right)}}
#' where \eqn{C}{C} is 1.0 for SI units and 1.49 for Eng (U.S. Customary) units. 
#' The momentum function is assumed to be the same on both sides of a hydraulic
#' jump, allowing the determination of the sequent depth.
#'
#' @examples
#'
#' #Solving for sequent depth: SI Units
#' #Flow of 0.2 m^3/s, bottom width = 0.5 m, Depth = 0.1 m, side slope = 1:1
#' sequent_depth(Q=0.2,b=0.5,y=0.1,m=1,units = "SI", ret_units = TRUE)
#'
#' @name sequent_depth
NULL

ycfun_t <- function(yc = NULL, Q = NULL, g = NULL, b = NULL, m = NULL) {
  (Q^2 / g) - ((b * yc + m * yc^2)^3)/(b + 2 * m * yc)
  }

Mfun_t <- function(y = NULL, Q = NULL, g = NULL, b = NULL, m = NULL) {
  (b*y^2 / 2) + (m*y^3 / 3) + (Q^2/(g*y*(b+m*y)))
}

Mfun_zero_t <- function(y = NULL, Q = NULL, g = NULL, b = NULL, m = NULL, M = NULL) {
  (b*y^2 / 2) + (m*y^3 / 3) + (Q^2/(g*y*(b+m*y))) - M
}

#attaches units to output if specified
units::units_options(allow_mixed = TRUE)
return_fcns1 <- function(x = NULL, units = NULL, ret_units = FALSE) {
  if (units == "SI") {
    out_units <- c("m","m","m",1,1,"m","m")
  } else {
    out_units <- c("ft","ft","ft",1,1,"ft","ft")
  }    
  if( ret_units ) {
    a <- units::mixed_units(unlist(x), out_units)
    #names(a) <- names(x)
    x <- a
  }
  return(x)
}

#' @export
#' @rdname sequent_depth
sequent_depth <- function (Q = NULL, b = NULL, y = NULL, m = NULL,
                      units = c("SI", "Eng"), ret_units = FALSE ) {

  units <- units

  #check if any values have class 'units' and change to numeric if necessary
  for( i  in c("Q", "b", "y", "m") ) {
    v <- get(i)
    if(inherits(v, "units")) assign(i, units::drop_units(v))
  }
  
  #initial check for missing variables and out of bounds
  if (length(c(Q, b, y, m)) != 4) {
    stop("At least one of Q, b, y, m is missing")
  }
  if (any(c(Q, y) <= 0)) {
    stop("Either Q or y is <= 0. Both of these variables must be positive")
  }
  if (any(c(b, m) < 0)) {
    stop("m (side slope) or b (bottom width) is negative. Both must be >=0")
  }
  if ( !(missing(b)) & !(missing(m)) ) {
    if ( (m == 0) & (b == 0) ) {
      stop("m (side slope) and b (bottom width) are zero. Channel has no area")
    }
  }

  if (units == "SI") {
    g <- 9.80665     # m / s^2
  } else if (units == "Eng") {
    g <- 32.2        #ft / s^2
  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Must be SI or Eng")
  }

  #Geometry for trapezoidal channel
  A <- y * (b + m * y)
  P <- b + 2 * y * sqrt(1 + m ^ 2)
  B <- b + 2 * m * y
  R <- A / P
  D <- A / B
  V <- Q / A
  E = y + V^2/(2*g)
  Fr <- V / (sqrt(g * D))
  yc <- uniroot(ycfun_t, interval = c(0.0000001, 200), Q = Q, g = g, b = b, m = m)$root
  
  #check if input depth is super or subcritical to set interval for seeking root
  #to momentum equation
  lowint = 0.0000001
  highint = 200
  if(y == yc) {
    stop("Input depth is critical depth, sequent depth not determined.")
  } else if (y > yc) {
    highint = yc
  } else {
    lowint = yc
  }
  
  #input momentum function
  M1 <- Mfun_t(y=y, Q=Q, g=g, b=b, m=m)
  y2 <- uniroot(Mfun_zero_t, interval = c(lowint, highint), Q = Q, g = g, b = b, m = m, M = M1)$root
  
  #geometry for sequent depth
  #Geometry for trapezoidal channel
  A2 <- y2 * (b + m * y2)
  V2 <- Q / A2
  B2 <- b + 2 * m * y2
  D2 <- A2 / B2
  E_seq = y2 + V2^2/(2*g)
  Fr2 <- V2 / (sqrt(g * D2))
  
  out <- list(y = y, y_seq = y2, yc = yc, Fr = Fr, Fr_seq = Fr2, E = E, E_seq = E_seq)
  return(return_fcns1(x = out, units = units, ret_units = ret_units))

}