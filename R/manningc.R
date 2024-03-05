#' Solves the Manning Equation for gravity flow in a circular pipe
#'
#' This function solves the Manning equation for water flow in a circular pipe 
#' at less than full. Uniform flow conditions are assumed, so that the pipe slope 
#' is equal to the slope of the water surface and the energy grade line. This is a 
#' modification of the code prepared by Irucka Embry in his iemisc package.
#' The iemisc::manningcirc function was adapted here for more limited cases
#' commonly used in classroom exercises, additional checks were included to ensure
#' the pipe is flowing less than full, and a cross-section figure is also
#' available. The iemisc::manningcirc and iemisc::manningcircy functions were
#' combined into a single function. Manning n supplied is assumed to be that for
#' full pipe flow; an optional argument may be supplied to account for n varying 
#' with depth.
#'
#' The possible applications of this function for solving the Manning equation
#' for circular pipes are:
#' \tabular{ll}{
#'   \strong{Given} \tab \strong{Solve for} \cr
#'   y_d, Q, Sf, n \tab d \cr
#'   d, Sf, Q, n \tab y \cr
#'   y, d, Q, n \tab Sf \cr
#'   y, d, Sf, n \tab Q \cr
#'   d, Q, Sf, y \tab n
#' }
#'
#' @param Q numeric vector that contains the flow rate [\eqn{m^3 s^{-1}}{m^3/s} or \eqn{ft^3 s^{-1}}{ft^3/s}]
#' @param d numeric vector that contains the pipe diameter [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param Sf numeric vector that contains the slope of the pipe [unitless]
#' @param y numeric vector that contains the water depth [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param y_d numeric vector that contains the ratio of depth to diameter [unitless]
#' @param n numeric vector that contains the Manning roughness coefficient (for full flow or fixed).
#' @param n_var If set to TRUE the value of n for full flow is adjusted with depth. [Default is FALSE]
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units. This is used for compatibility with iemisc package]
#' @param ret_units If set to TRUE the value(s) returned are of class \code{units} with
#' units attached to the value. [Default is FALSE]
#'
#' @return Returns a list including the missing parameter:
#' \itemize{
#'   \item Q - flow rate
#'   \item V - flow velocity
#'   \item A - cross-sectional area of flow
#'   \item P - wetted perimeter
#'   \item R - hydraulic radius (A/P)
#'   \item y - flow depth
#'   \item d - pipe diameter
#'   \item Sf - slope
#'   \item n - Manning's roughness (for full flow, or as adjusted if n_var is TRUE)
#'   \item yc - critical depth
#'   \item Fr - Froude number
#'   \item Re - Reynolds number
#'   \item Qf - Full pipe flow rate
#' }
#'
#' @author Ed Maurer, Irucka Embry
#'
#' @details The Manning equation (also known as the Strickler equation) describes flow conditions in
#' an open channel under uniform flow conditions. It is often expressed as: 
#' \deqn{Q = A\frac{C}{n}{R}^{\frac{2}{3}}{S_f}^{\frac{1}{2}}}
#' where \eqn{C} is 1.0 for SI units and 1.49 for Eng (U.S. Customary) units. Critical depth is 
#' defined by the relation (at critical conditions):
#' \deqn{\frac{Q^{2}B}{g\,A^{3}}=1}{Q^2B/gA^3=1}
#' where \eqn{B}{B} is the top width of the water surface. Since B equals zero for a full pipe, critical 
#' depth is set to the pipe diameter \eqn{d}{d} if the flow \eqn{Q}{Q} exceeds a value that would produce a 
#' critical flow at \eqn{\frac{y}{d}=0.99}{y/d=0.99}.
#'
#' @examples
#'
#' #Solving for flow rate, Q: SI Units
#' manningc(d = 0.6, n = 0.013, Sf = 1./400., y = 0.24, units = "SI")
#' #returns 0.1 m3/s
#'
#' #Solving for Sf, if d=600 mm and pipe is to flow half full
#' manningc(d = 0.6, Q = 0.17, n = 0.013, y = 0.3, units = "SI")
#' #returns required slope of 0.003
#'
#' #Solving for diameter, d when given y_d): Eng (US) units
#' manningc(Q = 83.5, n = 0.015, Sf = 0.0002, y_d = 0.9, units = "Eng")
#' #returns 7.0 ft required diameter
#' 
#' #Solving for depth, y when given Q: SI units
#' manningc(Q=0.01, n=0.013, Sf=0.001, d = 0.2, units="SI")
#' #returns depth  y = 0.158 m, critical depth, yc = 0.085 m
#'
#' #Solving for depth, y when given Q: SI units, and n variable with depth
#' manningc(Q=0.01, n=0.013, Sf=0.001, d = 0.2, n_var = TRUE, units="SI")
#' #returns depth  y = 0.174 m, critical depth, yc = 0.085 m
#' 
#' @seealso \code{\link{xc_circle}} for a cross-section diagram of the circular channel
#'
#' @importFrom pracma fsolve
#'
#' @name manningc
NULL

# Function for finding critical depth
ycfunc <- function(Q = NULL, d = NULL, g = NULL) {
  # Find theta for 99% full (y/D = 0.99)
  thetafull <- 2 * acos(1 - (2 * (0.99)))
  Qcfull <- sqrt(g * d^5 * ((thetafull - sin(thetafull))^3)/(8^3 * sin(thetafull/2.0)))
  if ( Q >= Qcfull ) {
    yc <- d
  } else {
    thetafun <- function(theta) {Q^2 / ( g * d^5 ) - ((theta - sin(theta))^3)/(8^3 * sin(theta/2.0))}
    theta <- uniroot(thetafun, interval = c(0.00001, 2*pi))$root
    yc <- (d / 2) * (1 - cos(theta / 2))
  }
  return(yc)
}

Qfull <- function(d = NULL, Sf = NULL, n = NULL, k = NULL) {
  Vf <- (k / n) * ((d / 4.0) ^ (2.0/3.0)) * sqrt( Sf )
  Qf <- Vf * (0.25 * pi * d^2)
  return(Qf)
}

# Calculates n/nf using Eq. 30 from Akgiray, 2005, Can. J. Civ. Eng. 32: 490-499
n_adj_func <- function(yd) {
  X <- 1 - yd
  n_nf <- 1 - 0.8627*X^5 + 0.4281*X^4 + 0.7626*X^3 - 1.02*X^2 + 0.8057*X
  return(max(n_nf, 1.0))
}

# Attaches units to output if specified
units::units_options(allow_mixed = TRUE)
return_fcn2 <- function(x = NULL, units = NULL, ret_units = FALSE) {
  if (units == "SI") {
    out_units <- c("m^3/s","m/s","m^2","m","m","m","m",1,1,"m",1,1,"m^3/s")
  } else {
    out_units <- c("ft^3/s","ft/s","ft^2","ft","ft","ft","ft",1,1,"ft",1,1,"ft^3/s")
  }    
  if( ret_units ) {
    a <- units::mixed_units(unlist(x), out_units)
    #names(a) <- names(x)
    x <- a
  }
  return(x)
}

#' @export
#' @rdname manningc
manningc <- function (Q = NULL, n = NULL, Sf = NULL, y = NULL, d = NULL, y_d = NULL,
                      n_var = FALSE, units = c("SI", "Eng"), ret_units = FALSE ) {
  units <- units
  if (length(units) != 1) stop("Incorrect unit system. Specify either SI or Eng.")

  #check if any values have class 'units' and change to numeric if necessary
  for( i  in c("Q", "n", "Sf", "y", "d", "y_d") ) {
    v <- get(i)
    if(inherits(v, "units")) assign(i, units::drop_units(v))
  }
  
  #initial check for missing variables and out of bounds
  if(missing(y_d)) {
    if (length(c(Q, n, Sf, y, d)) != 4) {
      stop("There must be exactly one unknown variable among Q, n, Sf, y, d")
    }
    if (any(c(Q, n, Sf, y, d) <= 0)) {
      stop("Either Q, n, Sf, y, d is <= 0. All of these variables must be positive")
    }
    if ( ( ! missing(d) ) & ( ! missing(y) ) ) {
      if ( y > d ) stop("depth y cannot exceed diameter d.")
    }
    case <- 1
  } else {
    if ( (length(c(Q, Sf, n)) != 3) & (! missing(d))  & (! missing(y)) ) {
      stop("d, y must be missing when given y_d.")
    }
    if (any(c(Q, Sf, n, y_d) <= 0)) {
      stop("Either Q, n, Sf, y_d is <= 0. All of these variables must be positive")
    }
    if ( y_d > 1.0 ) {
      stop("y_d cannot exceed 1.0")
    }
    case <- 2
  }

  if (units == "SI") {
    g <- 9.80665     # m / s^2
    k <- 1.0
    mu <- dvisc(T = 20, units = 'SI')
    rho <- dens(T = 20, units = 'SI')
    dmax <- 3.5      #m maximum pipe size
    
  } else if (units == "Eng") {
    g <- 32.2        #ft / s^2
    k <- 1.4859
    mu <- dvisc(T = 68, units = 'Eng')
    rho <- dens(T = 68, units = 'Eng')
    dmax <- 12      #ft

  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Must be SI or Eng")
  }

  #########CASE 1########################
  if (case == 1) {
    if (missing(Q)) {
      nf <- n
      if (n_var == TRUE) n <- n * n_adj_func(y/d)
      Qf <- Qfull(d = d, Sf = Sf, n = nf, k = k)
      theta <- 2 * acos(1 - (2 * (y / d)))
      A <- (theta - sin(theta)) * (d ^ 2 / 8)
      P <- ((theta * d) / 2)
      B <- d * sin(theta / 2)
      R <- A / P
      D <- A / B
      Qfun <- function(Q) {Q - ((((theta - sin(theta)) * (d ^ 2 / 8)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((theta * d) / 2) ^ (2 / 3))}
      Quse <- uniroot(Qfun, interval = c(0.0000001, 200), extendInt = "yes")
      Q <- Quse$root
      V <- Q / A
      Re <- (rho * R * V) / mu
      if (Re < 2000) {
        message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
      }
      Fr <- V / (sqrt(g * D))
      yc <- ycfunc(Q = Q, d = d, g = g)
      out <- list(Q = Q, V = V, A = A, P = P, R = R, y = y, d = d, Sf = Sf, n = n, yc = yc, Fr = Fr, Re = Re, Qf = Qf)
      return(return_fcn2(x = out, units = units, ret_units = ret_units))
                             
    } else if (missing(n)) {
      theta <- 2 * acos(1 - (2 * (y / d)))
      A <- (theta - sin(theta)) * (d ^ 2 / 8)
      P <- ((theta * d) / 2)
      B <- d * sin(theta / 2)
      R <- A / P
      D <- A / B
      # In this case, if n_var == TRUE, what is returned is assumed to be the adjusted n value (variable with depth)
      nfun <- function(n) {Q - ((((theta - sin(theta)) * (d ^ 2 / 8)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((theta * d) / 2) ^ (2 / 3))}
      nuse <- uniroot(nfun, interval = c(0.0000001, 200), extendInt = "yes")
      n <- nuse$root
      # If n_var == TRUE, convert n back to n_full for the next calculation
      if (n_var == TRUE) nf <- n / n_adj_func(y/d)
      Qf <- Qfull(d = d, Sf = Sf, n = nf, k = k)
      V <- Q / A
      Re <- (rho * R * V) / mu
      if (Re < 2000) {
        message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
      }
      Fr <- V / (sqrt(g * D))
      yc <- ycfunc(Q = Q, d = d, g = g)
      out <- list(Q = Q, V = V, A = A, P = P, R = R, y = y, d = d, Sf = Sf, n = n, yc = yc, Fr = Fr, Re = Re, Qf = Qf)
      return(return_fcn2(x = out, units = units, ret_units = ret_units))
    
    } else if (missing(Sf)) {
      nf <- n
      if (n_var == TRUE) n <- n * n_adj_func(y/d)
      theta <- 2 * acos(1 - (2 * (y / d)))
      A <- (theta - sin(theta)) * (d ^ 2 / 8)
      P <- ((theta * d) / 2)
      B <- d * sin(theta / 2)
      R <- A / P
      D <- A / B
      Sffun <- function(Sf) {Q - ((((theta - sin(theta)) * (d ^ 2 / 8)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((theta * d) / 2) ^ (2 / 3))}
      Sfuse <- uniroot(Sffun, interval = c(0.0000001, 200), extendInt = "yes")
      Sf <- Sfuse$root
      Qf <- Qfull(d = d, Sf = Sf, n = nf, k = k)
      V <- Q / A
      Re <- (rho * R * V) / mu
      if (Re < 2000) {
        message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
      }
      Fr <- V / (sqrt(g * D))
      yc <- ycfunc(Q = Q, d = d, g = g)
      out <- list(Q = Q, V = V, A = A, P = P, R = R, y = y, d = d, Sf = Sf, n = n, yc = yc, Fr = Fr, Re = Re, Qf = Qf)
      return(return_fcn2(x = out, units = units, ret_units = ret_units))
      
    } else if (missing(y)) {
      nf <- n
      Qf <- Qfull(d = d, Sf = Sf, n = nf, k = k)
      if ( Q > Qf ) {
        stop("Flow Q exceeds full flow for the pipe.")
      }
      if (n_var == TRUE) {
        # Find the depth with n = nf, used as initial guess in search for y
        rh <- (n * Q) / (k * sqrt(Sf))
        thetafun <- function (theta) ((theta - sin(theta)) * (d ^ 2 / 8)) * (((theta - sin(theta)) * (d ^ 2 / 8) / ((theta * d) / 2)) ^ (2 / 3)) - rh
        thetause <- uniroot(thetafun, c(0.001, 2*pi), extendInt = "yes")
        theta <- thetause$root
        y_fixedn <- (d / 2) * (1 - cos(theta / 2))
        # Find n that would fill pipe at given Q, S, d -- upper bound for n
        #n_full <- k / Q * (pi/4)*d^2 * (d/4)^(2/3) * sqrt(Sf)
        #set up 2 unknowns (x) and 2 equations (y)
        fnc_trial <- function(x) {
          n1 <- x[1]
          y1 <- x[2]
          X <- 1 - y1/d
          theta <- 2 * acos(1 - (2 * (y1 / d)))
          A <- (theta - sin(theta)) * (d ^ 2 / 8)
          P <- ((theta * d) / 2)
          R <- A / P
          V1 <- (k / n1) * (R^(2.0/3.0))*sqrt(Sf)
          Q1 <- V1 * A
          y <- numeric(length(x))
          # n/nf for estimated for the depth equals the n/nf
          y[1] <- 1 - 0.8627*X^5 + 0.4281*X^4 + 0.7626*X^3 - 1.02*X^2 + 0.8057*X - n1/nf
          # flow equals the calculated flow given n and 
          y[2] <- Q - Q1
          return(y)
        }
        #provide initial guesses for unknowns and run the fsolve command
        xstart <- c(nf, y_fixedn)
        z <- pracma::fsolve(fnc_trial, xstart)
        n <- z$x[1]
        y <- z$x[2]
        if (y > d) stop("feasible solution not found\n")
        theta <- 2 * acos(1 - (2 * (y / d)))
      } 
      else {
        rh <- (n * Q) / (k * sqrt(Sf))
        thetafun <- function (theta) ((theta - sin(theta)) * (d ^ 2 / 8)) * (((theta - sin(theta)) * (d ^ 2 / 8) / ((theta * d) / 2)) ^ (2 / 3)) - rh
        thetause <- uniroot(thetafun, c(0.001, 2*pi), extendInt = "yes")
        theta <- thetause$root
        y <- (d / 2) * (1 - cos(theta / 2))
      }
      A <- (theta - sin(theta)) * (d ^ 2 / 8)
      P <- ((theta * d) / 2)
      B <- d * sin(theta / 2)
      R <- A / P
      D <- A / B
      V <- Q / A
      Re <- (rho * R * V) / mu
      if (Re < 2000) {
        message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
      }
      Fr <- V / (sqrt(g * D))
      yc <- ycfunc(Q = Q, d = d, g = g)
      out <- list(Q = Q, V = V, A = A, P = P, R = R, y = y, d = d, Sf = Sf, n = n, yc = yc, Fr = Fr, Re = Re, Qf = Qf)
      return(return_fcn2(x = out, units = units, ret_units = ret_units))
    }
  }
    #########CASE 2########################
  if (case == 2) {
    nf <- n
    if (n_var == TRUE) n <- n * n_adj_func(y_d)
    theta <- 2 * acos(1 - (2 * (y_d)))
    rh <- (n * Q) / (k * sqrt(Sf))
    dfun <- function (d) ((theta - sin(theta)) * (d ^ 2 / 8)) * (((theta - sin(theta)) * (d ^ 2 / 8) / ((theta * d) / 2)) ^ (2 / 3)) - rh
    duse <- uniroot(dfun, interval = c(0.001, dmax), extendInt = "yes")
    d <- duse$root
    Qf <- Qfull(d = d, Sf = Sf, n = nf, k = k)
    y <- y_d * d
    A <- (theta - sin(theta)) * (d ^ 2 / 8)
    P <- ((theta * d) / 2)
    B <- d * sin(theta / 2)
    R <- A / P
    D <- A / B
    V <- Q / A
    Re <- (rho * R * V) / mu
    if (Re < 2000) {
      message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
    }
    Fr <- V / (sqrt(g * D))
    yc <- ycfunc(Q = Q, d = d, g = g)
    out <- list(Q = Q, V = V, A = A, P = P, R = R, y = y, d = d, Sf = Sf, n = n, yc = yc, Fr = Fr, Re = Re, Qf = Qf)
    return(return_fcn2(x = out, units = units, ret_units = ret_units))
  }
}
