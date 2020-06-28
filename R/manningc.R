#' Solves the Manning Equation for gravity flow in a circular pipe
#'
#' This function solves the Manning equation for water 
#' flow in a circular pipe at less than full. This is a modification of the 
#' comprehensive code prepared by Irucka Embry in his iemisc package. Specifically
#' the iemisc::manningcirc function was adapted here for more limited cases
#' commonly used in classroom exercises, additional checks were included to ensure
#' the pipe is flowing less than full, and a cross-section figure is also
#' available. The iemisc::manningcirc and iemisc::manningcircy sunctions were also 
#' combined into a single function.
#' 
#' A common form of the Manning equation is:
#'
#' \deqn{Q = \frac{K_n}{n}\frac{A^\frac{5}{3}}{P^\frac{2}{3}}\sqrt{S}}
#'
#' \describe{
#'	\item{\emph{Q}}{the discharge [m^3/s or ft^3/s (cfs)]}
#'	\item{\emph{n}}{Manning's roughness coefficient (dimensionless)}
#'	\item{\emph{P}}{the wetted perimeter (m or ft)}
#'	\item{\emph{A}}{the cross-sectional area (m^2 or ft^2)}
#'	\item{\emph{S}}{the slope of the channel bed (m/m or ft/ft)}
#'	\item{\emph{\eqn{K_n}}}{the conversion constant: 1.0 (SI) and 1.4859 (Eng)}
#' }
#' 
#' The possible applications of this function for circular pipes are:
#' \tabular{ll}{
#'   \strong{Given} \tab \strong{Solve for} \cr
#'   y_d, Q, Sf, n \tab d \cr
#'   d, Sf, Q, n \tab y \cr
#'   y, d, Q, n \tab Sf \cr
#'   y, d, Sf, n \tab Q \cr
#'   d, Q, Sf, y \tab n 
#' }
#'
#' @param Q numeric vector that contains the flow rate [m^3/s or or ft^3/s]
#' @param d numeric vector that contains the pipe diameter [m or ft]
#' @param Sf numeric vector that contains the slope of the pipe [unitless]
#' @param y numeric vector that contains the water depth [m or ft]
#' @param y_d numeric vector that contains the ratio of depth to diameter [unitless]
#' @param n numeric vector that contains the Manning roughness coefficient.
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units. This is used for compatibility with iemisc package]
#'
#' @return Returns a list including the missing parameter:
#' \itemize{
#'   \item Q - flow rate
#'   \item V - flow velocity
#'   \item A - cross-sectional area of flow
#'   \item P - wetted perimeter
#'   \item R - hydraulic radius
#'   \item y - flow depth
#'   \item d - pipe diameter
#'   \item Sf - slope
#'   \item n - Manning's roughness
#'   \item Fr - Froude number
#'   \item Re - Reynolds number
#'   \item Qf - Full pipe flow rate
#' }
#'
#' @author Ed Maurer, Irucka Embry
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
#' @name manningc
NULL

Qfull <- function(d = NULL, Sf = NULL, n = NULL, k = NULL) {
  Vf <- (k / n) * ((d / 4.0) ^ (2.0/3.0)) * sqrt( Sf )
  Qf <- Vf * (0.25 * pi * d^2)
  return(Qf)
}

#' @export
#' @rdname manningc
manningc <- function (Q = NULL, n = NULL, Sf = NULL, y = NULL, d = NULL, y_d = NULL, 
                      units = c("SI", "Eng")) {

  units <- units
  
  #initial check for missing variables and out of bounds
  if(missing(y_d)) {
    if (length(c(Q, n, Sf, y, d)) != 4) {
      stop("There must be exactly one unknown variable among Q, n, Sf, y, d")
    }
    if (any(c(Q, n, Sf, y, d) <= 0)) {
      stop("Either Q, n, Sf, y, d is <= 0. All of these variables must be positive")
    }
    if ( (! missing(d)) & (! missing(y)) & ( y > d ) ) {
      stop("depth y cannot exceed diameter d.")
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
      Qf <- Qfull(d = d, Sf = Sf, n = n, k = k)
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
      #add cross-section plot########################3
      return(list(Q = Q, V = V, A = A, P = P, R = R, y = y, d = d, Sf = Sf, n = n, Fr = Fr, Re = Re, Qf = Qf))

      } else if (missing(n)) {
      theta <- 2 * acos(1 - (2 * (y / d)))
      A <- (theta - sin(theta)) * (d ^ 2 / 8)
      P <- ((theta * d) / 2)
      B <- d * sin(theta / 2)
      R <- A / P
      D <- A / B
      nfun <- function(n) {Q - ((((theta - sin(theta)) * (d ^ 2 / 8)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((theta * d) / 2) ^ (2 / 3))}
      nuse <- uniroot(nfun, interval = c(0.0000001, 200), extendInt = "yes")
      n <- nuse$root
      Qf <- Qfull(d = d, Sf = Sf, n = n, k = k)
      V <- Q / A
      Re <- (rho * R * V) / mu
      if (Re < 2000) {
        message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
      }
      Fr <- V / (sqrt(g * D))
      return(list(Q = Q, V = V, A = A, P = P, R = R, y = y, d = d, Sf = Sf, n = n, Fr = Fr, Re = Re, Qf = Qf))

      } else if (missing(Sf)) {
        theta <- 2 * acos(1 - (2 * (y / d)))
        A <- (theta - sin(theta)) * (d ^ 2 / 8)
        P <- ((theta * d) / 2)
        B <- d * sin(theta / 2)
        R <- A / P
        D <- A / B
        Sffun <- function(Sf) {Q - ((((theta - sin(theta)) * (d ^ 2 / 8)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((theta * d) / 2) ^ (2 / 3))}
        Sfuse <- uniroot(Sffun, interval = c(0.0000001, 200), extendInt = "yes")
        Sf <- Sfuse$root
        Qf <- Qfull(d = d, Sf = Sf, n = n, k = k)
        V <- Q / A
        Re <- (rho * R * V) / mu
        if (Re < 2000) {
          message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
        }
        Fr <- V / (sqrt(g * D))
        return(list(Q = Q, V = V, A = A, P = P, R = R, y = y, d = d, Sf = Sf, n = n, Fr = Fr, Re = Re, Qf = Qf))

    } else if (missing(y)) {
      Qf <- Qfull(d = d, Sf = Sf, n = n, k = k)
      if ( Q > Qf ) {
        stop("Flow Q exceeds full flow for the pipe")
      }
      rh <- (n * Q) / (k * sqrt(Sf))
      thetafun <- function (theta) ((theta - sin(theta)) * (d ^ 2 / 8)) * (((theta - sin(theta)) * (d ^ 2 / 8) / ((theta * d) / 2)) ^ (2 / 3)) - rh
      thetause <- uniroot(thetafun, c(-1000, 1000), extendInt = "yes")
      theta <- thetause$root
      y <- (d / 2) * (1 - cos(theta / 2))
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
      return(list(Q = Q, V = V, A = A, P = P, R = R, y = y, d = d, Sf = Sf, n = n, Fr = Fr, Re = Re, Qf = Qf))
    }
  }
    #########CASE 2########################
  if (case == 2) {
    theta <- 2 * acos(1 - (2 * (y_d)))
    rh <- (n * Q) / (k * sqrt(Sf))
    dfun <- function (d) ((theta - sin(theta)) * (d ^ 2 / 8)) * (((theta - sin(theta)) * (d ^ 2 / 8) / ((theta * d) / 2)) ^ (2 / 3)) - rh
    duse <- uniroot(dfun, interval = c(0.001, dmax), extendInt = "yes")
    d <- duse$root
    Qf <- Qfull(d = d, Sf = Sf, n = n, k = k)
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
    return(list(Q = Q, V = V, A = A, P = P, R = R, y = y, d = d, Sf = Sf, n = n, Fr = Fr, Re = Re, Qf = Qf))
  }
}
