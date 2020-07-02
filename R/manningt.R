#' Solves the Manning Equation for water flow in an open channel
#'
#' This function solves the Manning equation for water
#' flow in an open channel with a trapezoidal shape. This is a modification of the
#' code prepared by Irucka Embry in his iemisc package. Specifically
#' the iemisc::manningtrap, iemisc::manningrect, and iemisc::manningtri were combined
#' and adapted here for more limited cases commonly used in classroom exercises.Some
#' auxiliary variables in the iemisc code are not included here (shear stress, and
#' specific energy), as these will be done separately. A cross-section figure is also
#' available.
#'
#' @param Q numeric vector that contains the flow rate [m^3/s or or ft^3/s]
#' @param b numeric vector that contains the channel bottom width [m or ft]
#' @param Sf numeric vector that contains the slope of the channel [unitless]
#' @param y numeric vector that contains the water depth [m or ft]
#' @param m numeric vector that contains the side slope of the channel (m:1 H:V) [unitless]
#' @param n numeric vector that contains the Manning roughness coefficient
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units. This is used for compatibility with iemisc package.
#'
#' @return Returns a list including the missing parameter:
#' \itemize{
#'   \item Q - flow rate
#'   \item V - flow velocity
#'   \item A - cross-sectional area of flow
#'   \item P - wetted perimeter
#'   \item R - hydraulic radius
#'   \item y - flow depth (normal depth)
#'   \item b - channel bottom width
#'   \item m - channel side slope
#'   \item Sf - slope
#'   \item B - top width of water surface
#'   \item n - Manning's roughness
#'   \item yc - critical depth
#'   \item Fr - Froude number
#'   \item Re - Reynolds number
#' }
#'
#' @author Ed Maurer, Irucka Embry
#'
#' @examples
#'
#' #Solving for flow rate, Q, trapezoidal channel: SI Units
#' manningt(n = 0.013, m = 2, Sf = 0.0005, y = 1.83, b = 3, units = "SI")
#' #returns Q=22.2 m3/s
#'
#' #Solving for roughness, n, rectangular channel: Eng units
#' manningt(Q = 14.56, m = 0, Sf = 0.0004, y = 2.0, b = 4, units = "Eng")
#' #returns Manning n of 0.016
#'
#' #Solving for depth, y, triangular channel: SI units
#' manningt(Q = 1.0, n = 0.011, m = 1, Sf = 0.0065, b = 0, units = "SI")
#' #returns 0.6 m normal flow depth
#'
#' @name manningt
NULL

ycfun <- function(yc = NULL, Q = NULL, g = NULL, b = NULL, m = NULL) {
  (Q^2 / g) - ((b * yc + m * yc^2)^3)/(b + 2 * m * yc)
  }

#' @export
#' @rdname manningt
manningt <- function (Q = NULL, n = NULL, m = NULL, Sf = NULL, y = NULL, b = NULL,
                      units = c("SI", "Eng")) {

  units <- units

  #initial check for missing variables and out of bounds
  if (length(c(Q, n, m, Sf, y, b)) != 5) {
    stop("There must be exactly one unknown variable among Q, n, m, Sf, y, b")
  }
  if (any(c(Q, n, Sf, y) <= 0)) {
    stop("Either Q, n, Sf, y is <= 0. All of these variables must be positive")
  }
  if ( ( b < 0 ) | ( m < 0 ) ) {
    stop("m (side slope) or b (bottom width) is negative. Both must be >=0")
  }
  if ( ( b == 0 ) & ( m == 0 ) ) {
    stop("m (side slope) and b (bottom width) are zero. Channel has no area")
  }

  if (units == "SI") {
    g <- 9.80665     # m / s^2
    k <- 1.0
    mu <- dvisc(T = 20, units = 'SI')
    rho <- dens(T = 20, units = 'SI')
  } else if (units == "Eng") {
    g <- 32.2        #ft / s^2
    k <- 1.4859
    mu <- dvisc(T = 68, units = 'Eng')
    rho <- dens(T = 68, units = 'Eng')
  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Must be SI or Eng")
  }

  if (missing(Q)) {
    A <- y * (b + m * y)
    P <- b + 2 * y * sqrt(1 + m ^ 2)
    B <- b + 2 * m * y
    R <- A / P
    D <- A / B
    Qfun <- function(Q) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}
    Quse <- uniroot(Qfun, interval = c(0.0000001, 200), extendInt = "yes")
    Q <- Quse$root
    V <- Q / A
    Re <- (rho * R * V) / mu
    if (Re < 2000) {
      message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
    }
    Fr <- V / (sqrt(g * D))
    yc <- uniroot(ycfun, interval = c(0.0000001, 200), Q = Q, g = g, b = b, m = m)$root
    return(list(Q = Q, V = V, A = A, P = P, R = R, y = y, b = b, m = m, Sf = Sf, B = B, n = n, yc = yc, Fr = Fr, Re = Re))

    } else if (missing(n)) {
      A <- y * (b + m * y)
      P <- b + 2 * y * sqrt(1 + m ^ 2)
      B <- b + 2 * m * y
      R <- A / P
      D <- A / B
      nfun <- function(n) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}
      nuse <- uniroot(nfun, interval = c(0.0000001, 200), extendInt = "yes")
      n <- nuse$root
      V <- Q / A
      Re <- (rho * R * V) / mu
      if (Re < 2000) {
        message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
      }
      Fr <- V / (sqrt(g * D))
      yc <- uniroot(ycfun, interval = c(0.0000001, 200), Q = Q, g = g, b = b, m = m)$root
      return(list(Q = Q, V = V, A = A, P = P, R = R, y = y, b = b, m = m, Sf = Sf, B = B, n = n, yc = yc, Fr = Fr, Re = Re))

    } else if (missing(m)) {
      mfun <- function(m) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf) * (k / n)) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}
      muse <- uniroot(mfun, interval = c(0, 30), extendInt = "yes")
      m <- muse$root
      A <- y * (b + m * y)
      P <- b + 2 * y * sqrt(1 + m ^ 2)
      B <- b + 2 * m * y
      R <- A / P
      D <- A / B
      V <- Q / A
      Re <- (rho * R * V) / mu
      if (Re < 2000) {
        message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
      }
      Fr <- V / (sqrt(g * D))
      yc <- uniroot(ycfun, interval = c(0.0000001, 200), Q = Q, g = g, b = b, m = m)$root
      return(list(Q = Q, V = V, A = A, P = P, R = R, y = y, b = b, m = m, Sf = Sf, B = B, n = n, yc = yc, Fr = Fr, Re = Re))

    } else if (missing(b)) {
      bfun <- function(b) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}
      buse <- uniroot(bfun, interval = c(0.0000001, 200), extendInt = "yes")
      b <- buse$root
      A <- y * (b + m * y)
      P <- b + 2 * y * sqrt(1 + m ^ 2)
      B <- b + 2 * m * y
      R <- A / P
      D <- A / B
      V <- Q / A
      Re <- (rho * R * V) / mu
      if (Re < 2000) {
        message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
      }
      Fr <- V / (sqrt(g * D))
      yc <- uniroot(ycfun, interval = c(0.0000001, 200), Q = Q, g = g, b = b, m = m)$root
      return(list(Q = Q, V = V, A = A, P = P, R = R, y = y, b = b, m = m, Sf = Sf, B = B, n = n, yc = yc, Fr = Fr, Re = Re))

    } else if (missing(y)) {
      yfun <- function(y) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}
      yuse <- uniroot(yfun, interval = c(0.0000001, 200), extendInt = "yes")
      y <- yuse$root
      A <- y * (b + m * y)
      P <- b + 2 * y * sqrt(1 + m ^ 2)
      B <- b + 2 * m * y
      R <- A / P
      D <- A / B
      V <- Q / A
      Re <- (rho * R * V) / mu
      if (Re < 2000) {
        message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
      }
      Fr <- V / (sqrt(g * D))
      yc <- uniroot(ycfun, interval = c(0.0000001, 200), Q = Q, g = g, b = b, m = m)$root
      return(list(Q = Q, V = V, A = A, P = P, R = R, y = y, b = b, m = m, Sf = Sf, B = B, n = n, yc = yc, Fr = Fr, Re = Re))

    } else if (missing(Sf)) {
      A <- y * (b + m * y)
      P <- b + 2 * y * sqrt(1 + m ^ 2)
      B <- b + 2 * m * y
      R <- A / P
      D <- A / B
      Sffun <- function(Sf) {Q - (((y * (b + m * y)) ^ (5 / 3) * sqrt(Sf)) * (k / n) / ((b + 2 * y * sqrt(1 + m ^ 2)) ^ (2 / 3)))}
      Sfuse <- uniroot(Sffun, interval = c(0.0000001, 200), extendInt = "yes")
      Sf <- Sfuse$root
      V <- Q / A
      Re <- (rho * R * V) / mu
      if (Re < 2000) {
        message(sprintf("Low Reynolds number: %.0f indicates not rough turbulent, Manning eq. not valid\n",Re))
      }
      Fr <- V / (sqrt(g * D))
      yc <- uniroot(ycfun, interval = c(0.0000001, 200), Q = Q, g = g, b = b, m = m)$root
      return(list(Q = Q, V = V, A = A, P = P, R = R, y = y, b = b, m = m, Sf = Sf, B = B, n = n, yc = yc, Fr = Fr, Re = Re))
     }
}

