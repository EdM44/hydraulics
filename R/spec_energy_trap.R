#' Creates a specific energy diagram for a trapezoidal channel
#'
#' This function plots a specific energy diagram of a trapezoidal
#' (including rectangular and triangular) channel, with annotation of
#' critical depth and minimum specific energy.
#'
#' @param Q flow rate [\eqn{m^3 s^{-1}}{m^3/s} or \eqn{ft^3 s^{-1}}{ft^3/s}]
#' @param b bottom width [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param m side slope (H:1) [unitless]
#' @param scale multiplier (of yc) for axis scales (default is 3)
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units.
#'
#' @return a specific energy diagram
#'
#' @author Ed Maurer
#'
#' @details Specific Energy, E, is the energy, expressed as a head (i.e., the mechanical energy 
#' per unit weight of the water, with units of length) relative to the channel bottom.
#' It is calculated as: 
#' \deqn{E = y+\alpha\frac{Q^{2}}{2g\,A^{2}} = y+\alpha\frac{V^{2}}{2g}}
#' where \eqn{y} is flow depth, \eqn{A} is the cross-sectional flow area, \eqn{{V}=\frac{Q}{A}}, and 
#' and \eqn{\alpha} is a kinetic energy correction factor to account for non-uniform velocities across 
#' the cross-section;  \eqn{\alpha=1.0} in this function (as is commonly assumed).
#'
#' @examples
#'
#' # Draw a specific cross-section with flow 1, width 2, side slope 3:1 (H:V)
#' spec_energy_trap(Q = 1.0, b = 2.0, m = 3.0, scale = 4, units = "SI")
#'
#' @import ggplot2
#' @import grid
#'
#' @export
spec_energy_trap <- function(Q = NULL, b = NULL, m = NULL, scale = 3,
                             units = c("SI", "Eng")) {

  # Check for packages needed to create plot
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("xc_trap diagram plot requires ggplot2 to be installed.",
         call. = FALSE)
  }
  if(!requireNamespace("grid", quietly = TRUE)) {
    stop("xc_trap diagram plot requires grid to be installed.",
         call. = FALSE)
  }

  if (length(c(Q, b, m)) != 3) {
    stop("One of required inputs is missing: Q, b, m. b or m may be zero")
  }

  if (any(c(Q, b, m) < 0)) {
    stop("Either Q, b, or m is < 0. All of these variables must be non-negative")
  }

  if ( ( b == 0 ) & ( m == 0 ) ) {
    stop("m (side slope) and b (bottom width) are zero. Channel has no area")
  }

  scalefact <- scale
  units <- units
  if (units == "SI") {
    g <- 9.80665     # m / s^2
    txtx <- sprintf("Specific Energy, E (m)")
    txty <- sprintf("Depth, y (m)")
  } else if (units == "Eng") {
    g <- 32.2        #ft / s^2
    txtx <- sprintf("Specific Energy, E (ft)")
    txty <- sprintf("Depth, y (ft)")
  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Must be SI or Eng")
  }

  ycfun <- function(yc) { (Q^2 / g) - ((b * yc + m * yc^2)^3)/(b + 2 * m * yc)}
  yc <- uniroot(ycfun, interval = c(0.0000001, 200), extendInt = "yes")$root

  Ac <- yc * (b + m * yc)
  Emin <- yc + ((Q ^ 2) / (2 * g * Ac ^ 2))

  ymax <- ceiling(yc*scalefact)

  ys <- seq(0.1*yc,ymax,length=1000)
  As <- ys * (b + m * ys)
  Es <- ys + ((Q ^ 2) / (2 * g * As ^ 2))

  xx <- yy <- NULL
  eycurve <- data.frame( xx = Es , yy = ys )
  eycurve <- subset(eycurve, xx <= ymax,select=c(xx, yy))
  seg1 <- data.frame(xx = c(Emin, Emin),yy = c(0, yc))
  seg2 <- data.frame(xx = c(0, Emin),yy = c(yc, yc))

  txt1 <- sprintf("Emin=%.2f",Emin)
  txt2 <- sprintf("yc=%.2f",yc)

  p <- ggplot() +
    ggplot2::geom_path(data=eycurve,ggplot2::aes(x=xx, y=yy),color="black", size=1.5) +
    ggplot2::scale_x_continuous(txtx, limits = c(0, ymax), expand = c(0,0)) +
    ggplot2::scale_y_continuous(txty, limits = c(0, ymax), expand = c(0,0)) +
    ggplot2::geom_abline(slope = 1, intercept = 0 ,color="black",linetype = "dashed") +
    ggplot2::geom_segment(ggplot2::aes(x=Emin, xend=Emin, y=0, yend=yc)) +
    ggplot2::geom_segment(ggplot2::aes(x=0, xend=Emin, y=yc, yend=yc)) +
    ggplot2::annotate(geom="text", x=Emin*0.92, y=yc/2, label=txt1, angle = 90, size = 3) +
    ggplot2::annotate(geom="text", x=Emin/2, y=yc*1.08, label=txt2, angle = 0, size = 3) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_bw()

  return(p)
}

