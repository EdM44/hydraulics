#' Creates a cross-section plot for an open channel
#'
#' This function plots a cross-section of a  (trapezoid, rectangle, triangle), 
#' shaded as filled to the level indicated by the values passed to it.
#'
#' @param y water depth  [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param b bottom width  [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param m side slope (H:1)
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units.
#'
#' @return a cross-section diagram
#'
#' @author Ed Maurer
#'
#' @examples
#'
#' # Draw a cross-section with depth 1, width 2, side slope 3:1 (H:V)
#' xc_trap(y = 1.0, b = 2.0, m = 3.0, units = "SI")
#'
#' @import ggplot2
#' @import grid
#'
#' @export
xc_trap <- function(y = NULL, b = NULL, m = NULL, units = c("SI", "Eng")) {
  
  # Check for packages needed to create plot
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("xc_trap diagram plot requires ggplot2 to be installed.",
         call. = FALSE)
  }
  if(!requireNamespace("grid", quietly = TRUE)) {
    stop("xc_trap diagram plot requires grid to be installed.",
         call. = FALSE)
  }
  if( class(b) == "units" ) b <- units::drop_units(b)
  if( class(y) == "units" ) y <- units::drop_units(y)
  if( class(m) == "units" ) m <- units::drop_units(m)

  B <- b + 2 * ( m * y )
  
  if (units == "SI") {
    txt1 <- sprintf("%.2f m",y)
    txt2 <- sprintf("%.2f m",b)
    txt3 <- sprintf("%.2f m",B)
  } else if (units == "Eng") {
    txt1 <- sprintf("%.2f ft",y)
    txt2 <- sprintf("%.2f ft",b)
    txt3 <- sprintf("%.2f ft",B)
  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Must be SI or Eng")
  }
  
  xx <- yy <- NULL
  polyvert <- data.frame(
    xx = c( (B - b)/2., (B + b)/2., B, 0 ),
    yy = c(0, 0, y, y)
  )
  seg1 <- data.frame(xx = c(0, -0.2*y*m),yy = c(y, 1.2*y))
  seg2 <- data.frame(xx = c(B, B + 0.2*y*m),yy = c(y, 1.2*y))
  
  p <- ggplot() +
    ggplot2::geom_polygon(data=polyvert,ggplot2::aes(x=xx, y=yy),fill="lightblue") +
    ggplot2::geom_polygon(data=polyvert,ggplot2::aes(x=xx, y=yy),fill=NA,color="black") +
    ggplot2::geom_segment(data=seg1, ggplot2::aes(x=0, xend=-0.1*y*m, y=y, yend=1.1*y)) +
    ggplot2::geom_segment(data=seg1, ggplot2::aes(x=B, xend=B + 0.1*y*m, y=y, yend=1.1*y)) +
    ggplot2::geom_segment(ggplot2::aes(x=B/2., xend=B/2., y=0, yend=y), 
                          arrow = grid::arrow(length = unit(0.2, "cm"), ends = "both")) +
    ggplot2::annotate(geom="text", x=B/2*1.05, y=y/2, label=txt1, angle = 90, size = 3) +
    ggplot2::geom_segment(ggplot2::aes(x=0, xend=B, y=1.08*y, yend=1.08*y), 
                        arrow = grid::arrow(length = unit(0.2, "cm"), ends = "both")) +
    ggplot2::annotate(geom="text", x=B/2, y=1.2*y, label=txt3, angle = 0, size = 3)
  
  if( b > 0 ) {
    p = p +
      ggplot2::geom_segment(ggplot2::aes(x=(B-b)/2., xend=(B+b)/2., y=-0.22*y, yend=-0.22*y), 
                                    arrow = grid::arrow(length = unit(0.2, "cm"), ends = "both")) +
      ggplot2::annotate(geom="text", x=B/2, y=-0.1*y, label=txt2, angle = 0, size = 3)
  }
  p = p +
    ggplot2::coord_equal() + 
    ggplot2::theme_void()
  return(p)
}

