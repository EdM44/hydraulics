#' Creates a cross-section plot for a partially filled pipe
#'
#' This function plots a cross-section of a circular pipe, shaded as filled
#' to the level indicated by the depth and diameter values passed to it.
#'
#' @param y water depth [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param d pipe diameter [\eqn{m}{m} or \eqn{ft}{ft}]
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
#' # Draw a cross-section with diameter 1.0 and depth 0.7
#' xc_circle(y = 0.7, d = 1.0, units = "SI")
#'
#' @import ggplot2
#' @import grid
#'
#' @export
xc_circle <- function(y = NULL, d = NULL, units = c("SI", "Eng")) {

  units <- units
  if (length(units) != 1) stop("Incorrect unit system. Specify either SI or Eng.")
  
  if( inherits(y, "units") ) y <- units::drop_units(y)
  if( inherits(d, "units") ) d <- units::drop_units(d)
  
  # Check for packages needed to create plot
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("xc_circle diagram plot requires ggplot2 to be installed.",
         call. = FALSE)
  }
  if(!requireNamespace("grid", quietly = TRUE)) {
    stop("xc_circle diagram plot requires grid to be installed.",
         call. = FALSE)
  }

  #function adapted from solutions at
  #https://stackoverflow.com/questions/12794596/how-fill-part-of-a-circle-using-ggplot2
  xx <- yy <- NULL
  circleFun <- function(center=c(0.5,0.5), diameter=1, npoints=1000, start=0, end=2){
    tt <- seq((start-0.5)*pi, (end-0.5)*pi, length.out=npoints)
    dat <- data.frame(xx = center[1] + diameter / 2 * cos(tt),yy = center[2] + diameter / 2 * sin(tt))
    return(dat)
  }

  if (units == "SI") {
    txt1 <- sprintf("%.3f m",y)
    txt2 <- sprintf("%.3f m",d)
  } else if (units == "Eng") {
    txt1 <- sprintf("%.3f ft",y)
    txt2 <- sprintf("%.3f ft",d)
  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Must be SI or Eng")
  }
  
  hoverd <- y / d
  angle <- acos(1-2*hoverd)/pi
  dat <- circleFun(start=0, end=2)
  dat2 <- circleFun(start=-angle, end=angle)
  ggplot2::ggplot() +
    ggplot2::geom_polygon(data=dat2,ggplot2::aes(x=xx, y=yy),fill="lightblue") +
    ggplot2::geom_polygon(data=dat,ggplot2::aes(x=xx, y=yy),fill=NA,color="black") +
    ggplot2::geom_segment(ggplot2::aes(x=-0.1, xend=-0.1, y=0, yend=1.0), arrow = grid::arrow(length = unit(0.5, "cm"), ends = "both")) +
    ggplot2::annotate(geom="text", x=-0.05, y=0.5, label=txt2, angle = 90) +
    ggplot2::geom_segment(ggplot2::aes(x=0.5, xend=0.5, y=0, yend=hoverd), arrow = grid::arrow(length = unit(0.5, "cm"), ends = "both")) +
    ggplot2::annotate(geom="text", x=0.55, y=hoverd/2.0, label=txt1, angle = 90) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()

}
