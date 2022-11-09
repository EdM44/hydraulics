#' Creates a Moody diagram with optional manually added points
#'
#' This function plots a standard Moody diagram, and allows additional
#' points to be added by including arguments Re and f.
#'
#' @param Re (optional) numeric vector that contains the Reynolds numbers of points to be
#' manually added
#' @param f (optional) numeric vector (same length as Re) that contains the Darcy-Weisbach
#' friction factors corresponding to the points to be manually added
#'
#' @return a Moody diagram, with the optional added (Re, f) points
#'
#' @author Ed Maurer
#'
#' @examples
#'
#' # Draw canonical Moody diagram
#' moody()
#'
#' # Draw Moody diagram plotting two additional points
#' Re = c(10000, 100000)
#' f = c(0.04, 0.03)
#' moody( Re = Re, f = f )
#'
#'
#' @importFrom utils tail
#' @import ggplot2
#' @import reshape2
#' @import grid
#'
#' @export
moody <- function(Re = NULL, f = NULL) {

  # Check for packages needed to create plot
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("moody diagram plot requires ggplot2 to be installed.",
         call. = FALSE)
  }
  if(!requireNamespace("reshape2", quietly = TRUE)) {
    stop("moody diagram plot requires reshape2 to be installed.",
         call. = FALSE)
  }
  if(!requireNamespace("grid", quietly = TRUE)) {
    stop("moody diagram plot requires grid to be installed.",
         call. = FALSE)
  }

  # Create a data frame of manually added points
  if (length(Re) > 0) {
    if (length(Re) != length(f)) {
      stop("\nRe and f vectors must have the same length")
    }
    dfpts <- as.data.frame(cbind(Re, f))
  }
  # Function used for friction factor Use approximation of the
  # Swamee-Jain equation for turbulent conditions Swamee,P.K.,Jain, A.K.
  # (1976).Journal of the Hydraulics Division 102(5):657–664.
  jain <- function(ksoverD, Re) {
    f <- 0.25/((log10((ksoverD)/3.7 + 5.74/(Re^0.9)))^2)
    return(f)
  }
  # Set up ranges for plot axes, tick marks and labels Reynold's number
  # ranges (x-axis)
  xmin <- 600
  xmax <- 10^8
  # Friction factor ranges (primary y-axis)
  ymin <- 0.008
  ymax <- 0.1
  # friction factor lines to label
  ytickloc <- c(0.008, 0.009, 0.01, 0.015, 0.02, 0.025, 0.03, 0.04, 0.05,
                0.06, 0.07, 0.08, 0.08, 0.1)
  # relative roughness (lines to be drawn in turbulent region)
  kd <- c(1e-05, 5e-05, 1e-04, 2e-04, 4e-04, 6e-04, 8e-04, 0.001, 0.002,
          0.004, 0.006, 0.008, 0.01, 0.015, 0.02, 0.03, 0.04, 0.05)
  # Use three relationships for different portions of graph Laminar
  # Portion
  Relam <- seq(from = 600, to = 3000, by = 100)
  flam <- 64/Relam
  dflam <- as.data.frame(cbind(Relam, flam))
  # Turbulent Regions Re values from 3000 to 1E8
  Returb <- utils::tail(c(2:10 %o% 10^(3:7)), -1)
  fsmooth = 0.316 * Returb^(-0.25)  # Blausis formula for smooth turbulent pipe flow
  dfsmooth <- as.data.frame(cbind(Returb, fsmooth))
  # set up empty array for lines for various values of relative roughness
  f_rough <- array(numeric(), c(length(Returb), length(kd)))
  colnames(f_rough) <- kd
  # calculate f values for each Re value
  for (i in 1:length(kd)) {
    f_rough[, i] <- jain(kd[i], Returb)
  }
  # Reconfigure data for plotting rearrange data frame for plotting
  dfrough <- as.data.frame(f_rough, row.names = Returb, col.names = kd)
  dfrough$Re <- Returb
  dfrough2 <- reshape2::melt(dfrough, id.vars = "Re", value.name = "f")
  ksd_colname <- "ks_d"
  names(dfrough2)[names(dfrough2) == "variable"] <- ksd_colname
  # Set up plotting parameters and create diagram add grid lines at
  # specific intervals
  breaks <- 10^(-10:10)
  minor_breaks <- rep(1:9, 21) * (10^rep(-10:10, each = 9))
  yminor_breaks <- c(seq(from = 0.008, to = 0.05, by = 0.001), seq(from = 0.055,
                                                                   to = 0.1, by = 0.005))
  ybreaks <- ytickloc
  # plot lines for turbulent region
  p1 <- ggplot2::ggplot(dfrough2, ggplot2::aes_string(x = "Re", y = "f", group = ksd_colname)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_log10(limits = c(ymin, ymax), expand = c(0, 0),
                           breaks = ybreaks, minor_breaks = yminor_breaks) +
    ggplot2::scale_x_log10(limits = c(xmin, xmax), expand = c(0, 0),
                           breaks = breaks, minor_breaks = minor_breaks) +
    ggplot2::scale_colour_discrete(guide = "none") +
    ggplot2::geom_text(data = subset(dfrough2, Re == 1e+08),
                       ggplot2::aes_string(label = ksd_colname, x = "1e+08", y = "f"), hjust = -0.3, size=3) +
    ggplot2::annotate("text", x = 1e+08, y = 0.092, label = "ks/D", hjust = -0.3, size=3) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.margin = grid::unit(c(1,3, 1, 1), "lines"))
  # add the smooth, turbulent line
  p2 <- p1 + ggplot2::geom_line(ggplot2::aes(x = Returb, y = fsmooth), data = dfsmooth,
                                inherit.aes = FALSE, na.rm = TRUE)
  # add the laminar line
  p3 <- p2 + ggplot2::geom_line(ggplot2::aes(x = Relam, y = flam), data = dflam,
                                linetype = "dashed", inherit.aes = FALSE, na.rm = TRUE)
  # add points, if there are any
  if (length(Re) <= 0) {
    p4 <- p3
  } else {
    p4 <- p3 + ggplot2::geom_point(ggplot2::aes(x = Re, y = f), data = dfpts,
                                   inherit.aes = FALSE, shape = 21, fill = "blue", size = 2)
  }
  # Code to turn off clipping -- allows labels of relative roughness
  # outside plot region
  gt1 <- ggplot2::ggplotGrob(p4)
  gt1$layout$clip[gt1$layout$name == "panel"] <- "off"
  grid::grid.draw(gt1)
}
