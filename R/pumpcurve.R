#' Fits a polynomial curve to three or more points from a pump characteristic
#' curve to be used in solving for an operating point of the pump in a 
#' piping system.
#' 
#' Fits a polynomial curve to three or more points from a pump characteristic
#' curve. This allows solving for an operating point of the pump in a 
#' piping system. A portion of this is based on
#' https://github.com/PhDMeiwp/basicTrendline/blob/master/R/trendline.R
#' 
#' @param Q  Numeric vector of flow rates for selected points on the pump curve [\eqn{m^3 s^{-1}}{m^3/s} or \eqn{ft^3 s^{-1}}{ft^3/s}]
#' @param h  Numeric vector of heads for selected points on the pump curve [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param eq Character vector identifying the for of equation to fit (see details)
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units.
#'   
#' @return Returns an object of class \code{pumpcurve} consisting of a list including:
#' \itemize{
#'   \item curve - a function defining the curve that is fit to the data
#'   \item eqn - a character vector of the equation for the curve
#'   \item r2 - the coefficient of determination for the curve fit, \eqn{R^2}{R2}
#'   \item p - a plot object of the fit curve
#'   \item units - the units system passed to the function
#' }
#'  
#' @author Ed Maurer
#'
#' @details The form of the equation fit to the input points may be one of the 
#' following, as determined by the \code{eq} input parameter.
#' \tabular{ll}{
#'   \strong{eq} \tab \strong{equation form} \cr
#'   poly1 \tab \eqn{h = a + {b}{Q} + {c}{Q}^2} \cr
#'   poly2 \tab \eqn{h = a + {c}{Q}^2} \cr
#'   poly3 \tab \eqn{h = h_{shutoff} + {c}{Q}^2}
#'   }
#' where \eqn{h_{shutoff}} is the head on the pump curve associated with 
#' \eqn{Q=0}. The shutoff head at \eqn{Q=0} should be included in the input
#' vectors if the \code{poly3} equation form is to be used.
#'  
#' @examples
#'
#' #Input in Eng units - use \code{units} package for easy unit conversion
#' qgpm <- units::set_units(c(0, 5000, 7850), gallons/minute)
#' qcfs <- units::set_units(qgpm, ft^3/s)
#' hft <- c(81, 60, 20) #units are already in ft so setting units is optional
#' pumpcurve(Q = qcfs, h = hft, eq = "poly2", units = "Eng")
#'
#' @importFrom stats coef complete.cases lm
#'
#' @name pumpcurve
NULL

#' @export
#' @rdname pumpcurve
pumpcurve <- function (Q = NULL, h = NULL, eq = "poly1", units = c("SI", "Eng")) {
  
  units <- units
  if (length(units) != 1) stop("Incorrect unit system. Specify either SI or Eng.")
  if (units == "SI") {
    txtx <- expression("Flow, "~m^{3}/s)
    txty <- sprintf("Head, m")
  } else if (units == "Eng") {
    txtx <- expression("Flow, "~ft^{3}/s)
    txty <- sprintf("Head, ft")
  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Must be SI or Eng")
  }
  
  #check if any values have class 'units' and change to numeric if necessary
  for( i  in c("Q", "h") ) {
    v <- get(i)
    if(inherits(v, "units")) assign(i, units::drop_units(v))
  }
  
  nQ <- length(Q)
  nh <- length(h)
  if(nQ != nh){
    stop('The Q and h vectors must be the same length')
  }
  if(nQ <= 2){
    stop('Q and h vectors must have at least three points')
  }

  OK <- complete.cases(Q, h)
  if(length(OK) <= 2){
    stop('Q and h vectors must each have at least three valid numeric points')
  }

  Q <- Q[OK]
  h <- h[OK]
  z<-data.frame(Q,h)
  z <- z[order(z$Q),]
  row.names(z) <- NULL

  if (any(z < 0)) {
    stop("negative Q or h not permitted")
  }

  if ( eq == "poly1" ) {
    hqfit <- lm(h ~ poly(Q, 2, raw=TRUE), data = z)
    r2 <- summary(hqfit)$r.squared
    fitpCurve <- function(x) coef(hqfit)[1] +x*coef(hqfit)[2]+x^2*coef(hqfit)[3]
    a <- signif(coef(hqfit)[1],3)
    b <- signif(coef(hqfit)[2],3)
    c <- signif(coef(hqfit)[3],3)
    if( c > 0 ) {
      stop("Concavity of pump characteristic curve is reversed.")
    }
    eqn <- paste0( 'h == ',a,' + ',b,' - ',abs(c),'*Q^2')
  } else if ( eq == "poly2" ) {
    hqfit <- lm(h ~ I(Q^2), data = z) #remove linear term
    r2 <- summary(hqfit)$r.squared
    fitpCurve <- function(x) coef(hqfit)[1] +x^2*coef(hqfit)[2]
    a <- signif(coef(hqfit)[1],3)
    b <- signif(coef(hqfit)[2],3)
    if( b > 0 ) {
      stop("Concavity of pump characteristic curve is reversed.")
    }
    eqn <- paste0( 'h == ',a,' - ',abs(b),'*Q^2')
  } else if ( eq == "poly3" ) {
    h_shutoff <- z$h[1]
    if(min(z$Q) != 0) {
      message("Shutoff head not provided, setting it to head at min(Q)")
      h_shutoff <- z$h[which(z$Q == min(z$Q))]
      newrow <- data.frame(0, h_shutoff)
      names(newrow) <- names(z)
      z <- rbind(newrow, z)
    }
    hqfit <- lm(h ~ -1+I(Q^2), offset=rep(h_shutoff,length(Q)), data = z) #remove linear term and fix h_shutoff
    r2 <- summary(hqfit)$r.squared
    fitpCurve <- function(x) h_shutoff +x^2*coef(hqfit)[1]
    b <- signif(coef(hqfit)[1],3)
    if( b > 0 ) {
      stop("Concavity of pump characteristic curve is reversed.")
    }
    eqn <- paste0( 'h == ',h_shutoff,' - ',abs(b),'*Q^2')
  } else {
    stop("eqn parameter must be one of poly1, poly2 or poly3.")
  }
  r2t <- signif(r2,3)
  
  #create a plot to export
  p <- ggplot2::ggplot(data=z) +
    ggplot2::geom_point(ggplot2::aes(x=Q, y=h)) +
    ggplot2::geom_function(fun = fitpCurve, colour = "blue") +
    ggplot2::annotate("text", x=min(Q)*1.2,y=min(h)*1.2,
                      label = eqn, parse=T, hjust = 0, colour = "blue") +
    ggplot2::annotate("text", x=min(Q),y=min(h), 
                      label = paste("r^2: ", r2t,sep=""), parse=T, hjust = 0, colour = "blue") +
    ggplot2::labs(x = txtx, y = txty) +
    ggplot2::theme_bw()

  output <- list(curve = fitpCurve, eqn = eqn, r2 = r2t, p = p, units = units)
  return(structure(output, class = "pumpcurve"))
}
