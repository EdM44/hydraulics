#' Creates a system curve for a piping system using the static head and 
#' a coefficient.
#' 
#' @param hs    Numeric value of the static head [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param K  Numeric value of the coefficient in the equation \eqn{h = hs + {K}{Q}^2}
#' where Q has units of \eqn{m^3 s^{-1}}{m^3/s} or \eqn{ft^3 s^{-1}}{ft^3/s}
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units.
#'   
#' @return Returns an object of class \code{systemcurve} consisting of a list including:
#' \itemize{
#'   \item curve - a function defining the system curve
#'   \item eqn - a character vector of the equation for the curve
#'   \item units - the units system passed to the function
#' }
#'  
#' @author Ed Maurer
#'
#' @examples
#'
#' #Input in Eng units. Coefficient can be calculated manually or using 
#' #other package functions for friction loss in a pipe system using \eqn{Q=1}
#' ans <- darcyweisbach(Q = 1,D = 20/12, L = 3884, ks = 0.0005, nu = 1.23e-5, units = "Eng")
#' systemcurve(hs = 30, K = ans$hf, units = "Eng")
#'
#' @name systemcurve
NULL

#' @export
#' @rdname systemcurve
systemcurve <- function (hs = NULL, K = NULL, units = c("SI", "Eng")) {
 
  units <- units
  if (units != "SI" && units != "Eng") {
    stop("Incorrect unit system. Must be SI or Eng")
  }
  
  #check if any values have class 'units' and change to numeric if necessary
  for( i  in c("hs", "K") ) {
    v <- get(i)
    if(inherits(v, "units")) assign(i, units::drop_units(v))
  }
  
  if(length(hs) != 1 || length(K) != 1){
    stop('Static head and coefficient must be single numeric values')
  }
  
  if(hs < 0 || K < 0){
    stop('Static head and coef must be non-negative')
  }
  
  eqn <- paste0( 'h == ',signif(hs,3),' + ',signif(K,3),'*Q^2')
  systemCurve <- function(x) hs +x^2*K
  
  output <- list(curve = systemCurve, eqn = eqn, units = units)
  return(structure(output, class = "systemcurve"))
}
  