#' Uses input pump and system curves to find the operating point
#' for a pump and create a plot.
#' 
#' @param pcurve  A pumpcurve object
#' @param scurve  A systemcurve object
#' 
#' @return Returns a list including:
#' \itemize{
#'   \item Qop - flow at the operating point [\eqn{m^3 s^{-1}}{m^3/s} or \eqn{ft^3 s^{-1}}{ft^3/s}]
#'   \item hop - head at the operating point [\eqn{m}{m} or \eqn{ft}{ft}]
#'   \item p - a plot object of the curves
#' }
#' 
#' @author Ed Maurer
#' 
#' @seealso \code{\link{pumpcurve}} and \code{\link{systemcurve}} for input object preparation
#' 
#' @importFrom stats uniroot optimise
#'
#' @name operpoint
NULL
#' @export
#' @rdname operpoint
operpoint <- function (pcurve = NULL, scurve = NULL) {

  if(!inherits(pcurve, "pumpcurve")) {
    stop("Pump curve not a valid pumpcurve object")
  }
  if(!inherits(scurve, "systemcurve")) {
    stop("System curve not a valid systemcurve object")
  }
  if(scurve$curve(0) > pcurve$curve(0)) {
    stop("System static head exceeds pump shutoff head")
  }
  if(scurve$units != pcurve$units) {
    stop("Unit systems for curves do not match")
  }
  
  units <- scurve$units
  if (length(units) != 1) stop("Incorrect unit system. Specify either SI or Eng.")
  
  if (units == "SI") {
    txtx <- expression("Flow, "~m^{3}/s)
    txty <- sprintf("Head, m")
  } else if (units == "Eng") {
    txtx <- expression("Flow, "~ft^{3}/s)
    txty <- sprintf("Head, ft")
  }
    
  #find intersection in interval of flows from 0 to somewhere beyond
  #point where pump curve reaches zero
  maxQ <- uniroot(pcurve$curve, interval = c(0, 100000))$root
  maxh <- max(scurve$curve(maxQ), pcurve$curve(maxQ))
  Qop = optimise(f=function(x) abs(scurve$curve(x)-pcurve$curve(x)), c(0,maxQ))$minimum
  hop = as.numeric(scurve$curve(Qop))
  
  #oppt <- paste0( 'Op pt:(',Qop,',',hop,')')
  oppt1 <- paste0( "Q:",signif(Qop,3) )
  oppt2 <- paste0( "h:",signif(hop,3) )
  
  #create a plot to export
  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(txtx, limits = c(0, maxQ), expand = c(0,0)) +
    ggplot2::scale_y_continuous(txty, limits = c(0, maxh), expand = c(0,0)) +
    ggplot2::geom_point(ggplot2::aes(x=Qop, y=hop)) +
    ggplot2::geom_function(fun = pcurve$curve, colour = "blue") +
    ggplot2::annotate("text", x=maxQ*0.05,y=maxh*0.05,
                      label = pcurve$eqn, parse=T, hjust = 0, colour = "blue") +
    ggplot2::geom_function(fun = scurve$curve, colour = "black") +
    ggplot2::annotate("text", x=maxQ*0.05,y=maxh*0.10,
                      label = scurve$eqn, parse=T, hjust = 0, colour = "black") +
    ggplot2::annotate("text", x=maxQ*0.95,y=hop+maxh*0.03, 
                      label = oppt1, parse=T, hjust = 1, colour = "black") +
    ggplot2::annotate("text", x=maxQ*0.95,y=hop-maxh*0.03, 
                      label = oppt2, parse=T, hjust = 1, colour = "black") +
    ggplot2::theme_bw()
  
  output <- list(Qop = Qop, hop = hop, p = p)
  return(output)
}
