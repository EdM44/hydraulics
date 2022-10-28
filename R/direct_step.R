#' Uses the direct step method to find the distance between two known depths
#' in a trapezoidal channel
#'
#' This function applies the direct step method for a gradually-varying water 
#' surface profile for flow in an open channel with a trapezoidal shape.
#'
#' @param So numeric channel bed slope [unitless]
#' @param n numeric vector that contains the Manning roughness coefficient
#' @param Q numeric vector that contains the flow rate [\eqn{m^3 s^{-1}}{m^3/s} or \eqn{ft^3 s^{-1}}{ft^3/s}]
#' @param y1 numeric vector that contains the initial water depth [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param y2 numeric vector that contains the final water depth [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param b numeric vector that contains the channel bottom width [\eqn{m}{m} or \eqn{ft}{ft}]
#' @param m numeric vector that contains the side slope of the channel (m:1 H:V) [unitless]
#' @param nsteps integer of the number of calculation steps between y1 and y2 [unitless]
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units. This is used for compatibility with iemisc package.
#' @param ret_units If set to TRUE the value(s) returned are of class \code{units} with
#'   units attached to the value. [Default is FALSE]
#'
#' @return Returns a data frame (tibble) with the columns:
#' \itemize{
#'   \item x - cumulative distance from position of y1
#'   \item z - elevation of the channel bed at location x
#'   \item y - depth of the water at location x
#'   \item A - cross-sectional area at location x
#'   \item Sf - slope of the energy grade line at location x
#'   \item E - specific energy at location x
#'   \item Fr - Froude number at location x
#' }
#'
#' @author Ed Maurer
#'
#' @details The direct step method applies the energy equation to gradually-varied 
#' open channel flow conditions, assuming each increment is approximately uniform.
#' This function works with a trapezoidal channel shape. The water depths at two 
#' locations are input with channel geometry and flow rate, and the distance between
#' the two locations, \eqn{{\Delta}X}, is calculated: 
#' \deqn{{\Delta}X = \frac{E_1 - E_2}{S_f-S_o}}
#' where \eqn{E_1}{E1} and  \eqn{E_2}{E2} are the secific energy values at the locations 
#' of \eqn{y_1}{y1} and  \eqn{y_2}{y2},  \eqn{S_f}{Sf} is the slope of the energy grade line, 
#' and \eqn{S_o}{So} is the slope of the channel bed.
#'
#' @examples
#'
#' #Solving for profile between depths 3.1 ft and 3.4 ft in a rectangular channel
#' #Flow of 140 ft^3/s, bottom width = 6 ft:
#' direct_step(So=0.0015, n=0.013, Q=140, y1=3.1, y2=3.4, b=6, m=0, nsteps=2, units="Eng")
#'
#' @importFrom purrr map2_dfc
#'
#' @name direct_step
NULL

slope_f <- function(V,n,R,C) {
  return(V^2*n^2/(C^2*R^(4./3.)))
}

# This useful function taken from mkoohafkan/rivr package
get_profile = function(So, yn, yc, y0){
  if(So < 0) # adverse slope
    if (y0 > yc)
      return("A2")
  else
    return("A3")
  else if (So == 0) # horizontal slope
    if(y0 > yc)
      return("H2")
  else
    return("H3")
  else if (yn > yc) # Mild slope
    if(y0 > yn)
      return("M1")
  else if (y0 > yc)
    return("M2")
  else
    return("M3")
  else if (yc > yn) # steep slope
    if (y0 > yc)
      return("S1")
  else if (yn > y0)
    return("S3")
  else
    return("S2")    
  else # critical profile
    if (y0 > yc)
      return("C1")
  else
    return("C3")    
}

#attaches units to output if specified
units::units_options(allow_mixed = TRUE)
return_fcnds <- function(x = NULL, units = NULL, ret_units = FALSE) {
  if (units == "SI") {
    out_units <- c("m","m","m","m^2",1,"m",1)
  } else {
    out_units <- c("ft","ft","ft","ft^2",1,"ft",1)
  }
  df <- tibble::as_tibble(x)
  if( ret_units ) {
    df<- x %>% purrr::map2_dfc(out_units, ~units::set_units(.x, .y, mode = "standard"))
  }
  return(df)
}

#' @export
#' @rdname direct_step
direct_step <- function (So = NULL, n = NULL, Q = NULL, y1 = NULL, y2 = NULL, b = NULL, 
                         m = NULL, nsteps = 1, units = c("SI", "Eng"), ret_units = FALSE ) {

  units <- units

  #check if any values have class 'units' and change to numeric if necessary
  for( i  in c("So", "n", "Q", "y1", "y2", "b", "m") ) {
    v <- get(i)
    if(inherits(v, "units")) assign(i, units::drop_units(v))
  }
  
  #initial check for missing variables and out of bounds
  if (length(c("So", "n", "Q", "y1", "y2", "b", "m")) != 7) {
    stop("At least one of So, n, Q, y1, y2, b, m is missing")
  }
  if (any(c(Q, y1, y2) <= 0)) {
    stop("Either Q, y1, or y2 is <= 0. These variables must be positive")
  }
  if (any(c(b, m) < 0)) {
    stop("m (side slope) or b (bottom width) is negative. Both must be >=0")
  }
  if ( !(missing(b)) & !(missing(m)) ) {
    if ( (m == 0) & (b == 0) ) {
      stop("m (side slope) and b (bottom width) are zero. Channel has no area")
    }
  }
  if(nsteps < 1) {
    if (!(all.equal(nsteps, as.integer(nsteps))))  {
      stop("nsteps must be a positive integer")
    }
  }

  if (units == "SI") {
    g <- 9.80665     # m / s^2
    C <- 1.0
  } else if (units == "Eng") {
    g <- 32.2        #ft / s^2
    C <- 1.4859
  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Must be SI or Eng")
  }
  
  #find normal and critical depths
  ans <- manningt(Q = Q, n = n, m = m, Sf = So, b = b, units = units)
  yn <- ans$y
  yc <- ans$yc
  proftype <- get_profile(So, yn, yc, y1)
  txt <- sprintf("y1=%.3f, y2=%.3f, yn=%.3f, yc=%3f\nProfile type  = %s\n",
                 y1, y2, yn, yc,proftype)
  message(txt)
  
  # Check that depths do not span critical depth
  if(y1 == yc || y2 == yc) {
    stop("One of input depths is critical depth; not gradually-varied flow conditions.")
  } else if (y1 > yc && y2 < yc) {
    stop("y1 is subcritical and y2 is supercritical; transition through critical not allowed.")
  } else if (y1 < yc && y2 > yc) {
    stop("y1 is supercritical and y2 is subcritical; hydraulic jump occurs, not gradually varied flow.")
  }
  
  depth_incr <- (y2 - y1) / nsteps
  depths <- seq(from=y1, to=y2, by=depth_incr)

  # data for starting cross-section
  y=depths[1]
  A1 <- y * (b + m * y)
  P1 <- b + 2 * y * sqrt(1 + m ^ 2)
  B1 <- b + 2 * m * y
  R1 <- A1 / P1
  D1 <- A1 / B1
  V1 <- Q / A1
  E1 = y + V1^2/(2*g)
  Fr1 <- V1 / (sqrt(g * D1))
  Sf1 <- slope_f(V1,n,R1,C)
  
  # initialize output data frame
  z <- 0
  L <- 0
  df.out <- data.frame(x = L, z = z, y = y, A = A1, Sf = Sf1, E = E1, Fr = Fr1)
  for ( i in 1:nsteps ) {
    #find hydraulic geometry for trapezoid, E and Sf at first depth
    y=depths[i]
    A1 <- y * (b + m * y)
    P1 <- b + 2 * y * sqrt(1 + m ^ 2)
    B1 <- b + 2 * m * y
    R1 <- A1 / P1
    D1 <- A1 / B1
    V1 <- Q / A1
    E1 = y + V1^2/(2*g)
    Fr1 <- V1 / (sqrt(g * D1))
    Sf1 <- slope_f(V1,n,R1,C)
    
    #find hydraulic geometry, E and Sf at second depth
    y=depths[i+1]
    A2 <- y * (b + m * y)
    P2 <- b + 2 * y * sqrt(1 + m ^ 2)
    B2 <- b + 2 * m * y
    R2 <- A2 / P2
    D2 <- A2 / B2
    V2 <- Q / A2
    E2 = y + V2^2/(2*g)
    Fr2 <- V2 / (sqrt(g * D2))
    Sf2 <- slope_f(V2,n,R2,C)

    Sf_avg <- (Sf1 + Sf2) / 2.0
    dX <- (E1 - E2) / (Sf_avg - So)
    L <- L + dX
    z <- z - (So * dX) # positive So slopes downstream, dX positive downstream
    outrow <- data.frame(x = L, z = z, y = y, A = A2, Sf = Sf2, E = E2, Fr = Fr2)
    df.out <- rbind(df.out,outrow)
  }
  return(return_fcnds(x = df.out, units = units, ret_units = ret_units))
}