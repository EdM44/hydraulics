#' Tabulates into a tibble the basic water properties: density, dynamic and kinematic 
#' viscosity, saturation vapor pressure, surface tension, and bulk modulus.
#'
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units. This is used for compatibility with iemisc package
#' @param ret_units If set to TRUE the value(s) returned are of class \code{units} with
#' units attached to the value. [Default is TRUE]
#' @author Ed Maurer
#'
#' @examples
#'
#' water_table(units = 'SI')
#'
#' @import tibble
#'
#' @name water_table
NULL
#' @export
#' @rdname water_table
water_table <- function(units = c("SI", "Eng"), ret_units = TRUE ) {
  units <- units
  if (units == "SI") {
    temps <- seq(from=0, to=100, by=5)
    temps <- units::set_units(temps,"C")
  } else if (units == "Eng") {
    temps <- seq(from=32, to=212, by=10)
    temps <- units::set_units(temps,"F")
  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Specify either SI or Eng.")
  }

  df <- tibble::tibble(
    Temperature = temps,
    Density = dens(T = temps, units = units, ret_units = ret_units),
    Spec_Weight = specwt(T = temps, units = units, ret_units = ret_units),
    dvisc(T = temps, units = units, ret_units = ret_units),
    kvisc(T = temps, units = units, ret_units = ret_units),
    Sat_VP = svp(T = temps, units = units, ret_units = ret_units),
    Surf_Tens = surf_tension(T = temps, units = units, ret_units = ret_units),
    Bulk_Mod = Ev(T = temps, units = units, ret_units = ret_units)
  )
  return(df)
}
