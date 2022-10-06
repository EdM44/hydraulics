#' Tabulates into a tibble some properties of the standard atmosphere: temperature, 
#' density, and pressure.
#'
#' @param units character vector that contains the system of units [options are
#'   \code{SI} for International System of Units and \code{Eng} for English (US customary)
#'   units. 
#' @param ret_units If set to TRUE the value(s) returned are of class \code{units} with
#' units attached to the value. [Default is TRUE]
#' @author Ed Maurer
#'
#' @examples
#'
#' atmos_table(units = 'SI')
#'
#' @import tibble
#'
#' @name atmos_table
NULL
#' @export
#' @rdname atmos_table
atmos_table <- function(units = c("SI", "Eng"), ret_units = TRUE ) {
  units <- units
  if (units == "SI") {
    alts <- seq(from=0, to=15000, by=1000)
    alts <- units::set_units(alts,"m")
  } else if (units == "Eng") {
    alts <- seq(from=0, to=50000, by=5000)
    alts <- units::set_units(alts,"ft")
  } else if (all(c("SI", "Eng") %in% units == FALSE) == FALSE) {
    stop("Incorrect unit system. Specify either SI or Eng.")
  }

  df <- tibble::tibble(
    Altitude = alts,
    Temperature = atmtemp(alt = alts, units = units, ret_units = ret_units),
    Pressure = atmpres(alt = alts, units = units, ret_units = ret_units),
    Density = atmdens(alt = alts, units = units, ret_units = ret_units)
  )
  return(df)
}
