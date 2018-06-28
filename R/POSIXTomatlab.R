#' Convert between MATLAB datenum and R POSIXt
#'
#' @description This function is used to convert between MATLAB datenum values and R POSIXt time values.
#'
#' @usage POSIXTomatlab(gTime)
#'
#' @param gTime             a POSIXct or POSIXlt date value
#'
#' @export
#'
#' @return MATLAB datenum value.
#'
#' @author Abdoulaye (Ab) N'Diaye
#'
#' @examples
#'
#' POSIXTomatlab(as.POSIXlt(as.Date("2010-01-01")))
#'
POSIXTomatlab = function(gTime) {
  if (class(gTime)[1] == "POSIXlt") {
    days = as.numeric(as.Date(gTime))
    frac.day = (((gTime$hour) * 3600) + ((gTime$min) * 60) + gTime$sec) /
      86400
    datenum = 719529 + days + frac.day
    datenum = 719529 + days + frac.day
    
  } else if (class(gTime)[1] == "POSIXct") {
    gTime = as.POSIXlt(gTime)
    days = as.numeric(as.Date(gTime))
    frac.day = (((gTime$hour) * 3600) + ((gTime$min) * 60) + gTime$sec) /
      86400
    datenum = 719529 + days + frac.day
  } else {
    stop("Input cannot be coerced to POSIXlt or numeric value")
  }
  return(datenum)
}

