#' Convert a numeric  MATLAB datenum to R POSIXt time values
#'
#' @description This function is used to convert a numeric  MATLAB datenum
#'
#' @usage matlabToPOSIX(gTime, timez)
#'
#' @param gTime             a MATLAB datenum value
#' @param timez         time zone, default is "UTC"
#'
#' @export
#'
#' @return R POSIXt time values.
#'
#' @author Abdoulaye (Ab) N'Diaye
#'
#' @examples
#'
#' matlabToPOSIX(734139)
#'
matlabToPOSIX = function(gTime, timez = "UTC") {
  d = gTime - 719529
  s = d * 86400
  
  R_Date <- as.POSIXct(strftime(
    as.POSIXct(s, origin = '1970-1-1',
               tz = 'UTC'),
    format = '%Y-%m-%d %H:%M',
    tz = 'UTC',
    usetz = FALSE
  ),
  tz = timez)
  return(R_Date)
}

