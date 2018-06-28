
#' Create Date Squence
#'
#' @description This function takes two dates and returns a sequence of dates using the interval.
#'
#' @usage cfdates(sdates,edates,snapshots)
#'
#' @param sdates            start date in Matlab datenum format.
#' @param edates            end date in Matlab datenum format.
#' @param snapshots         Integer indicating the number of credit-rating snapshots per year to be considered for the estimation.
#'                          Valid values are 1, 4, 12, 54, and 356.
#'                          For example, 1 = one snapshot per year
#'
#' @export
#'
#' @return a list containing a date sequence.
#'
#' @author Abdoulaye (Ab) N'Diaye
#'
#' @examples
#' # Convert a date string to Matlab datenum format.
#' sdates <-  POSIXTomatlab(as.POSIXlt(as.Date("2000-01-01")))
#' edates <-  POSIXTomatlab(as.POSIXlt(as.Date("2002-01-01")))
#'
#' cfdates(sdates, edates, 1)
#'
#'
cfdates <- function(sdates, edates, snapshots) {
  sdates <- as.Date(matlabToPOSIX(sdates))
  edates <- as.Date(matlabToPOSIX(edates))
  
  sdates_yrs <- as.numeric(format(sdates, '%Y'))
  sdates_mns <- as.numeric(format(sdates, '%m'))
  sdates_dys <- as.numeric(format(sdates, '%d'))
  
  edates_yrs <- as.numeric(format(edates, '%Y'))
  edates_mns <- as.numeric(format(edates, '%m'))
  edates_dys <- as.numeric(format(edates, '%d'))
  
  
  
  if (snapshots == 1) {
    snapshotDates <-
      seq(
        ISOdate(sdates_yrs, sdates_mns, sdates_dys),
        ISOdate(edates_yrs, edates_mns, edates_dys),
        by = "years"
      )
  } else if (snapshots == 4) {
    snapshotDates <-
      seq(
        ISOdate(sdates_yrs, sdates_mns, sdates_dys),
        ISOdate(edates_yrs, edates_mns, edates_dys),
        by = "quarter"
      )
    if (length(snapshotDates) < 2) {
      snapshotDates[2] <- ISOdate(edates_yrs, edates_mns, edates_dys)
    }
  } else if (snapshots == 12) {
    snapshotDates <-
      as.Date(chron::seq.dates(
        format(sdates, "%m/%d/%Y"),
        format(edates, "%m/%d/%Y"),
        by = "months"
      ),
      "%Y-%m-%d")
  } else if (snapshots == 54) {
    snapshotDates <-
      seq(
        ISOdate(sdates_yrs, sdates_mns, sdates_dys),
        ISOdate(edates_yrs, edates_mns, edates_dys),
        by = "week"
      )
  } else if (snapshots == 356) {
    snapshotDates <-
      seq(
        ISOdate(sdates_yrs, sdates_mns, sdates_dys),
        ISOdate(edates_yrs, edates_mns, edates_dys),
        by = "day"
      )
  }
  
  
  snapshotDates <- as.Date(snapshotDates)
  snapshotDates <-
    POSIXTomatlab(as.POSIXlt(as.Date(snapshotDates, format = "%m/%d/%Y")))
  return (snapshotDates)
  
}
