#' Reshape Data to 'Wide' Data Format
#'
#' @description This function reshapes time series vector of transition counts with elements of both 'wide' and 'long' data Formats. Each non-Zero weighted row is
#'  expanded to show one row per record.
#'
#' @usage expandTransData(transData, wgtname)
#'
#' @param transData dataframe containing the time series vector of transition counts.
#' @param wgtname weight.
#'
#'
#' @return The output a dataframe of transition data in 'wide' format.
#'
#' @export
#'
#' @author  Abdoulaye (Ab) N'Diaye
#'
expandTransData <- function(transData, wgtname = "mCount") {
  transData <- transData[which(transData[[wgtname]] > 0),]
  
  #replicate the structure of the input data
  df <- transData[0,]
  df$Id      <-
    numeric(0)   #create a new column to hold the counts
  
  #get the names and name count of the table df
  transData_nm <- names(df)
  transData_nm_cnt <- length(transData_nm)
  
  industryName = transData[["industryName"]]
  Qtr_Year = transData[["Qtr_Year"]]
  endDate = transData[["endDate"]]
  Rating_Trans = transData[["Rating_Trans"]]
  start_Rating = transData[["start_Rating"]]
  end_rating = transData[["end_rating"]]
  mCount = transData[["mCount"]]
  Id = transData[["Id"]]
  
  totalCountExpanded <- sum(mCount)
  df = getExpandTransData(
    industryName,
    Qtr_Year,
    endDate,
    Rating_Trans,
    start_Rating,
    end_rating,
    mCount,
    wgtname,
    transData_nm_cnt,
    totalCountExpanded
  )
  
  return (df)
}
