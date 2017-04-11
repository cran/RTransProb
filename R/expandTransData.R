#' Reshape Data to 'Wide' Data Format
#'
#' @description This function reshapes time series vector of transition counts into a 'Wide' Data Format. Each non-Zero weighted row is
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
expandTransData <- function(transData, wgtname) {


  transData <- transData[ which(transData[[wgtname]] >0), ]

  #replicate the structure of the input data
  df <- transData[0, ]
  df$Id      <-  numeric(0)   #create a new column to hold the counts

  #get the names and name count of the table df
  transData_nm <- names(df)
  transData_nm_cnt <- length(transData_nm)

  r <- 1


  for (i in 1:nrow(transData)){

    wght <- transData[[wgtname]][i]

    for (c in 1:wght){
      for (columnCount in 1:transData_nm_cnt){
        if(columnCount <= (transData_nm_cnt-2)){

          df[r,columnCount] <- transData[[transData_nm[columnCount]]][i]

        } else if(columnCount == (transData_nm_cnt-1)) {

          df[r,columnCount] <- wght

        } else {

          df[r,columnCount] <- c

        }
      }
      r <- r+1
    }

  }

  return (df)
}
