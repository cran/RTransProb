#' Vector of Transition Counts
#'
#' @description This function reshapes transition matrices of counts into a 'wide format', time series of transition counts.
#'
#' @usage VecOfTransData(lstCnt,ratingCat,startDate,endDate,snapshots)
#'
#' @param lstCnt quarterly transition count data.
#' @param ratingCat list containing the unique rating caetgories.
#' @param startDate             start date of the estimation time window, in string or numeric format. The default start date is the earliest date in 'data'.
#' @param endDate             end date of the estimation time window, in string or numeric format. The default end date is the latest date in 'data'. The end date cannot be a date before the start date.
#' @param snapshots         Integer indicating the number of credit-rating snapshots per year to be considered for the estimation.
#'                          Valid values are 1, 4, 12, 54, and 356.
#'                          For example, 1 = one snapshot per year
#'
#'
#' @return The output is a time series vector of transition counts .
#'
#' @export
#'
#' @author  Abdoulaye (Ab) N'Diaye
#'
VecOfTransData<-function(lstCnt,ratingCat, startDate,endDate,snapshots){

  df <- data.frame(industryName=character(),
                   Qtr_Year =character(),
                   endDate = as.Date(character()),
                   Rating_Trans = character(),
                   start_Rating = character(),
                   end_rating = character(),
                   mCount = integer(),
                   stringsAsFactors = FALSE)

  
  if (snapshots == 1){
    snaps = "years"
  } else if (snapshots == 4){
    snaps = "quarters"
  } else if (snapshots == 12){
    snaps = "months"
  }
  
  TotalDateRange <- seq.Date(as.Date(startDate), as.Date(endDate), snaps)

  #initialize counters
  nn <- 1
  k <- 1
  r <- 1

  TRows <- ratingCat
  TCols <- ratingCat

  #now create transitions table
  for (l in 1:(length(TotalDateRange)-1)){

    #istartDate = POSIXTomatlab(as.POSIXlt(as.Date(TotalDateRange[l],format = "%Y-%m-%d")))
    #iendDate = POSIXTomatlab(as.POSIXlt(as.Date(TotalDateRange[l+1],format = "%Y-%m-%d")))
    #DateRange        <- as.Date(matlabToPOSIX(cfdates(istartDate,iendDate,snapshots)))

    #for(i in 1:(length(DateRange)-1)){

      startDate  <- TotalDateRange[l]
      endDate    <- TotalDateRange[l+1]

      getRowCOunt <- nrow(lstCnt[[k]][[nn]])

      #Transpose the matrix before looping through it
      cntMatrix <- t(lstCnt[[k]][[nn]])

      c <- 1

      for (gcols in 1:getRowCOunt){
        for (grows in 1:getRowCOunt){
          df[r,1] <- "X"
          df[r,2] <- paste(as.character(chron::years(TotalDateRange[l])) ,quarters(TotalDateRange[l]),sep=" ")
          df[r,3] <- TotalDateRange[l+1]
          df[r,4] <- paste(TCols[gcols], "-", TRows[grows], sep="")
          df[r,5] <- TCols[gcols]
          df[r,6] <- TRows[grows]
          df[r,7] <- cntMatrix[c]

          c <- c+1
          r <- r +1
        }
      }


      nn <- nn+1

      if(nn>snapshots){
        nn <- 1
        k <- k+1
      }

    #}

  }

  transData <- df
  return(transData)
}

