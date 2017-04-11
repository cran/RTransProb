# Aggregate totals for the whole sample
getSampleTotals <- function(totals){

  sampleTotals <- list()

  totalsVec = matrix(0,nrow(totals$totalsVec[[1]]),ncol(totals$totalsVec[[1]]))
  totalsMat = matrix(0,nrow(totals$totalsMat[[1]]),ncol(totals$totalsMat[[1]]))
  method = totals$method[[1]]

  for (k in 1:length(totals$totalsVec)){
    if(anyNA(totals$totalsVec[[k]])){
      totals$totalsVec[[k]][is.na(totals$totalsVec[[k]])] <- 0
    }

    totalsVec = totalsVec + totals$totalsVec[[k]]
    totalsMat = totalsMat + totals$totalsMat[[k]]
  }

  sampleTotals <- list(totalsVec=totalsVec,
                       totalsMat=totalsMat,
                       method=method);
  return(sampleTotals)
}

