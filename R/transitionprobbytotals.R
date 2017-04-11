
transitionprobbytotals <- function(idTotCnt,snapshots,interval,method){


  totals = idTotCnt
  snapshots = snapshots
  interval = interval

  if (length(totals)>1){
    sampleTotals = getSampleTotals(totals);
  } else {
    sampleTotals = totals;
  }

  transMat1= getTransitionProbability(sampleTotals,snapshots,interval);
  transMat = transMat1$transMat
  genMat = transMat1$genMat

  if(method == "cohort"){
    probbytotals <- list(sampleTotals=sampleTotals,
                         transMat=transMat)
  } else if (method == "duration"){
    probbytotals <- list(sampleTotals=sampleTotals,
                         transMat=transMat,
                         genMat = genMat)
  }

  return(probbytotals)

}

