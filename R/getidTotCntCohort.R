
# Get totals per ID, cohort method

getidTotCntCohort <- function(numDate,numericalRating,StartPos,sDate,eDate,RatingsCat,snapshots){

  idTotCnt_totalsVec = list()
  idTotCnt_totalsMat = list()
  idTotCnt_method = list()
  idTotCnt           = list()

  nIDs <- nrow(StartPos);
  algo <- 'cohort';

  #Introduce an artificial 'NaN' state - time window can be larger than some
  #companies' migration history
  nRtgsCatNaN <- RatingsCat+1;


  if( snapshots==12){
    eDate <- POSIXTomatlab(as.POSIXlt(as.Date(matlabToPOSIX(eDate))+1))
  }

  # sDate<-matlabToPOSIX(sDate)
  #
  # if (snapshots==1){
  #     dates <- as.Date(strDates, "%m/%d/%Y")
  #     ymd("2010/03/17") - years(2)
  # > ymd("2010/03/17") - months(2)
  # > ymd("2010/03/17") - quarters(2)
  # > ymd("2010/03/17") - quarter(2)
  # > ymd("2010/03/17") - weeks(2)
  # > ymd("2010/03/17") - days(2)



  snapshotDates <- cfdates(sDate-1,eDate,snapshots)



  for (k in 1:(nIDs-1)){

    totalsVec <- matrix(0,1,nRtgsCatNaN);
    totalsMat <- matrix(0,nRtgsCatNaN,nRtgsCatNaN);


    rating = getRatingsSnapshotsLocal(k,numDate,numericalRating,StartPos,snapshotDates);

    if(is.nan(rating)){
      rating <- replace(rating, is.nan(rating),  nRtgsCatNaN)
    }

    #Info at start time
    prevRating <- as.numeric(rating[1]);

    for (t in 2:length(snapshotDates)){
      # Update total count in previous rating
      totalsVec[prevRating] <- totalsVec[prevRating] + 1;

      # Get info on latest transition and update transition counts
      currentRating <- rating[t];
      totalsMat[prevRating,currentRating] <- totalsMat[prevRating,currentRating] + 1;

      # Shift current info
      prevRating <- currentRating

    }

    ##Uncomment to use regular matrix
    idTotCnt_totalsVec[[k]] <- as.matrix(totalsVec[1:RatingsCat])
    idTotCnt_totalsMat[[k]] <-as.matrix(totalsMat[1:RatingsCat,1:RatingsCat])
    idTotCnt_method[k] <- algo;

  }

  idTotCnt  <- list(totalsVec=idTotCnt_totalsVec,
                    totalsMat=idTotCnt_totalsMat,
                    method=idTotCnt_method)

  return(idTotCnt)
}

