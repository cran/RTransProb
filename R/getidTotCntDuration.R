# Get totals per ID, duration method

getidTotCntDuration <- function(numDate,numericalRating,StartPos,sDate,eDate,RatingsCat){

  idTotCnt_totalsVec = list()
  idTotCnt_totalsMat = list()
  idTotCnt_method = list()
  idTotCnt           = list()
  rating             = list()

  nIDs <- nrow(StartPos);
  algo <- 'duration';


  nRtgsCatNaN <- RatingsCat+1;

  for (k in 1:(nIDs-1)){

    totalsVec <- matrix(0,1,nRtgsCatNaN);
    totalsMat <- matrix(0,nRtgsCatNaN,nRtgsCatNaN);

    date_rating <- getCategoriesLocal(k,numDate,numericalRating,StartPos, sDate,eDate)
    rating      <- date_rating$rating
    mdate       <- date_rating$mdate

    if(is.nan(rating)){
      rating[is.na(rating)] = nRtgsCatNaN;
    }


    # Info at start time
    prevDate = mdate[1];
    prevRating <- as.numeric(rating[1]);

    for (t in 2:length(mdate)){

      # Get info on latest transition
      currentDate = mdate[t];
      currentRating <- rating[t];

      # Update total time spent in previous rating
      elapsedTime = yearFractionLocal(prevDate,currentDate);
      totalsVec[prevRating] = totalsVec[prevRating] + elapsedTime;

      # Update counts according to latest transition
      if (prevRating!=currentRating){
        totalsMat[prevRating,currentRating] = totalsMat[prevRating,currentRating] + 1;
      }

      # Shift current info
      prevDate = currentDate;
      prevRating = currentRating;
    }

    idTotCnt_totalsVec[[k]] <- as.matrix(totalsVec[1:RatingsCat])
    idTotCnt_totalsMat[[k]] <-as.matrix(totalsMat[1:RatingsCat,1:RatingsCat])
    idTotCnt_method[k] <- algo;

  }


  idTotCnt  <- list(totalsVec=idTotCnt_totalsVec,
                    totalsMat=idTotCnt_totalsMat,
                    method=idTotCnt_method)

  return(idTotCnt)

}


