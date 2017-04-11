
getRatingsSnapshotsLocal <- function(ind,numDate,numericalRating,StartPos,snapshotDates){

  ind1 <- as.numeric(StartPos[ind,1]);
  ind2 <- as.numeric(StartPos[ind+1,1]-1);
  j <- 1

  rating = matrix(0,length(snapshotDates),1);

  # # Expand ratings history from 0 to inf and sort by date
  dTemp = c(0,numDate[c(ind1:ind2)],Inf);
  rTemp = c(NaN,numericalRating[c(ind1:ind2)]);
  iTemp = sort(dTemp, index.return=TRUE)$ix
  dTemp = sort(dTemp)[iTemp]
  rTemp = rTemp[iTemp[1:length(iTemp)-1]];

  # Fill in the ratings for the snapshot dates
  for (i in 1:(length(dTemp)-1)){

    rating[snapshotDates>=dTemp[i] & snapshotDates<dTemp[i+1]]<-rTemp[i]

  }

  return(rating)

}

