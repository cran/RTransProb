# Get ratings information, within a given time window, for a given index
getCategoriesLocal <- function(ind,numDate,numericalRating,StartPos,sDate,eDate){

  date_rating <- list()

  ind1 <- as.numeric(StartPos[ind,1]);
  ind2 <- as.numeric(StartPos[ind+1,1]-1);

  mdate = matrix(0,2,1);
  rating = matrix(0,2,1);



  # Insert time-window limit dates and sort by date
  dTemp = c(sDate,numDate[c(ind1:ind2)],eDate);
  rTemp = c(NaN,numericalRating[c(ind1:ind2)],NaN);
  iTemp = sort(dTemp, index.return=TRUE)$ix;
  dTemp = sort(dTemp)[sort(iTemp)];
  rTemp = rTemp[iTemp];



  # Take care of rating information on the boundaries
  i1<-which(iTemp==1,arr.ind = T)
  i1 = i1[1];

  n = length(dTemp);

  i2<-which(iTemp==n,arr.ind = T)
  i2 = i2[1];

  if ((dTemp[i1]==dTemp[i1+1]) && (i1+1 < i2)){
    i1 = i1 + 1
  } else if(i1>1){
    rTemp[i1] = rTemp[i1-1];
  }

  if (dTemp[i2]==dTemp[i2-1]){
    i2 = i2 - 1;
  } else {
    rTemp[i2] = rTemp[i2-1];
  }

  mdate = as.matrix(dTemp[i1:i2]);
  rating = as.matrix(rTemp[i1:i2]);

  date_rating <- list(mdate=mdate, rating=rating)

  return(date_rating)
}

