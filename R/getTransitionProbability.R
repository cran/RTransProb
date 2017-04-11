# Get transition probabilities from sample totals

getTransitionProbability <- function(sampleTotals,snapshots,interval){
  transProbs <- list()

  dim_RatingsCat = dim(sampleTotals$totalsMat);
  RatingsCat1 = dim_RatingsCat[1]
  RatingsCat2 = dim_RatingsCat[2]
  RatingsCat  = RatingsCat2

  #diagInd = eye(RatingsCat1,RatingsCat2);
  diagInd = diag(1,RatingsCat1,RatingsCat2);

  totalsVec = sampleTotals$totalsVec;
  totalsMat = sampleTotals$totalsMat;
  method = sampleTotals$method;

  # For duration, 'transMat' temporarily stores the generator matrix
  if (method =='duration'){
    totalsMat[diagInd] = 0;
    totalsMat[ row(totalsMat) == col(totalsMat) ] <- -rowSums(totalsMat)
    transMat = matrix(0,RatingsCat,RatingsCat);
  } else if(method == 'cohort') {
    transMat = diag(1,RatingsCat);
  }

  nonZeroIndicies<-which(totalsVec!=0,arr.ind = T)
  nonEmpty = nonZeroIndicies[,1];
  transMat = totalsMat[nonEmpty,]/totalsVec[nonEmpty]



  # Get the transition matrix with the desired periodicity, in percent
  if (method =='duration'){
    genMat   = transMat
    transMat = expm::expm(transMat*interval);
  } else if(method =='cohort'){
    genMat = 0
    if (snapshots!=1 | interval>1){
      transMat = expm::`%^%`(transMat,(snapshots*interval))
    }

  }
  transMat = 100*transMat[1:RatingsCat1,1:RatingsCat2];

  transProbs <- list(transMat=transMat,
                     genMat=genMat);
  return(transProbs)
}

