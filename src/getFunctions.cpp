// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include <RcppArmadillo.h>


using namespace Rcpp;
using namespace arma;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]



//--------------------------------------------------------------------------


// [[Rcpp::export]]
Rcpp::List getTransitionProbability(arma::mat sampleTotals_totalsMat, arma::vec sampleTotals_totalsVec, String sampleTotals_methods, signed int snapshots, signed int interval){
  
  
  arma::Col<double> Vec;
  arma::Mat<double> Mat;
  signed int Mg_row;
  signed int Mg_col;
  signed int RatingsCat1;
  signed int RatingsCat2;
  arma::mat diagInd;
  arma::vec totalsVec;
  arma::mat totalsMat;
  arma::mat  genMat;
  arma::mat  transMat;      
  arma::uvec nonZeroIndicies;
  arma::vec totalsVec1;
  arma::mat totalsVecRep;  
  arma::mat gRowSums;
  arma::vec gRowSumsL;
  arma::mat mmult;
  
  
  
  Mat = sampleTotals_totalsMat;
  Vec = sampleTotals_totalsVec;
  String method = sampleTotals_methods;
  
  Mg_row = Mat.n_rows;
  Mg_col = Mat.n_cols;
  
  RatingsCat1 = Mg_row;
  RatingsCat2 = Mg_col;
  
  diagInd = arma::eye<arma::mat>(RatingsCat1, RatingsCat2);
  
  totalsVec = arma::conv_to< arma::vec >::from(Vec);                   
  totalsMat = arma::conv_to< arma::mat >::from(Mat);                  
  
  
  
  //# For duration, 'transMat' temporarily stores the generator matrix
  if (method == "duration") {
    
    totalsMat.diag().zeros();
    gRowSums = arma::cumsum(totalsMat, 1);                                       
    gRowSumsL = gRowSums.col(Mg_row - 1);                              
    totalsMat.diag() = -gRowSumsL;                                     
    transMat = arma::zeros(RatingsCat1, RatingsCat2);
  }
  else if (method == "cohort") {
    transMat = arma::eye<arma::mat>(RatingsCat1, RatingsCat2);
  }
  
  
  
  if (accu(totalsVec)!=0){
    totalsVecRep = repmat(totalsVec, 1, totalsVec.n_rows);              
    transMat = totalsMat / totalsVecRep;                                
    transMat.replace(datum::nan, 0);                                    
    
    
    //# Get the transition matrix with the desired periodicity, in percent
    if (method == "duration") {
      genMat = transMat;                                                
      transMat = arma::expmat((transMat*interval));                     
      
    }
    else if (method == "cohort") {
      arma::mat genMat = arma::zeros(RatingsCat1, RatingsCat1);
      transMat = transMat;

    }
    
  }
  
  NumericVector transMat_= wrap(transMat);
  NumericMatrix genMat_= wrap(genMat);
  
  return Rcpp::List::create(Rcpp::Named("transMat") = transMat, Rcpp::Named("genMat") = genMat);
  
}






// [[Rcpp::export]]
Rcpp::List getTransitionProbabilityTNH(arma::mat sampleTotals_totalsMat,signed int snapshots, signed int interval){
  
  arma::Mat<double> Mat;
  signed int Mg_row;
  signed int Mg_col;
  signed int RatingsCat1;
  signed int RatingsCat2;
  arma::mat diagInd;
  arma::mat totalsMat;
  arma::mat  genMat;
  arma::mat  transMat;
  arma::mat gRowSums;
  arma::vec gRowSumsL;
  arma::mat mmult;
  
  
  
  Mat = sampleTotals_totalsMat;
  
  Mg_row = Mat.n_rows;
  Mg_col = Mat.n_cols;
  
  RatingsCat1 = Mg_row;
  RatingsCat2 = Mg_col;
  
  diagInd = arma::eye<arma::mat>(RatingsCat1, RatingsCat2);
  
  totalsMat = arma::conv_to< arma::mat >::from(Mat);                  
  
  
  
  //# For duration, 'transMat' temporarily stores the generator matrix
  
  transMat = arma::eye<arma::mat>(RatingsCat1, RatingsCat2);
  
  
  if (accu(totalsMat)==0){
    
    transMat = diagInd + totalsMat;     
    transMat.replace(datum::nan, 0);                                    
    
    
    //# Get the transition matrix with the desired periodicity, in percent
    
    arma::mat genMat = sampleTotals_totalsMat;
    if ((snapshots != 1) | (interval>1)) {
      mmult = transMat*transMat;         
      
      for (signed int mm = 0; mm < ((snapshots*interval)) - 2; ++mm) {
        mmult = transMat*mmult;          
      }
      transMat = mmult;
    }
    
  }
  
  
  NumericVector transMat_= wrap(transMat);
  NumericMatrix genMat_= wrap(genMat);
  
  return Rcpp::List::create(Rcpp::Named("transMat") = transMat, Rcpp::Named("genMat") = genMat);
  
}




// [[Rcpp::export]]
List getCategoriesLocal(signed int ind, arma::ivec numDate_, arma::vec numericalRating_, arma::ivec StartPos_, signed int sDate, signed int eDate){
  
  
  //Note: C++ indices start with 0 unlike R which starts with 1
  
  signed int ind1;
  signed int ind2;
  arma::ivec dTemp0;
  arma::ivec dTempf;
  arma::ivec dTempl;
  arma::ivec dTemp;
  arma::vec rTemp0;
  arma::vec rTempf;
  arma::vec rTempl;
  arma::vec rTemp;
  arma::uvec iTemp;
  signed int iTemp0;
  arma::uvec iTemp1;
  arma::uvec i1_;
  signed int i1;
  signed int n;
  arma::uvec i2_;
  signed int i2;
  arma::ivec mdate;
  arma::vec rating;
  NumericVector mdate_;
  NumericVector rating_;
  
  
  ind1 = StartPos_[ind];                                            
  ind2 = StartPos_[ind + 1] - 1;                                                                  
  
  dTemp0 = numDate_.subvec(ind1 - 1, ind2 - 1);       
  dTempf = arma::zeros<ivec>(1);                       
  dTempl = arma::zeros<ivec>(1);                        
  dTempf.fill(sDate);                                   
  dTempl.fill(eDate);                                   
  dTemp = arma::join_cols(dTempf, dTemp0);              
  dTemp = arma::join_cols(dTemp, dTempl);                                                   
  
  rTemp0 = numericalRating_.subvec(ind1 - 1, ind2 - 1);       
  rTempf = arma::zeros<vec>(1);                                 
  rTempl = arma::zeros<vec>(1);                               
  rTempf.fill(arma::datum::nan);                              
  rTempl.fill(arma::datum::nan);                              
  rTemp = arma::join_cols(rTempf, rTemp0);                    
  rTemp = arma::join_cols(rTemp, rTempl);                                  
  
  iTemp = arma::sort_index(dTemp);                                                                  
  
  dTemp = sort(dTemp);                                                 
  
  iTemp0 = iTemp.n_rows;                                      
  iTemp1 = iTemp.subvec(1 - 1, (iTemp0 - 1));                 
  rTemp = rTemp(iTemp1);                                                  
  
  
  //# Take care of rating information on the boundaries
  i1_ = find(iTemp == 1-1);                                   
  i1 = arma::conv_to< signed int >::from(i1_);                          
  n = dTemp.n_rows;                                                            
  i2_ = find(iTemp == n-1);                                  
  i2 = arma::conv_to< signed int >::from(i2_);                                
  
  if ((dTemp[i1] == dTemp[i1 + 1]) && (i1 + 1 < i2)) {
    i1 = i1 + 1;                                                                                       
  } else if (i1>0) {
    rTemp[i1] = rTemp[i1 - 1];                                                                       
  }
  
  if (dTemp[i2] == dTemp[i2 - 1]) {
    i2 = i2 - 1;                                                                                         
  } else {
    rTemp[i2] = rTemp[i2 - 1];                                             
  }
  
  
  mdate = dTemp.subvec(i1,i2);                                        
  rating = rTemp.subvec(i1,i2);                                                  
                                                            
  mdate_= wrap(mdate);
  rating_= wrap(rating);
  
  return List::create(Named("mdate") = mdate_, Named("rating") = rating_);
  
}





// [[Rcpp::export]]
arma::vec getRatingsSnapshotsLocal(signed int ind, arma::ivec numDate_, arma::ivec numericalRating_, arma::ivec StartPos_, arma::ivec snapshotDates_){
  
  signed int ind1;  
  signed int ind2;  
  
  
  //Note: C++ indices start with 0 unlike R which starts with 1
  ind1 = StartPos_(ind);                                             
  ind2 = StartPos_((ind) + 1) - 1;                   
  
  signed int s = snapshotDates_.n_rows;       
  
  arma::vec rating;
  rating.zeros(s, 1);                         
  
  
  // #Expand ratings history from 0 to inf and sort by date
  arma::ivec dTemp0 = numDate_.subvec(ind1-1, ind2-1);                 
  arma::ivec dTempf = arma::zeros<ivec>(1);                                  
  arma::ivec dTempl = arma::zeros<ivec>(1);                                  
  signed int infinit = 2000000000; /*(abs(arma::datum::inf));*/                      
  dTempl.fill(infinit);                                      
  arma::ivec dTemp00 = arma::join_cols(dTempf, dTemp0);                  
  arma::ivec dTemp000 = conv_to<ivec>::from(dTemp00);                        
  arma::ivec dTemp = arma::join_cols(dTemp000, dTempl);       
  
  
  arma::vec rTemp0 = conv_to<vec>::from(numericalRating_.subvec(ind1-1, ind2-1));         
  arma::vec rTempf = arma::zeros<vec>(1);                                  
  rTempf.fill(arma::datum::nan);                                      
  arma::vec rTemp = arma::join_cols(rTempf, rTemp0);          
  
  
  arma::uvec iTemp = arma::sort_index(dTemp);                 
  
  
  dTemp = sort(dTemp);                                               
  
  signed int iTemp0 = iTemp.n_rows;
  arma::uvec iTemp1 = iTemp.subvec(1-1, (iTemp0-1)-1);                
  rTemp = rTemp(iTemp1);                                             
  
  
  //#Fill in the ratings for the snapshot dates
  for(unsigned int i=0; i < dTemp.n_rows - 1; ++i){
    
    
    arma::uvec f_compare = (snapshotDates_ >= dTemp[i]);                  
    arma::uvec s_compare = (snapshotDates_ < dTemp[i + 1]);               
    
    std::vector<int> indxrating;
    for (unsigned int t = 0; t < f_compare.n_rows; ++t) {
      
      if ((f_compare[t] == 1) & (s_compare[t] == 1)) {
        
        indxrating.push_back(t);                                          
        
        
        
        arma::vec indxrating_ = arma::conv_to< arma::vec >::from(indxrating);       
        
        
        signed int gmin;
        signed int gmax;
        
        
        //Check if the indxrating is empty
        if(indxrating_.is_empty()==1){
          gmin = 0;
          gmax = indxrating_.n_rows;
        } else {
          gmin = indxrating_.min();
          gmax = indxrating_.max();
        }
        for (signed int r = gmin; r < gmax+1; ++r) {
          
          rating[r] = rTemp[i];                                  
        }
        
        
      } 
    }   
    
  }
  
  
  
  return rating;
  
}



// [[Rcpp::export]]
List getidTotCntCohortRCPP(signed int nIDs, arma::ivec numDate, arma::ivec numericalRating, arma::ivec StartPos, arma::ivec snapshotDates, int nRtgsCatNaN){
  
  
  arma::field<std::string> methods(nIDs);
  arma::sp_mat totalsVec;
  arma::sp_mat totalsMat;
  arma::vec rating;
  int intermTotalsVec;
  int intermTotalsMat;
  int prevRating;
  int currentRating;
  
  arma::field<arma::sp_mat> idTotCnt_totalsVec(nIDs,1);  
  arma::field<arma::sp_mat> idTotCnt_totalsMat(nIDs,1);
  
  
  for (signed int k=0; k < nIDs-1; ++k){                                         
  
    
    totalsVec = sp_mat(nRtgsCatNaN-1,1);
    totalsMat = sp_mat(nRtgsCatNaN-1,nRtgsCatNaN-1);
    
    rating = getRatingsSnapshotsLocal(k,numDate,numericalRating,StartPos,snapshotDates);    
    rating.replace(datum::nan, nRtgsCatNaN);                                                
    
    prevRating = rating[0];                                               
    
    if(snapshotDates.n_elem > 1){
      
      for (unsigned int t=1; t < snapshotDates.n_elem; ++t){
                                     
                                     
                                     
          if(prevRating!=nRtgsCatNaN){
          //Note: we automatically subtract one from the both the vector and matrix because by default the sparse matrix puts a one in these spots.
          intermTotalsVec = totalsVec(prevRating-1);    
          totalsVec(prevRating-1) = intermTotalsVec+1;  
          }
          currentRating = rating[t];                     
          
          if(prevRating!=nRtgsCatNaN){
            intermTotalsMat = totalsMat(prevRating-1,currentRating-1);   
          totalsMat(prevRating-1,currentRating-1) = intermTotalsMat+1;   
          }
        
        prevRating = currentRating;                     
        
        
      }
    }
    
    idTotCnt_totalsVec(k,0) =  totalsVec;  
    idTotCnt_totalsMat(k,0) =  totalsMat;  
    methods(k) = "cohort";
    
  }
  
  return List::create(Named("idTotCnt_totalsVec") = wrap(idTotCnt_totalsVec), Named("idTotCnt_totalsMat") = wrap(idTotCnt_totalsMat),  Named("idTotCnt_methods") = wrap(methods));
  
}




// [[Rcpp::export]]
List getidTotCntDurationRCPP(signed int nIDs, arma::ivec numDate, arma::vec numericalRating, arma::ivec StartPos,  int nRtgsCatNaN, String algo,signed int sDate, signed int eDate){

  arma::field<std::string> methods(nIDs);
  arma::sp_mat totalsVec;
  arma::sp_mat totalsMat;
  List date_rating;
  IntegerVector Rating;
  arma::vec rating;
  IntegerVector Mdate;
  arma::vec mdate;
  int prevDate;
  int prevRating;
  int currentRating;
  int currentDate;
  int year1;
  int month1;
  int day1;
  int year2=0;
  double  dayDiffYr2=0.0;
  double ly2=0.0;
  double daysInYr2=0.0;
  double elapsedTime=0.0;

  
  arma::field<arma::sp_mat> idTotCnt_totalsVec(nIDs,1);  
  arma::field<arma::sp_mat> idTotCnt_totalsMat(nIDs,1);

  for (signed int k=0; k < nIDs-1; ++k){ 

    totalsVec = sp_mat(nRtgsCatNaN-1,1);
    totalsMat = sp_mat(nRtgsCatNaN-1,nRtgsCatNaN-1);
                                                                                         
    date_rating = getCategoriesLocal(k,numDate,numericalRating,StartPos, sDate,eDate);       
    Rating      = date_rating["rating"]; 
    rating      = as<arma::vec>(Rating);
    Mdate       = date_rating["mdate"]; 
    mdate       = as<arma::vec>(Mdate);                           
    
    rating.replace(datum::nan, nRtgsCatNaN);
    
    //Info at start time
    prevDate = mdate[0];                                          
    prevRating =rating[0];                                        


    
    for (unsigned int t=1; t<mdate.n_elem; ++t){

        // Get info on latest transition
        currentDate = mdate[t];                                                                     
        currentRating = rating[t];                                     
        
        Date prevDate_d(((((prevDate - 719529)*86400)/60)/60/24));                              
        Date currentDate_d(((((currentDate - 719529)*86400)/60)/60/24));                        

        year1 = prevDate_d.getYear();  
        month1 = prevDate_d.getMonth();
        day1 = prevDate_d.getDay();
        year2 = currentDate_d.getYear(); 
        Date prevDate_d2(year2,month1,day1);
        dayDiffYr2  = currentDate_d - prevDate_d2;                                 
        ly2 = (((year2%4 == 0) & (year2%100 != 0)) | (year2%400 == 0));              
        daysInYr2 = 365 + ly2; 
        elapsedTime = ((year2-year1)+(dayDiffYr2/daysInYr2));                           

        
        if(prevRating!=nRtgsCatNaN){
          std::setprecision(5);
        totalsVec(prevRating-1) = totalsVec(prevRating-1)+elapsedTime;         
        
          if ((prevRating-1)!=(currentRating-1)){
           totalsMat(prevRating-1,currentRating-1) = totalsMat(prevRating-1,currentRating-1) + 1;    
          }
        }
        
        prevDate = currentDate;
        prevRating = currentRating;
  
    }

    idTotCnt_totalsVec(k,0) =  totalsVec;  
    idTotCnt_totalsMat(k,0) =  totalsMat;  
     methods(k) = "duration";
    
  }
  

  return List::create(Named("idTotCnt_totalsVec") = wrap(idTotCnt_totalsVec), Named("idTotCnt_totalsMat") = wrap(idTotCnt_totalsMat),  Named("idTotCnt_methods") = wrap(methods));
  
  
}




// [[Rcpp::export]]
List getSampleTotals(List lsttotals_totalsMat, List lsttotals_totalsVec, StringVector totals_methods, int sliceCnt, int rownum, int colnum) {

  arma::sp_mat totalsVec;         
  arma::sp_mat totalsMat;         
  
  totalsVec = sp_mat(rownum,1); 
  totalsMat = sp_mat(rownum,colnum);  


if (sliceCnt==1){

  totalsVec =  totalsVec + as<arma::sp_mat>(lsttotals_totalsVec[0]);       
  totalsMat = totalsMat +  as<arma::sp_mat>(lsttotals_totalsMat[0]);        
  
} else {

  for (signed int k = 0; k < sliceCnt-1; ++k) {

    totalsVec =  totalsVec + as<arma::sp_mat>(lsttotals_totalsVec[k]);        
    totalsMat = totalsMat +  as<arma::sp_mat>(lsttotals_totalsMat[k]);        

  }

}  
  
  return List::create(Named("totalsVec") = totalsVec, Named("totalsMat") = totalsMat, Named("method") =totals_methods);
  
  
  
}





// [[Rcpp::export]]          
DataFrame getExpandTransData(StringVector industryName, StringVector Qtr_Year,DateVector endDate,StringVector Rating_Trans,StringVector start_Rating,StringVector end_rating, arma::vec mCount,StringVector  wgtname, int transData_nm_cnt, double totalCountExpanded){


  StringVector industryNameV(totalCountExpanded);
  StringVector Qtr_YearV(totalCountExpanded);
  DateVector endDateV(totalCountExpanded);
  StringVector Rating_TransV(totalCountExpanded);
  StringVector start_RatingV(totalCountExpanded);
  StringVector end_ratingV(totalCountExpanded);
  NumericVector mCountV(totalCountExpanded);
  NumericVector wghtV(totalCountExpanded);
  NumericVector IdV(totalCountExpanded);
  
  
  int getCounter = 0;

  for (unsigned int i = 0; i < mCount.n_rows; ++i){      

      int wght = mCount(i);
  
      for (int c = 0; c < wght; ++c){    // for (c in 1:wght){

        industryNameV(getCounter)= industryName(i);
        Qtr_YearV(getCounter)= Qtr_Year(i);
        endDateV(getCounter)= endDate(i);
        Rating_TransV(getCounter) =  Rating_Trans(i);
        start_RatingV(getCounter)= start_Rating(i);
        end_ratingV(getCounter)=  end_rating(i);
        mCountV(getCounter)=  mCount(i);           
        wghtV(getCounter)=  wght;
        IdV(getCounter) = c; 

        getCounter = getCounter +1;
        
     }

   }
  
  
  return DataFrame::create(_["industryName"]= industryNameV, _["Qtr_Year"]= Qtr_YearV,
                           _["endDate"]= endDateV,_["Rating_Trans"]=  Rating_TransV,
                           _["start_Rating"]= start_RatingV, _["end_rating"]= end_ratingV,
                           _["mCount"]= mCountV, _["wght"]= wghtV,
                           _["Id"]= IdV);
}

