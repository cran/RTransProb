


cm.matrix.counts<-function (mtrix)
{
  # tests if the given matrix mtrix is a migration matrix. So the dimensions of the migration matrix should
  # be at least 2 times 2 and the row and column dimensions must be equal. Further the values in the migration
  # matrix should be between 0 and 1. And the sum of each row should be 1.
  
  if (!is.matrix(mtrix))
    stop("mtrix is not a matrix")
  if (dim(mtrix)[1] < 2 || dim(mtrix)[2] < 2 || dim(mtrix)[1] != dim(mtrix)[2])
    stop("incorrect dimension of migration matrix")
  if (length(mtrix[mtrix < 0]) != 0)
    stop("negative value in input data")
  
}




cm.matrix<-function (mtrix)
{
  # tests if the given matrix mtrix is a migration matrix. So the dimensions of the migration matrix should
  # be at least 2 times 2 and the row and column dimensions must be equal. Further the values in the migration
  # matrix should be between 0 and 1. And the sum of each row should be 1.
  
  if (!is.matrix(mtrix))
    stop("mtrix is not a matrix")
  if (dim(mtrix)[1] < 2 || dim(mtrix)[2] < 2 || dim(mtrix)[1] != dim(mtrix)[2])
    stop("incorrect dimension of migration matrix")
  if (length(mtrix[mtrix < 0]) != 0)
    stop("negative value in input data")
  
  y <- numeric(dim(mtrix)[1])
  for (i in 1:dim(mtrix)[1]) {
    y[i] <- sum(mtrix[i, ])
  }
  y <- round(y, digits = 12)
  
}



cm.vector.counts<-function (vctr)
{
  # tests if the given matrix mtrix is a migration matrix. So the dimensions of the migration matrix should
  # be at least 2 times 2 and the row and column dimensions must be equal. Further the values in the migration
  # matrix should be between 0 and 1. And the sum of each row should be 1.
  
  if (!is.vector(vctr))
    stop("mtrix is not a vector")
  if (length(vctr[vctr < 0]) != 0)
    stop("negative value in input data")
}





# Get totals per ID, cohort method

getidTotCntCohort <- function(numDate,numericalRating,StartPos,sDate,eDate,RatingsCat, snapshots){
  
  idTotCnt_method = list()
  idTotCnt           = list()
  
  nIDs <- nrow(StartPos);
  algo <- 'cohort';
  
  #Introduce an artificial 'NaN' state - time window can be larger than some
  #companies' migration history
  nRtgsCatNaN <- RatingsCat+1;
  
  
  if( snapshots==12){
    eDate <- POSIXTomatlab(as.POSIXlt(as.Date(matlabToPOSIX(eDate))))
  }
  
  
  sDates_yrs <- as.numeric(format(as.Date(matlabToPOSIX(sDate)),'%Y'))
  sDates_mns <- as.numeric(format(as.Date(matlabToPOSIX(sDate)),'%m'))
  sDates_dys <- as.numeric(format(as.Date(matlabToPOSIX(sDate)),'%d'))
  if( sDates_mns==1 & sDates_dys==31){
    snapshotDates <- cfdates(sDate,eDate,snapshots)
  } else {

    snapshotDates <- cfdates(sDate,eDate,snapshots)
  }
  

  idTotCntRCPP <- getidTotCntCohortRCPP(nIDs, numDate, numericalRating,as.numeric(StartPos[,1]), snapshotDates,nRtgsCatNaN) 

  
  idTotCnt_totalsVec <- idTotCntRCPP$idTotCnt_totalsVec
  idTotCnt_totalsMat <- idTotCntRCPP$idTotCnt_totalsMat
    
idTotCnt  <- list(totalsVec=idTotCnt_totalsVec,
                  totalsMat=idTotCnt_totalsMat,
                  method=idTotCntRCPP$idTotCnt_methods)
 

  return(idTotCnt)
}



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
  
  idTotCntRCPP <- getidTotCntDurationRCPP(nIDs, numDate, numericalRating,as.numeric(StartPos[,1]), nRtgsCatNaN, algo,sDate,eDate) 
  idTotCnt_totalsVec <- idTotCntRCPP$idTotCnt_totalsVec
  idTotCnt_totalsMat <- idTotCntRCPP$idTotCnt_totalsMat
  idTotCnt_method <- idTotCntRCPP$idTotCnt_methods
  
  idTotCnt  <- list(totalsVec=idTotCnt_totalsVec,
                    totalsMat=idTotCnt_totalsMat,
                    method=idTotCnt_method)
  
  return(idTotCnt)
  
}





is.leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}




fixDate <- function(data_set) {
  
  text_features <- c(names(data_set[sapply(data_set, is.character)]), names(data_set[sapply(data_set, is.factor)]))
  
  for (feature_name in text_features) {
    feature_vector <- as.character(data_set[,feature_name])
    
    date_pattern_m_dd_YYYY1 <- '[0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]'    
    date_pattern_dd_mm_YYYY1 <- '[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]'
    date_pattern_dd_mm_YYYY2 <- '[0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]'
    date_pattern_YYYY_mm_dd1 <- '[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]'
    date_pattern_YYYY_mm_dd2 <- '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]'
    
    
    validDate = "NO"
    if (any(grepl(date_pattern_m_dd_YYYY1, data_set[[feature_name]]))){
      date_pattern <- date_pattern_m_dd_YYYY1
      validDate = "Yes"  
      type = "m_dd_YYYY1"
    } else if (any(grepl(date_pattern_dd_mm_YYYY1, data_set[[feature_name]]))){
      date_pattern <- date_pattern_dd_mm_YYYY1
      validDate = "Yes"  
      type = "dd_mm_YYYY1"
    } else if (any(grepl(date_pattern_dd_mm_YYYY2, data_set[[feature_name]]))){
      date_pattern <- date_pattern_dd_mm_YYYY2
      validDate = "Yes"
      type = "dd_mm_YYYY2"
    } else if (any(grepl(date_pattern_YYYY_mm_dd1, data_set[[feature_name]]))) {
      date_pattern <- date_pattern_YYYY_mm_dd1
      validDate = "Yes"
      type = "YYYY_mm_dd1"
    } else if (any(grepl(date_pattern_YYYY_mm_dd2, data_set[[feature_name]]))){
      date_pattern <- date_pattern_YYYY_mm_dd2
      validDate = "Yes"  
      type = "YYYY_mm_dd2"
    }
    
    
    if (validDate=="Yes"){
      if (max(nchar(feature_vector)) == 10) {
        if (sum(grepl(date_pattern, feature_vector)) > 0) {
          #print(paste('Casting dates:',feature_name))
          if (type == "m_dd_YYYY1"){
            data_set[,feature_name] <- as.Date(feature_vector, format="%m/%d/%Y")
          } else if (type == "YYYY_mm_dd1"){
            data_set[,feature_name] <- as.Date(feature_vector, format="%Y/%m/%d")
          } else if (type == "YYYY_mm_dd2"){
            data_set[,feature_name] <- as.Date(feature_vector, format="%Y-%m-%d")
          } else if (type == "dd_mm_YYYY1"){
            data_set[,feature_name] <- as.Date(feature_vector, format="%d/%m/%Y")
          } else if (type == "dd_mm_YYYY2"){
            data_set[,feature_name] <- as.Date(feature_vector, format="%d-%m-%Y")
          }
          
        }
      }
    }
    
    
  }
  
  return (data_set)
  
}








forecast_mnl <- function(transData, histData, predData, startDate, endDate, ref, depVar, indVars, ratingCat, wgt) {
  
  if (is.data.frame(transData) && nrow(transData)==0){ stop("Error: this function requires historical loan tra")}
  
  if ((is.null(startDate))){ stop("Error: 'startDate' is missing")}
  if ((is.null(endDate))){ stop("Error: 'endDate' is missing")}
  if ((is.null(ref))){ stop("Error: 'ref' is missing. A reference category must be specified")}
  if (length(depVar)!=1){ stop("Error: A dependent variable is required")}
  if (length(indVars)<=1){ stop("Error: A list of independent variables is required")}
  if (length(ratingCat)<=1){ stop("Error: A list of rating categories is required")}
  
  
  
  
  #format specific data in histData and transData
  transData$endDate <- as.character(transData$endDate)
  transData$endDate  <- as.Date(transData$endDate,format="%Y-%m-%d")
  ########transData$Qtr_Year <- as.Date(zoo::as.yearqtr(transData$Qtr_Year))
  
  ########histData$Date  <- as.Date(zoo::as.yearqtr(histData$Date))
  
  #merge histData and transData
  trainData<-merge(x=transData,y=histData, by.x="Qtr_Year", by.y = "Date")
  ########trainData <- subset(trainData, trainData$Qtr_Year>=startDate & trainData$Qtr_Year<=endDate)
  trainData <- subset(trainData, trainData$endDate>=startDate & trainData$endDate<=endDate)
  trainData <- subset(trainData, trainData[[wgt]] >0)
  
 
  #assign rownames to the prediction data
  trainData$start_Rating <- as.factor(trainData$start_Rating)  #make sure ratings info is a factor
  trainData$end_rating <- as.factor(trainData$end_rating)      #make sure ratings info is a factor
  rn  <- ratingCat                                             #ratingCat[-(length(ratingCat))]
  rnr <- rn[rn != ref]                    #remove the ref category from the list of levels
  rn  <- c(rnr,ref)                       #put the ref category back in the last position of the list
  row.names(predData) <-  rn              #assign rownames to the prediction data
  
  dummyList <- c()
  #create dummy variables for the off-reference categories
  for(t in unique(rnr)) {
    trainData[paste("D",t,sep="_")] <- ifelse(trainData$start_Rating==t,1,0)
    dummyList <- c(dummyList,paste("D",t,sep="_"))
  }
  
  #set reference category
  trainData$end_rating <- as.factor(trainData$end_rating)
  trainData$end_rating <- stats::relevel(trainData$end_rating, ref = ref)
  
  #create regression formula
  count <- 1
  formula.init <- paste(depVar, " ~ ", sep =  )
  for (v in unique(indVars)){
    
    if (count==1){
      formula.init <- paste(formula.init, v, sep="")
    } else {
      formula.init <- paste(formula.init, " + ", v, sep="")
    }
    count<-count+1
  }
  
  for (d in unique(dummyList)){
    formula.init <- paste(formula.init, " + ", d, sep="")
  }
  
  formula.init <- stats::as.formula(formula.init)
  
  
  #running MNL regression
  mnlReg <- nnet::multinom(formula.init, data=trainData, weights=trainData[[wgt]],maxit = 1000000)
  
  mnlSummary <- summary(mnlReg)
  mnlZ       <- summary(mnlReg)$coefficients/summary(mnlReg)$standard.errors
  mnlP       <- (1 - stats::pnorm(abs(mnlZ), 0, 1)) * 2
  mnlExp     <- exp(cbind(stats::coef(mnlReg), stats::confint(mnlReg))) #extract the coefficients from the model and exponentiate
  mnlFitted  <- stats::fitted(mnlReg)
  
  mnlPredict <-stats::predict(mnlReg, newdata = predData, "probs")
  #Note: in the output of 'predict' the A to A reference is the last row because it goes through all of the explicit transitions then produces the one for
  #       A to A. Also, there is not row signifying the transition 'from' G because G is an absorbing state and the data does not have any thing that has
  #       G as a start state.
  
  #sort the predicted results by row then by column
  mnlPredict <- mnlPredict[match(ratingCat,row.names(mnlPredict)),]   #by row
  mnlPredict <- subset(mnlPredict, select=ratingCat)                  #by column
  
  
  mnlPredict<-as.data.frame(mnlPredict)
  
  #add Default column
  mnlPredict['Def'] <- mnlPredict[[ratingCat[length(ratingCat)]]]
  mnlPredict['Def'][mnlPredict['Def'] > 0] <- 0
  
  #add Default row
  temprow <- matrix(c(rep.int(NA,length(mnlPredict))),nrow=1,ncol=length(mnlPredict))
  temprow<-as.data.frame(temprow)
  names(temprow)<-names(mnlPredict)
  row.names(temprow) <- "Def"
  temprow[is.na(temprow)] <- 0
  mnlPredict <- rbind(mnlPredict,temprow)
  
  #fill in default values
  mnlPredict$Def <- 1- rowSums(mnlPredict)
  
  
  mnlOutput <- list(mnl_Reg=mnlReg,
                    mnl_Summary=mnlSummary,
                    mnl_Z=mnlZ,
                    mnl_P=mnlP,
                    mnl_Exp=mnlExp,
                    mnl_Fitted=mnlFitted,
                    mnl_Predict=mnlPredict);
  return(mnlOutput)
  
}







# svm_T<-transForecast_svm(transData, histData, predData_svm,  startDate, endDate,
#        depVar,indVars,  ratingCat, pct, tuning, kernelType,cost, cost.weights,
#        gamma, gamma.weights)
#
#
forecast_svm <- function(transData, histData, predData_svm, startDate, endDate, depVar, indVars,  ratingCat, pct, tuning, kernelType,cost,cost.weights,gamma,gamma.weights) {
  
  ## data type transformations - factoring
  to.factors <- function(df, variables){
    for (variable in variables){
      df[[variable]] <- as.factor(df[[variable]])
    }
    return(df)
  }
  
  ## normalizing - scaling
  scale.features <- function(df, variables){
    for (variable in variables){
      df[[variable]] <- scale(df[[variable]], center=T, scale=T)
    }
    return(df)
  }
  
  
  #format specific data in histData and transData
  transData$endDate <- as.character(transData$endDate)
  transData$endDate  <- as.Date(transData$endDate,format="%Y-%m-%d")
  ########transData$Qtr_Year <- as.Date(zoo::as.yearqtr(transData$Qtr_Year))
  
  ########histData$Date  <- as.Date(zoo::as.yearqtr(histData$Date))
  
  #merge histData and transData
  trainData<-merge(x=transData,y=histData, by.x="Qtr_Year", by.y = "Date")
  #trainData_1 <- subset(trainData, trainData$Qtr_Year>=startDate & trainData$Qtr_Year<=endDate)
  trainData_1 <- subset(trainData, trainData$endDate>=startDate & trainData$endDate<=endDate)
  
  LvL <- ratingCat
  
  #create a matrix to hold the final transition matrix
  TransitionMatrix_SVM <- matrix(numeric(0), length(LvL),length(LvL), dimnames = list(LvL,LvL), byrow=TRUE)
  if(tuning=="TRUE"){
    TransitionMatrix_SVM.Tuned <- matrix(numeric(0), length(LvL),length(LvL), dimnames = list(LvL,LvL), byrow=TRUE)
  }
  #TransitionMatrix_SVM.AUC <- matrix(numeric(0), length(LvL),length(LvL), dimnames = list(LvL,LvL), byrow=TRUE)
  
  for (k in 1:(length(LvL)-1)){
#print(paste("svm enter - ",k,sep=""))
    trainData <- subset(trainData_1, trainData_1$start_Rating %in% c(LvL[k]))
    
    #-------New---------------------------------------------------------
    trainData$start_Rating <- as.factor(trainData$start_Rating)  #make sure ratings info is a factor
    trainData$end_rating <- as.factor(trainData$end_rating)     #make sure ratings info is a factor
    
    trainData.df <- trainData
    trainData.df  <- trainData[c(depVar, indVars)]
    
    
    # split data into training and test datasets. Extract the forecast data from the training data this way the forecast dataset
    # will have the same number as factors are the training dataset....
    # 2) setup training and testing data
    trainData.df<-utils::head(trainData.df,-1)        #return all rows except the last row (which contains the forecast data)
    indexes <- sample(1:nrow(trainData.df), size=pct*nrow(trainData.df))
    train.data <- trainData.df[sort(indexes),]
    
    test.data <- trainData.df[-indexes,]
    test.feature.vars <- test.data[,-1]        #all other variables   (independent vars)
    test.class.var <- test.data[,1]            #credit rating         (dependent var)
    
    
    #import the forecast data and append it to the training dataset
    forecast.data <-predData_svm[,-1]
    test.feature.vars <- rbind(test.feature.vars, forecast.data)               #append the forecast data to the last row of the larger dataset
    
    forc.feature.vars <- utils::tail(test.feature.vars,1)
    test.feature.vars <-utils::head(test.feature.vars,-1)
    
    #-------------------------------SVM (before Tuning) using selected features ---------------------------------
    
    
    ## build initial model with training data
    
    #create regression formula
    count <- 1
    formula.init <- paste(depVar, " ~ ", sep =  )
    for (v in unique(indVars)){
      
      if (count==1){
        formula.init <- paste(formula.init, v, sep="")
      } else {
        formula.init <- paste(formula.init, " + ", v, sep="")
      }
      count<-count+1
    }
    
    
    formula.init <- stats::as.formula(formula.init)
    svm.model <- e1071::svm(formula=formula.init, data=train.data,
                            kernel=kernelType, cost=cost, gamma=gamma,  probability=TRUE)
    
    ## view inital model details
    summary(svm.model)
    
    ## predict and evaluate results (using real forecasting data)
    svm.predictions <- stats::predict(svm.model, forc.feature.vars, probability=TRUE)
    svm.predictions <- attr(svm.predictions, "probabilities")
    
    ## predict and evaluate results (using test data)
    svm.predictions.test <- stats::predict(svm.model, test.feature.vars)
    
    
    #get names from the svm.predictions
    pred.LvL <- dimnames(svm.predictions)[[2]]
    
    
    for (trans in 1:length(LvL)){
      try(TransitionMatrix_SVM[k,trans] <- svm.predictions[,LvL[trans]],TRUE)
    }
    
    
    #---------------------------------------------------------------------------------------------
    
    
    #-------------------------SVM USing Tuning----------------------------------------------------
    if(tuning=='TRUE'){
      ## hyperparameter optimizations
      
      # run grid search
      tuning.results <- e1071::tune(e1071::svm, formula.init,
                                    data = train.data, kernel="radial",
                                    ranges=list(cost=cost.weights, gamma=gamma.weights,probability = TRUE))
      
      # get best model and evaluate predictions
      svm.model.best = tuning.results$best.model
      
      
      ## predict and evaluate results (using real forecasting data)
      svm.predictions.best <- stats::predict(svm.model.best, forc.feature.vars, probability=TRUE)
      svm.predictions.best <- attr(svm.predictions.best, "probabilities")
      
      ## predict and evaluate results (using test data)
      svm.predictions.best.test <- stats::predict(svm.model.best, test.feature.vars)
      
      
      #get names from the svm.predictions
      pred.LvL <- dimnames(svm.predictions.best)[[2]]
      
      for (trans in 1:length(LvL)){
        try(TransitionMatrix_SVM.Tuned[k,trans] <- svm.predictions.best[,LvL[trans]],TRUE)
      }
      
      
      # plot best model evaluation metric curves
      svm.predictions.best <- stats::predict(svm.model.best, test.feature.vars, decision.values = T)
      svm.prediction.values <- attributes(svm.predictions.best)$decision.values
      
    }
    
    rm(trainData)
    rm(trainData.df)
    rm(forecast.data)
    
    #print(paste("svm end - ",k,sep=""))
  }
  
  #sort the predicted results by row then by column
  TransitionMatrix_SVM[is.na(TransitionMatrix_SVM)] <- 0
  TransitionMatrix_SVM[length(TransitionMatrix_SVM)]<-1
  
  svmOutput <- list(TransitionMatrix_SVM=TransitionMatrix_SVM)
  
  if(tuning=="TRUE"){
    TransitionMatrix_SVM.Tuned[is.na(TransitionMatrix_SVM.Tuned)] <- 0
    TransitionMatrix_SVM.Tuned[length(TransitionMatrix_SVM.Tuned)]<-1
    
    
    svmOutput <- list(TransitionMatrix_SVM=TransitionMatrix_SVM,
                      TransitionMatrix_SVM.Tuned=TransitionMatrix_SVM.Tuned)
  }

  
  return(svmOutput)
  
  
}




forecast_lda <- function(transData, histData, predData_lda, startDate, endDate,  method, interval, snapshots, depVar, indVars,  pct, ratingCat) {
  
  
  ## data type transformations - factoring
  to.factors <- function(df, variables){
    for (variable in variables){
      df[[variable]] <- as.factor(df[[variable]])
    }
    return(df)
  }
  
  ## normalizing - scaling
  scale.features <- function(df, variables){
    for (variable in variables){
      df[[variable]] <- scale(df[[variable]], center=T, scale=T)
    }
    return(df)
  }
  
  
  #format specific data in histData and transData
  transData$endDate <- as.character(transData$endDate)
  transData$endDate  <- as.Date(transData$endDate,format="%Y-%m-%d")
  
  #td <- zoo::as.yearqtr(transData$Qtr_Year)
  #transData$Qtr_Year <- as.Date(td,format="%Y-%m-%d")
  
  #hd <- zoo::as.yearqtr(histData$Date)
  #histData$Date  <- as.Date(hd,format="%Y-%m-%d")
  
  #merge histData and transData
  trainData<-merge(x=transData,y=histData, by.x="Qtr_Year", by.y = "Date")
  trainData_1 <- subset(trainData, trainData$endDate>=startDate & trainData$endDate<=endDate)
  
  LvL <- ratingCat
  
  #create a matrix to hold the final transition matrix
  TransitionMatrix_lda <- matrix(numeric(0), length(LvL),length(LvL), dimnames = list(LvL,LvL), byrow=TRUE)
  
  for (k in 1:(length(LvL)-1)){
    
    trainData <- subset(trainData_1, trainData_1$start_Rating %in% c(LvL[k]))
    
    #-------New---------------------------------------------------------
    trainData$start_Rating <- as.factor(trainData$start_Rating)  #make sure ratings info is a factor
    trainData$end_rating <- as.factor(trainData$end_rating)     #make sure ratings info is a factor
    
    trainData.df <- trainData
    trainData.df  <- trainData[c(depVar, indVars)]
    
    
    # split data into training and test datasets. Extract the forecast data from the training data this way the forecast dataset
    # will have the same number as factors are the training dataset....
    # 2) setup training and testing data
    trainData.df<-utils::head(trainData.df,-1)        #return all rows except the last row (which contains the forecast data)
    indexes <- sample(1:nrow(trainData.df), size=pct*nrow(trainData.df))
    train.data <- trainData.df[sort(indexes),]
    
    test.data <- trainData.df[-indexes,]
    test.feature.vars <- test.data[,-1]        #all other variables   (independent vars)
    test.class.var <- test.data[,1]            #credit rating         (dependent var)
    
    
    #import the forecast data and append it to the training dataset
    forecast.data <-predData_lda[,-1]
    test.feature.vars <- rbind(test.feature.vars, forecast.data)               #append the forecast data to the last row of the larger dataset
    
    forc.feature.vars <- utils::tail(test.feature.vars,1)
    test.feature.vars <-utils::head(test.feature.vars,-1)
    
    #-------------------------------lda (before Tuning) using selected features ---------------------------------
    
    
    ## build initial model with training data
    
    #create regression formula
    count <- 1
    formula.init <- paste(depVar, " ~ ", sep =  )
    for (v in unique(indVars)){
      
      if (count==1){
        formula.init <- paste(formula.init, v, sep="")
      } else {
        formula.init <- paste(formula.init, " + ", v, sep="")
      }
      count<-count+1
    }
    
    
    formula.init <- stats::as.formula(formula.init)
    
    #running lda regression
    lda.model <- MASS::lda(formula.init, data=train.data)
    
    ## view inital model details
    summary(lda.model)
    
    ## predict and evaluate results (using real forecasting data)
    lda.predictions <- stats::predict(lda.model, forc.feature.vars, probability=TRUE)
    #print(lda.predictions)
    
    ## predict and evaluate results (using test data)
    lda.predictions.test <- stats::predict(lda.model, test.feature.vars)
    
    
    #get names from the lda.predictions
    pred.LvL <- dimnames(lda.predictions)[[2]]
    
    
    for (trans in 1:length(LvL)){
      try(TransitionMatrix_lda[k,trans] <- lda.predictions$posterior[,LvL[trans]],TRUE)
    }
    
    
    #---------------------------------------------------------------------------------------------
    
    
    rm(trainData)
    rm(trainData.df)
    rm(forecast.data)
    
    
  }
  
  #sort the predicted results by row then by column
  TransitionMatrix_lda[is.na(TransitionMatrix_lda)] <- 0
  TransitionMatrix_lda[length(TransitionMatrix_lda)]<-1
  
  ldaOutput <- list(TransitionMatrix_lda=TransitionMatrix_lda)
  
  # if(tuning=="TRUE"){
  #   TransitionMatrix_lda.Tuned[is.na(TransitionMatrix_lda.Tuned)] <- 0
  #   TransitionMatrix_lda.Tuned[length(TransitionMatrix_lda.Tuned)]<-1
  #
  #
  #   ldaOutput <- list(TransitionMatrix_lda=TransitionMatrix_lda,
  #                     TransitionMatrix_lda.Tuned=TransitionMatrix_lda.Tuned)
  # }
  #TransitionMatrix_lda.AUC[is.na(TransitionMatrix_lda)] <- 0
  
  
  
  
  
  #ldaOutput <- list(TransitionMatrix_lda=TransitionMatrix_lda,
  #                  TransitionMatrix_lda.Tuned=TransitionMatrix_lda.Tuned);
  
  return(ldaOutput)
  
}

