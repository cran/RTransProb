#' Forecast - using Support Vector Machines
#'
#' @description This model implements a forecasting method using Support Vector Machines.
#'
#' @usage transForecast_svm(transData, histData, predData_svm, startDate, endDate,
#'                     depVar, indVars,  ratingCat, pct, tuning, kernelType, cost,
#'                     cost.weights, gamma, gamma.weights)
#'
#' @param transData aggregate transition matrix data.
#' @param histData historical macroeconomic,financial and non-financial data.
#' @param predData_svm forecasting data.
#' @param startDate start date of the estimation time window, in string or numeric format.
#' @param endDate end date of the estimation time window, in string or numeric format.
#' @param depVar dependent variable, as a string.
#' @param indVars list containing the independent variables.
#' @param ratingCat list containing the unique rating caetgories
#' @param pct percent of data used in training dataset.
#' @param tuning perform tuning. If tuning='TRUE' tuning is perform. If tuning='FALSE' tuning is not performed
#' @param cost cost of constraints violation (default: 1) it is the 'C' constant of the regularization term in the Lagrange formulation.
#' @param cost.weights vector containing tuning parameters for cost
#' @param gamma parameter needed for all kernels except linear (default: 1/(data dimension))
#' @param gamma.weights vector containing tuning parameters for gamma
#' @param kernelType the kernel used in training and predicting (see Package e1071 for more detail)
#'
#' @return The output consists of a forecasted transition matrix.
#'
#' @export
#'
#' @author  Abdoulaye (Ab) N'Diaye
#'
#'
#' @examples
#' startDate <- as.Date("1990-01-01")
#' endDate <- as.Date("2005-01-01")
#' depVar <- c("end_rating")
#' indVars <-c("Macro1", "Financial1","Industry1")
#' pct <- 0.8
#' wgt <-  "mCount"
#' ratingCat <- c("A","B", "C", "D", "E", "F", "G", "N")
#' lstCategoricalVars <- c("end_rating")
#' tuning <- "TRUE"
#' cost <- 100
#' gamma <- .1
#' cost.weights <- c(0.1, 10, 100)
#' gamma.weights <- c(0.01, 0.25, 0.5, 1)
#' kernelType <- "radial"
#'
#' \dontrun{
#' transData <- expandTransData(df,wgt)
#'}
#' \dontrun{
#' svm_T<-transForecast_svm(transData, histData, predData_svm,  startDate, endDate,
#'        depVar,indVars,  ratingCat, pct, tuning, kernelType,cost, cost.weights,
#'        gamma, gamma.weights)
#'}
#'
transForecast_svm <- function(transData, histData, predData_svm, startDate, endDate, depVar, indVars,  ratingCat, pct, tuning, kernelType,cost,cost.weights,gamma,gamma.weights) {

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
  transData$Qtr_Year <- as.Date(zoo::as.yearqtr(transData$Qtr_Year))

  histData$Date  <- as.Date(zoo::as.yearqtr(histData$Date))

  #merge histData and transData
  trainData<-merge(x=transData,y=histData, by.x="Qtr_Year", by.y = "Date")
  trainData_1 <- subset(trainData, trainData$Qtr_Year>=startDate & trainData$Qtr_Year<=endDate)

  LvL <- ratingCat

  #create a matrix to hold the final transition matrix
  TransitionMatrix_SVM <- matrix(numeric(0), length(LvL),length(LvL), dimnames = list(LvL,LvL), byrow=TRUE)
  if(tuning=="TRUE"){
  TransitionMatrix_SVM.Tuned <- matrix(numeric(0), length(LvL),length(LvL), dimnames = list(LvL,LvL), byrow=TRUE)
  }
  #TransitionMatrix_SVM.AUC <- matrix(numeric(0), length(LvL),length(LvL), dimnames = list(LvL,LvL), byrow=TRUE)

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
    train.data <- trainData.df[indexes,]

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
      #cost.weights <- c(0.1, 10, 100)
      #gamma.weights <- c(0.01, 0.25, 0.5, 1)
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
  #TransitionMatrix_SVM.AUC[is.na(TransitionMatrix_SVM)] <- 0





  #svmOutput <- list(TransitionMatrix_SVM=TransitionMatrix_SVM,
  #                  TransitionMatrix_SVM.Tuned=TransitionMatrix_SVM.Tuned);

  return(svmOutput)


}

