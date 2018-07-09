#' Forecast - using Support Vector Machines
#'
#' @description This model implements a forecasting method using Support Vector Machines.
#'
#' @usage transForecast_svm(data, histData, predData_svm, startDate, endDate,
#'                     method, interval, snapshots, defind, depVar, indVars,  ratingCat, 
#'                     pct, tuning, kernelType, cost, cost.weights, gamma, gamma.weights)
#'
#' @param data a table containing historical credit ratings data (i.e., credit migration data). A dataframe of size \emph{nRecords} x 3 where each row contains an ID (column 1), a date (column 2), and a credit rating (column 3); The credit rating is the rating assigned to the corresponding ID on the corresponding date.
#' @param histData historical macroeconomic,financial and non-financial data.
#' @param predData_svm forecasting data.
#' @param startDate start date of the estimation time window, in string or numeric format.
#' @param endDate end date of the estimation time window, in string or numeric format.
#' @param method  estimation algorithm, in string format. Valid values are 'duration'  or 'cohort'.
#' @param interval  the length of the transition interval under consideration, in years. The default value is 1, \emph{i.e., 1-year transition probabilities are estimated}.
#' @param snapshots  integer indicating the number of credit-rating snapshots per year to be considered for the estimation. Valid values are 1, 4, or 12. The default value is 1, \emph{i.e., one snapshot per year}. This parameter is only used in the 'cohort' algorithm.
#' @param defind Default Indicator
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
#' @return The output consists of a forecasted transition matrix using SVM.
#'
#' @export
#'
#' @author  Abdoulaye (Ab) N'Diaye
#' 
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(plyr)
#' library(Matrix)
#' library(tictoc)
#' 
#' 
#' 
#'for (i in c(24, 25, 26)) {
#'  print(paste("RUN-",i,sep=""))
#'  data <- data
#'  
#'  histData <- histData.normz
#'  
#'  
#'  predData_svm2 <- predData_svm_Baseline
#'  predData_svm2 <- subset(
#'    predData_svm2,
#'    X == i,
#'    select = c(Market.Volatility.Index..Level..normz
#'
#'    )
#'  )
#'  
#'  indVars   = c("Market.Volatility.Index..Level..normz"
#'
#'  )
#'  
#'  
#'  startDate = "1991-08-16"
#'  endDate   = "2007-08-16"
#'  
#'  depVar <- c("end_rating")
#'  
#'  pct <- 1
#'  wgt <-  "mCount"
#'  ratingCat <- c("A", "B", "C", "D", "E", "F", "G")
#'  defind    <- "G"
#'  lstCategoricalVars <- c("end_rating")
#'  tuning <- "FALSE"
#'  cost <- 0.01
#'  gamma <- 0.01
#'  cost.weights <-  c(0.01, 0.05, 0.1, 0.25, 10, 50, 100)
#'  gamma.weights <- c(0.01, 0.05, 0.1, 0.25, 10, 50, 100)
#'  kernelType <- "sigmoid"
#'  method    = "cohort"
#'  snapshots = 1
#'  interval  = 1
#'  
#'  
#'  svm_TM <-
#'    transForecast_svm(
#'      data,
#'      histData,
#'      predData_svm2,
#'      startDate,
#'      endDate,
#'      method,
#'      interval,
#'      snapshots,
#'      defind,
#'      depVar,
#'      indVars,
#'      ratingCat,
#'      pct,
#'      tuning,
#'      kernelType,
#'      cost,
#'      cost.weights,
#'      gamma,
#'      gamma.weights
#'    )
#'  print(svm_TM)
#'  
#'}
#'}
transForecast_svm <- function(data, histData, predData_svm, startDate, endDate,  method, interval, snapshots, defind, depVar, indVars,  ratingCat, pct, tuning, kernelType,cost,cost.weights,gamma,gamma.weights) {


  if (snapshots == 1){
    snaps = "years"
  } else if (snapshots == 4){
    snaps = "quarters"
  } else if (snapshots == 12){
    snaps = "months"
  }
  
  #Set parameters
  startDate  <- startDate   
  endDate    <- endDate     
  TotalDateRange <- seq(as.Date(startDate), as.Date(endDate), snaps)
  
  snapshots <- snapshots  
  interval <- interval     
  AnnualBlock <- as.integer(as.numeric(as.Date(endDate)-as.Date(startDate))/365)+1   
  
  lstCnt <-rep(list(list()), AnnualBlock)
  lstInit <-rep(list(list()), AnnualBlock)
  lstPct <-rep(list(list()), AnnualBlock)
  
  #initialize counters
  n <- 1
  k <- 1
  
  #get transition counts and percentages
  for (l in 1:(length(TotalDateRange)-1)){
    
    
    sDate  <- TotalDateRange[l]
    eDate  <- TotalDateRange[l+1]
    Example1<-TransitionProb(data,sDate, eDate, method, snapshots, interval)
    lstCnt[[k]][[n]] <- Example1$sampleTotals$totalsMat #list of quarterly transition counts
    lstInit[[k]][[n]] <- Example1$sampleTotals$totalsVec #list of annual initial counts
    
    
    A<-as.data.frame(lstCnt[[k]][[n]])
    B<-as.data.frame(lstInit[[k]][[n]])
    lstPct[[k]][[n]]   <- A/t(B)
    
    n <- n+1
    if(n>snapshots){
      n <- 1
      k <- k+1
    }
  }
  
  
  
  
   #(3) extract the aggregate transition counts by date and transition type (i.e, AAA to AA,
   #    AAA to A, AAA to BBB, etc...)
   ratingCat <- ratingCat   #c("A","B", "C", "D", "E", "F", "G", "N")
   df <- VecOfTransData(lstCnt,ratingCat,startDate,endDate,snapshots)
   df <- subset(df, df[["start_Rating"]] !=defind) #Notes: remove any record having a default
  

   transData <- expandTransData(df,wgt)
  
   
   depVar <- depVar
   indVars <-indVars
   pct <- pct
   wgt <-  wgt
   ratingCat <- ratingCat
   lstCategoricalVars <- lstCategoricalVars
   tuning <- tuning
   cost <- cost
   gamma <- gamma
   cost.weights <- cost.weights
   gamma.weights <- gamma.weights
   kernelType <- kernelType
   
    
   svm_T<-forecast_svm(transData, histData, predData_svm,  startDate, endDate,
                                   depVar,indVars,  ratingCat, pct, tuning, kernelType,cost, cost.weights,
                                   gamma, gamma.weights) 
   return(svm_T)


}


