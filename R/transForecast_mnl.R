#' Forecast - using Multinomial Logistic Regression
#'
#' @description This model implements a forecasting method using multinomial logistic regression (also known as Softmax Regression in machine learning parlance).
#'
#' @usage transForecast_mnl(data, histData, predData_mnl, startDate, endDate, method, 
#'                          interval,snapshots, defind,ref, depVar, indVars, ratingCat, wgt)
#'
#' @usage transForecast_mnl()
#'
#' @param data a table containing historical credit ratings data (i.e., credit migration data). A dataframe of size \emph{nRecords} x 3 where each row contains an ID (column 1), a date (column 2), and a credit rating (column 3); The credit rating is the rating assigned to the corresponding ID on the corresponding date.
#' @param histData historical macroeconomic,financial and non-financial data.
#' @param predData_mnl forecasting data.
#' @param startDate start date of the estimation time window, in string or numeric format.
#' @param endDate end date of the estimation time window, in string or numeric format.
#' @param method  estimation algorithm, in string format. Valid values are 'duration'  or 'cohort'.
#' @param interval  the length of the transition interval under consideration, in years. The default value is 1, \emph{i.e., 1-year transition probabilities are estimated}.
#' @param snapshots  integer indicating the number of credit-rating snapshots per year to be considered for the estimation. Valid values are 1, 4, or 12. The default value is 1, \emph{i.e., one snapshot per year}. This parameter is only used in the 'cohort' algorithm.
#' @param defind Default Indicator
#' @param ref  base or reference category for the dependent variable.
#' @param depVar dependent variable, as a string.
#' @param indVars list containing the independent variables
#' @param ratingCat list containing the unique rating categories
#' @param wgt weights
#'
#' @return The output consists of a forecasted transition matrix.
#'
#' @details
#' Multinomial logistic regression is a simple extension of binary logistic regression that allows for more than two categories of
#' the dependent or outcome variable.  Whereas, a binary logistic regression model compares one dichotomy, the multinomial logistic
#' regression model compares a number of dichotomies. Like binary logistic regression, multinomial logistic regression uses maximum
#' likelihood estimation to evaluate the probability of categorical membership.
#'
#' Assume there are 1,2,3 ...K groups in a dataset, and group 1 is the one chosen as the reference category. The logistic model states
#' that the probability of falling into group j given the set of predictor values x is given by the general expression
#' \deqn{P(y=k|\emph{X}) = \frac{exp(\emph{X}\beta_{k})}{1+\sum_{j=2}^{N}exp(\emph{X}\beta_{j})}}
#'
#'
#' @export
#'
#' @author  Abdoulaye (Ab) N'Diaye
#'
#'
#' @examples
#' \dontrun{
#' mnl_TM<-transForecast_mnl(data, histData, predData_mnl, startDate, endDate, method, interval,  
#'                          snapshots, defind, ref, depVar,indVars, ratingCat, wgt)
#' }
#'
#'
transForecast_mnl <- function (data,histData, predData_mnl, startDate, endDate, method, interval, snapshots, defind, ref,depVar,indVars, ratingCat, wgt){
                                  
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


    if(l==85){
      s="l"
    }
      
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
  
  
  # -------------------------------- Multinomial Logistic -----------------------------------------------------------
  
  startDate <-as.Date(startDate)      
  endDate <-  as.Date(endDate)        
  
  
  ratingCat <- ratingCat              
  transition_data_compressed_m <- VecOfTransData(lstCnt,ratingCat,startDate,endDate,snapshots)
  
  
  #3) Construct parameters for MNL model
  transition_data_compressed_m <- transition_data_compressed_m[ which(transition_data_compressed_m$start_Rating!=tail(ratingCat, n=1)), ]
  
  
  ref <- ref   #"A"
  depVar <- depVar   #c("end_rating")
  indVars <- indVars #c("Macro1", "Financial1","Industry1")
  wgt <-  "mCount"
  ratingCat <- ratingCat[ratingCat != defind]    
  histData  <-histData       #historical macro variables
  predData_mnl<-predData_mnl     #prediction data along with the dummy values for the buckets
  
  
  #4) run mnl model
  mnl_T<-forecast_mnl(transition_data_compressed_m, histData, predData_mnl, startDate, endDate, ref, depVar, indVars, ratingCat, wgt)
  
  return (mnl_T)
}



