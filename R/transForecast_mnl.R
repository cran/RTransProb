#' Forecast - using Multinomial Logistic Regression
#'
#' @description This model implements a forecasting method using multinomial logistic regression (also known as Softmax Regression in machine learning parlance).
#'
#' @usage transForecast_mnl(transData, histData, predData, startDate,
#'                         endDate, ref, depVar, indVars, ratingCat, wgt)
#'
#' @param transData dataframe containing date, beginning ratings, ending ratings, and transition counts.
#' @param histData historical macroeconomic,financial and non-financial data.
#' @param predData forecasting data.
#' @param startDate start date of the estimation time window, in string or numeric format.
#' @param endDate end date of the estimation time window, in string or numeric format.
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
#'
#' #(1) Import Data
#' startDate <-"2000-01-01"
#' endDate   <-"2002-01-01"
#' TotalDateRange <- seq(as.Date(startDate), as.Date(endDate), "years")
#'
#'
#' #(2) Create quarterly transition matrices for the entire date range selected using the normal
#' #   procedures
#'
#' #Set parameters
#' snapshots <- 4   #4,#12    #monthly transition matrices
#' interval <-  1   #1/12    #1 month transition matrices
#'
#' #define list to hold initial counts and transition counts
#' lstCnt <-rep(list(list()), length(TotalDateRange)-1)
#' lstInit <-rep(list(list()), length(TotalDateRange)-1)
#'
#' #initialize counters
#' n <- 1
#' k <- 1
#' for (l in 1:(length(TotalDateRange)-1)){
#'
#'   istartDate = POSIXTomatlab(as.POSIXlt(as.Date(TotalDateRange[l],format = "%Y-%m-%d")))
#'   iendDate = POSIXTomatlab(as.POSIXlt(as.Date(TotalDateRange[l+1],format = "%Y-%m-%d")))
#'   DateRange        <- as.Date(matlabToPOSIX(cfdates(istartDate,iendDate,snapshots)))
#'
#'   for(i in 1:(length(DateRange)-1)){
#'
#'     sDate  <- as.Date(DateRange[i])
#'     eDate    <- as.Date(DateRange[i+1])
#'
#'     Example1<-TransitionProb(data,sDate, eDate, 'cohort', snapshots, interval)
#'
#'     lstCnt[[k]][[n]] <- Example1$sampleTotals$totalsMat      #list of monthly transition counts
#'     lstInit[[k]][[n]]  <- Example1$sampleTotals$totalsVec    #list of monthly initial counts
#'     n <- n+1
#'
#'     if(n>snapshots){
#'       n <- 1
#'       k <- k+1
#'     }
#'
#'   }
#'
#' }
#'
#' #(3) extract the aggregate transition counts by date and transition type (i.e, AAA to AA,
#' #   AAA to A, AAA to BBB, etc...)
#' ratingCat <- c("A","B", "C", "D", "E", "F", "G", "N")
#'
#' df <- VecOfTransData(lstCnt,ratingCat,startDate,endDate,snapshots)
#' df <- subset(df, df[["start_Rating"]] !="N")   #Notes: remove any record having a default
#'                                                #rating in the 'start_Rating'
#' #(4) Construct parameters for MNL model
#' startDate <- as.Date("2000-01-01")
#' endDate <- as.Date("2005-01-01")
#'
#' ref <- "A"   #"AAA"
#' depVar <- c("end_rating")
#' indVars <-c("Macro1", "Financial1","Industry1")
#' wgt <-  "mCount"
#' ratingCat <- c("A","B", "C", "D", "E", "F", "G")  #NOTE: 'N' (Default Rating) is excluded
#'                                                   #(VBA book count data)
#' #(5) run mnl model
#' \dontrun{
#' mnl_T<-transForecast_mnl(df, histData, predData_mnl, startDate, endDate, ref, depVar,
#'                          indVars, ratingCat, wgt)
#' }
#'
#'
transForecast_mnl <- function(transData, histData, predData, startDate, endDate, ref, depVar, indVars, ratingCat, wgt) {

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
  transData$Qtr_Year <- as.Date(zoo::as.yearqtr(transData$Qtr_Year))

  histData$Date  <- as.Date(zoo::as.yearqtr(histData$Date))

  #merge histData and transData
  trainData<-merge(x=transData,y=histData, by.x="Qtr_Year", by.y = "Date")
  trainData <- subset(trainData, trainData$Qtr_Year>=startDate & trainData$Qtr_Year<=endDate)
  trainData <- subset(trainData, trainData[[wgt]] >0)

  trainRows <- sample(1:nrow(trainData), 0.8*nrow(trainData))
  trainData <- trainData[trainRows, ]
  test <- trainData[-trainRows, ]


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
  mnlReg <- nnet::multinom(formula.init, data=trainData, weights=trainData[[wgt]],maxit = 1000)

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

