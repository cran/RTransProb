
#-------------------------------------------------------------------
#' Estimation of credit transition probabilities
#'
#' @description This function is used to estimate transition probabilities and counts given historical credit data (a.k.a., credit migration data).
#'
#' @usage TransitionProb(data, startDate, endDate, method, snapshots, interval)
#'
#' @param data              a table containing historical credit ratings data (i.e., credit migration data). A dataframe of size \emph{nRecords} x 3 where each row contains an ID (column 1), a date (column 2), and a credit rating (column 3); The credit rating is the rating assigned to the corresponding ID on the corresponding date.
#' @param startDate             start date of the estimation time window, in string or numeric format. The default start date is the earliest date in 'data'.
#' @param endDate             end date of the estimation time window, in string or numeric format. The default end date is the latest date in 'data'. The end date cannot be a date before the start date.
#' @param method            estimation algorithm, in string format. Valid values are 'duration'  or 'cohort'.
#' @param snapshots  integer indicating the number of credit-rating snapshots per year to be considered for the estimation. Valid values are 1, 4, or 12. The default value is 1, \emph{i.e., one snapshot per year}. This parameter is only used in the 'cohort' algorithm.
#' @param interval  the length of the transition interval under consideration, in years. The default value is 1, \emph{i.e., 1-year transition probabilities are estimated}.
#'
# @param numericalRating   a table containing mapping of the letter credit rating to a numerical value.
# @param RatingsCat        number of rating categories.
#'
#' @details
#' The two most commonly used methods to estimate credit transition matrices are the cohort (discrete time) and duration (continuous time) methods.
#'
#' Cohort Method (Discrete-time Markov Chains) - The method most commlonly used by rating agencies is the cohort method. Let
#' \eqn{P_{ij} (\Delta t)} be the probability of migrating from grade i to j over a specified time period \eqn{\Delta t}. An
#' estimate of the transition probability of a 1 year horizon where \eqn{\Delta t = 1 year} is thus:
#' \deqn{P_{ij}(\Delta t) = \frac{N_{ij}}{N_{i}}}
#' where \eqn{N_{i}} = number of firms in rating category \eqn{i} at the beginning of the horizon, and \eqn{N_{ij}} = the
#' number of firms that migrated to grade \eqn{j} by horizon-end.
#'
#' It is important to note that any rating change activity which occurs within the period \eqn{\Delta t} is ignored, thus leading to information loss.
#'
#' Duration Method (Continuous-time Markov Chains) - A time homogenous continuous-time Markov chain in a sense uses all of the available information
#' and is specified using a \eqn{(KxK)} generator matrix estimated via the maximum likelihood estimator
#' \deqn{\lambda_{ij} = \frac{N_{ij}(T)}{\int_{T}^{0}Y_i(s)ds}}
#' where \eqn{Y_{i}(s)} is the number of firms in rating class \eqn{i} at time \eqn{s} and \eqn{N_{ij}(T)} is the total
#' number of transitions over the period from \eqn{i} to \eqn{j}, where \eqn{i \neq j}.
#'
#' @export
#'
#' @return Returns the following objects:
#' @return \item{\strong{sampleTotals}}{a list containing the following count components:}
#'
#' @return 	  \item{\emph{totalsVec}}{A vector of size \emph{1-by-nRatings.} For 'duration' calculations, the vector stores the total time spent on \emph{rating i.} For 'cohort' calculations, the vector stores the initial counts (start vector) in \emph{rating i.}}
#' @return 	  \item{\emph{totalsMat}}{A matrix of size \emph{nRatings-by-nRatings.} For 'duration' calculations, the matrix contains the total transitions observed out of \emph{rating i} into \emph{rating j} (all the diagonal elements are zero). For 'cohort' calculations, the matrix contains the total transitions observed from \emph{rating i} to \emph{rating j.}}
#' @return 	  \item{\emph{algorithm}}{A character vector with values 'duration' or 'cohort'.}
#'
#' @return \item{\strong{transMat}}{Matrix of transition probabilities in percent. The size of the transition matrix is \emph{nRatings-by-nRatings.}}
#' @return \item{\strong{genMat}}{Generator Matrix. \emph{use only with duration method}}
#'
#' @author Abdoulaye (Ab) N'Diaye
#'
#' @references
#' Jafry, Y. and Schuermann, T. 2003 Metrics for Comparing Credit Migration Matrices,
#' Wharton Financial Institutions Working Paper 03-08.
#'
#' Lando, D., Skodeberg, T. M. 2002 Analyzing Rating Transitions and Rating Drift with Continuous
#' Observations, Journal of Banking and Finance 26, No. 2-3, 423-444
#'
#' MathWorld.com (2011). Matlab Central \url{http://www.mathworks.com/matlabcentral/}. Mathtools.net \url{http://www.mathtools.net/}.
#'
#' Schuermann, T. and Hanson, S. 2004 Estimating Probabilities of Default,
#' Staff Report No. 190, Federal Reserve Bank of New York,
#'
#' @examples
#'
#' #Example 1:
#' #When start date and end date are not specified, the entire dataset is used and the package
#' #performs TTC calculations. Equally when snapshots and interval are not specified the defaults
#' #are 1.
#' snapshots <- 0
#' interval <- 0
#' startDate  <- 0
#' endDate    <- 0
#' Example1<-TransitionProb(data,startDate,endDate,'cohort', snapshots, interval)
#'
#' \dontrun{
#' #Example 2:
#' #using the duration method the time window of interest are specified 2-year period from the
#' #beginning of 2000 to the beginning of 2002 snapshots and interval are not specified.
#' snapshots <- 0
#' interval <- 0
#' startDate  <- "2000-01-01"
#' endDate    <- "2002-01-01"
#' Example2<-TransitionProb(data,startDate, endDate,'duration', snapshots, interval)
#' }
#'
#' #Example 3:
#' #using the cohort method the time window of interest are specified 5-year period from the
#' #beginning of 2000 to the beginning of 2005 snapshots and interval are not specified.
#' snapshots <- 0
#' interval <- 0
#' startDate  <- "2000-01-01"
#' endDate    <- "2005-01-01"
#' Example3<-TransitionProb(data,startDate, endDate,'cohort', snapshots, interval)
#'
#'
#' #Example 4:
#' #assume that the time window of interest is the 5-year period from the beginning of 2000 to
#' #the beginning of 2005. We want to estimate 1-year transition probabilities using quarterly
#' #snapshots using cohort method.
#' snapshots <- 4    #This uses quarterly transition matrices
#' interval <- 1    #This gives a 1 year transition matrix
#' startDate  <- "2000-01-01"
#' endDate    <- "2005-01-01"
#' Example4<-TransitionProb(data,startDate, endDate,'cohort', snapshots, interval)
#'
#'
#' #Example 5:
#' #assume that the time window of interest is the 5-year period from the beginning of 2000 to
#' #the beginning of 2005. We want to estimate a 2-year transition probabilities using quarterly
#' #snapshots using cohort method.
#' snapshots <- 4     #This uses quarterly transition matrices
#' interval <- 2     #This gives a 2 years transition matrix
#' startDate  <- "2000-01-01"
#' endDate    <- "2005-01-01"
#' Example5<-TransitionProb(data,startDate, endDate,'cohort', snapshots, interval)
#'
#' \dontrun{
#' #Example 6:
#' #assume that the time window of interest is the 2-year period from the beginning of 2000 to
#' #the beginning of 2005. We want to estimate 1-year transition probabilities using quarterly
#' #snapshots using duration method.
#' snapshots <- 4    #This uses quarterly transition matrices
#' interval <- 1    #This gives a 1 year transition matrix
#' startDate  <- "2000-01-01"
#' endDate    <- "2002-01-01"
#' Example6<-TransitionProb(data,startDate, endDate,'duration', snapshots, interval)
#' }
#'
#' #Example 7:
#' #assume that the time window of interest is the 5-year period from the beginning of 2000 to
#' #the beginning of 2005. We want to estimate 1-year transition probabilities using monthly
#' #snapshots using cohort method.
#' snapshots <- 12    #This uses monthly transition matrices
#' interval <- 1    #This gives a 1 year transition matrix
#' startDate  <- "2000-01-01"
#' endDate    <- "2005-01-01"
#' Example7<-TransitionProb(data,startDate, endDate,'cohort', snapshots, interval)
#'
#'
#' #Example 8:
#' #assume that the time window of interest is the 5-year period from the beginning of 2000 to
#' #the beginning of 2005. We want to estimate 1-year transition probabilities using annual
#' #snapshots using cohort method.
#' snapshots <- 1    #This uses annual transition matrices
#' interval <- 1    #This gives a 1 year transition matrix
#' startDate  <- "2000-01-01"
#' endDate    <- "2005-01-01"
#' Example8<-TransitionProb(data,startDate, endDate,'cohort', snapshots, interval)
#'
#'
TransitionProb <- function(data, startDate, endDate, method, snapshots, interval ){

  options(warn=-1)
  data_names <- names(data)

  # if(!isTRUE('ID' %in% data_names)){
  #     stop("Error: Invalid input data. The field 'ID' is missing")
  # } else if (!isTRUE('mDate' %in% data_names)) {
  #     stop("Error: Invalid input data. The field 'mDate' is missing")
  # } else if (!isTRUE('Ratings' %in% data_names)) {
  #     stop("Error: Invalid input data. The field 'Ratings' is missing")
  # } else if (!isTRUE('Num_Ratings' %in% data_names)){
  #     stop("Error: Invalid input data. The field 'Num_Ratings' is missing")
  # }

  validSnapShots <- c(0,1,4,12)
  if (!isTRUE(snapshots %in% validSnapShots)){
    stop("Error: Invalid Snapshots Per Year. Valid values are 1, 4, or 12")
  }

  if (!(is.numeric(interval)) || interval < 0){
    stop("Error: Invalid interval values. Enter positive values greater than 0")
  }

  validMethod <- c('cohort','duration')
  if (!isTRUE(method %in% validMethod)){
    stop("Error: Invalid Method. Valid methods are  either 'cohort' or 'duration'")
  }

  data <- transform(data,id2=seq(1:nrow(data)))
  keepVars   <-  c("id2","ID")
  data_abridge <- data[keepVars]


  StartPos    <- data_abridge[ !duplicated(data_abridge$ID), ]
  StartPos    <- StartPos[c("id2")]

  numericalRating = data$Num_Ratings
  labels2 = sort(as.character(unique(data$Rating)));
  RatingsCat = length(labels2)

  numDate = POSIXTomatlab(as.POSIXlt(as.Date(data$Date,format = "%m/%d/%Y")));  #Convert dates to POSIXITlt date format and then consequently to Matlab datenum dates


  if (snapshots==0){
    snapshots <- 1
    interval <- 1
  }

  if (startDate == 0){
    sDate = min(as.Date(data$Date,format = "%m/%d/%Y"))
    sDate = POSIXTomatlab(as.POSIXlt(sDate))
  } else {
    sDate = POSIXTomatlab(as.POSIXlt(as.Date(startDate,format = "%Y-%m-%d")))
  }

  if (endDate == 0){
    eDate = max(as.Date(data$Date,format = "%m/%d/%Y"))
    eDate = POSIXTomatlab(as.POSIXlt(eDate))
  } else {
    eDate = POSIXTomatlab(as.POSIXlt(as.Date(endDate,format = "%Y-%m-%d")))
  }

  if (sDate>eDate){
    stop("Error: Start Date is Greater Than End Date")
  }


  # Get totals per ID
  if (method=="duration"){
    idTotCnt = getidTotCntDuration(numDate,numericalRating,StartPos,sDate,
                                   eDate,RatingsCat)
  }else if (method=="cohort") {
    idTotCnt = getidTotCntCohort(numDate,numericalRating,StartPos,sDate,
                                 eDate,RatingsCat,snapshots)
  }


  # Get transition probabilities and sample totals
  transMat_sampleTotals <- transitionprobbytotals(idTotCnt,snapshots,interval,method)


  return(transMat_sampleTotals)
}

