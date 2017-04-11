#' Cohort - Data Weighting and "TTC" Calculation
#'
#' @description Calculate \emph{Through-the-Cycle} transition rates using raw transition counts and starting obligor counts using the \emph{cohort method}.
#'
#' @usage cohort.TTC(transCount, initCount, gYear, gHorizon, transtype)
#'
#' @param transCount monthly "raw" data transitions
#' @param initCount monthly "raw" data counts
#' @param gYear number of relevant years
#' @param gHorizon number of months in the horizon
#' @param transtype averaging method. The valid values
#' \itemize{
#' \item{\emph{averageDataTransAnnual}}{ - "Raw" Data Average}
#' \item{\emph{averageMonthlyTransAnnual}}{ - Average of Monthly Transitions}
#' \item{\emph{transAnnual}}{ - Average of Annual Transitions}
#' \item{\emph{averageMonthlyTransAnnualGM}}{ - Geometric Average of Monthly Transitions}
#'}
#'
#'
#' @return One of the output options corresponding to the 'transtype'value selected as input.
#' @return  \item{averageDataTransAnnual}{\emph{"Raw" Data Average} - weighted average of separately scaled transition and obligor counts.}
#' @return  \item{averageMonthlyTransAnnual}{\emph{Average Monthly Transitions} - compute monthly transition matrices then average over years, e.g., average January matrices, then February matrices,...}
#' @return  \item{transAnnual}{\emph{Average Annual Transitions} - compute annual transition matrices then average over years}
#' @return  \item{averageMonthlyTransAnnualGM}{\emph{Geometric Average Monthly Transitions} - similar to Average Monthly Transitions except the geometric average of the monthly transition matrices, on an element-byelement basis, is used}
#'
#' @export
#'
#' @author  Abdoulaye (Ab) N'Diaye
#'
#' @details
#'
#' Many credit risk models require a \emph{long-run average} (Through-the-Cycle) PD estimate. This has been
#' interpreted as meaning the data from multiple years should be combined and the method capable of supporting
#' some form of weighting of  samples.
#'
#' The three methods of weighting considered for data generated via the cohort method are:
#' \enumerate{
#' 	\item Scale the number of transitions and firm counts/years using the a single year count to preserve dynamics, then average transitions and firms counts/years separately
#' 	\item Estimate the single-year quantities (estimate with monthly transition matrices), then average across years
#' 	\item Average annual transition matrices
#' }
#' The Markov property allows for direct weighting as each year can be regarded as distinct.
#'
#' @examples
#'
#' \dontrun{
#' #Get a sequence of years from "2000-01-01" to "2005-01-01"
#' TotalDateRange <- seq(as.Date("2000-01-01"), as.Date("2005-01-01"), "mon")
#' TotalDateRange_Yr <- seq(as.Date("2000-01-01"), as.Date("2005-01-01"), "years")
#'
#' #Set parameters
#' snapshots <- 12    #monthly transition matrices
#' interval <- 1/12    #1 month transition matrices
#'
#' #define list to hold initial counts and transition counts
#' lstCnt <-rep(list(list()), length(TotalDateRange_Yr))
#' lstInit <-rep(list(list()), length(TotalDateRange_Yr))
#'
#' #initialize counters
#' nn <- 1
#' k <- 1
#' kk <- 1
#'
#' #Create transition and initial counts (start vector) used as inputs to the function
#' for (l in 1:(length(TotalDateRange)-1)){
#'
#'   istartDate = POSIXTomatlab(as.POSIXlt(as.Date(TotalDateRange[l],format = "%Y-%m-%d")))
#'   iendDate = POSIXTomatlab(as.POSIXlt(as.Date(TotalDateRange[l+1],format = "%Y-%m-%d")))
#'   DateRange        <- as.Date(matlabToPOSIX(cfdates(istartDate,iendDate,snapshots)))
#'
#'   for(i in 1:(length(DateRange)-1)){
#'
#'     sDate  <- DateRange[i]
#'     eDate    <- DateRange[i+1]
#'
#'     Sample1<-TransitionProb(data,sDate, eDate, 'cohort', snapshots, interval)
#'
#'     lstCnt[[k]][[kk]] <- Sample1$sampleTotals$totalsMat
#'     lstInit[[k]][[kk]]  <- Sample1$sampleTotals$totalsVec
#'     #print(lstCnt)
#'
#'     if(kk>=snapshots){
#'       kk <- 1
#'       k <- k+1
#'     } else {
#'       kk <- kk+1
#'     }
#'
#'   }
#'
#' }
#'
#'
#'
#' #average of monthly transition matrices using the monthly initial portfolio
#' #counts  (start vector) and transition counts.
#'
#' #remove empty elements from the lists (lstCnt and lstInit)
#' lstInit <- lstInit[lapply(lstInit,length)>0]
#' lstCnt <- lstCnt[lapply(lstCnt,length)>0]
#'
#' transtype <- "AverageMonthlyTransitions"   #averaging method
#' gYear <- length(lapply(lstCnt,length))     #Add Error Checking for this
#' gHorizon <- snapshots                      #number of months in the horizon (default= 12)
#'
#' transMatTest <- cohort.TTC(lstCnt,lstInit,gYear,gHorizon,transtype)
#' }

cohort.TTC <- function(transCount, initCount, gYear, gHorizon, transtype){


  if(length(transCount[[1]])!=12){
    stop("Error: when calculating cohort through-the-cycle transition rates,'transCount' must contain 12 monthly transition count matrices for each year")
  }

  validMethod <- c("AverageMonthlyTransitionsAnnual", "GeometricAverageMonthlyTransitionsAnnual","RawDataAverageAnnual","AverageAnnualTransitions","AverageMonthlyTransitions","AverageMonthlyTransitionsGM")
  if (!isTRUE(transtype %in% validMethod)){
    stop("Error: Invalid Averaging Method. Valid averaging methods are listed in the documentation")
  }


  if (gYear < 1){
    stop("Error: Invalid number of years. Number of years must be greater than 0")
  }


  validHorizon <- c(1,2,3,4,6,12)
  if (!isTRUE(gHorizon %in% validHorizon)){
    stop("Error: Invalid horizon. Valid horizon numbers are 6 or 12")
  }


  #Check the lists to see if the count matrix and initial counts (start vector) are valid
  for (k in 1:length(transCount)){
    for(n in 1:length(transCount[[1]])){

      cm.matrix.counts(transCount[[k]][[n]])
      cm.vector.counts(initCount[[k]][[n]])

    }
  }

  nStates_row <- nrow(as.data.frame(transCount[[1]][1]))
  nStates_col <- ncol(as.data.frame(transCount[[1]][1]))

  if(nStates_row ==nStates_col){
    nStates <- nStates_row
  } else{
    stop("Error: Transition matrix rows and columns must be equal")
  }

  if (nStates < 2 || nStates > 25){
    stop("Error: Invalid Number of 'Risk States'. Valid 'Number of Risk States' are between 2 and 25")
  }



  averageTrans<- list()
  averageMonthlyTrans <-list()
  averageMonthlyTransGM <-list()
  averageCount <- list()
  glst_initCount <-list()
  glst_monthCount<-c()
  lst_mMonth <- list()
  lst_mMonth_cnt<- list()
  lst_averageTrans<- list()
  lst_initCount <- list()
  lst_averageCount <- list()
  mMonth <- list()
  mMonth_cnt<- list()
  mAnnual <- list()
  monthCount<-c()
  transAnnualMat <- list()
  scaler <- c()


  #Year Weights
  transWeight <- pracma::repmat(1/length(gYear),length(gYear),1)


  # Monthly counts by year (monthCount)
  # Create nState x nState matrix with rows having state counts for that month/year combination (initCount)
  #******Basically get the total counts for each month
  for (jj in 1:length(gYear)){

    for (ii in 1:gHorizon){
      initCount[[jj]][[ii]] <- pracma::repmat(matrix(initCount[[jj]][[ii]]),1,nStates)
      monthCount[[ii]]      <- sum(initCount[[jj]][[ii]][,1])
      gc()
    }

    glst_monthCount[[jj]] <- monthCount

  }

  # *****get the maximum 'total monthly count'
  # Monthly scalar based on maximun number of observations in given month
  for (i in 1:gHorizon){

    t <- matrix(unlist(glst_monthCount),gHorizon)
    scaler[i] <- max(t[i,])


  }


  # Monthly "raw" data average monthly transitions and counts and average monthly transition matrices
  for (m in 1:gHorizon){
    for (y in 1:length(gYear)){

      #Section 1
      # Calculate average initial (start vector) and transition counts using monthly scalar ("raw" data method)
      if (y==1){
        #Section 1.a
        # (scale the weights of the transition counts)
        averageTrans[[m]]<- transWeight[y]*scaler[m]/glst_monthCount[[y]][[m]]*(transCount[[y]][[m]])
        #Section 1.b
        # (scale the weights of the Initial counts (start vector) )
        averageCount[[m]] <- transWeight[y]*scaler[m]/glst_monthCount[[y]][[m]]*(initCount[[y]][[m]])

      }else{
        #Section 1.a
        # (scale the weights of the transition counts and compound the scaled transition counts)
        averageTrans[[m]]<- averageTrans[[m]] + transWeight[y]*scaler[m]/glst_monthCount[[y]][[m]]*(transCount[[y]][[m]])
        #Section 1.b
        # (scale the weights of the initial counts (start vector) and compound the scaled initial counts (start vector) )
        averageCount[[m]] <- averageCount[[m]] + transWeight[y]*scaler[m]/glst_monthCount[[y]][[m]]*(initCount[[y]][[m]])
      }

      #Section 2
      # Calculate monthly transition matrices using the averaged raw data
      if (y==1){
        #Section 2.a
        # (equally weight the each transition probability )
        averageMonthlyTrans[[m]]<- transWeight[y]*(transCount[[y]][[m]])/(initCount[[y]][[m]])
        #Section 2.b
        # (use the scaled and weighted initial counts (start vector)  in section 1.b to calculate transition probability)
        averageMonthlyTransGM[[m]] <- ((transCount[[y]][[m]])/(initCount[[y]][[m]]))^(1/length(gYear))

      }else{
        #Section 2.a
        # (equally weight the each transition probability )
        averageMonthlyTrans[[m]]<- averageMonthlyTrans[[m]] + transWeight[y]*(transCount[[y]][[m]])/(initCount[[y]][[m]])
        #Section 2.b
        # (use the scaled and weighted initial counts (start vector) in section 1.b to calculate transition probability)
        averageMonthlyTransGM[[m]] <- averageMonthlyTransGM[[m]] * ((transCount[[y]][[m]])/(initCount[[y]][[m]]))^(1/length(gYear))
      }


    }

    #Section 2.b.1
    # ----------Scale geometric average transition matrix to have row sum equal to 1-----------------
    # (correct the geometric average transition matrix to make sure the row sums equal to 1)
    averageMonthlyTransGM[[m]] <- averageMonthlyTransGM[[m]]/pracma::repmat(matrix(rowSums(averageMonthlyTransGM[[m]],2)),1,nStates)



    #Section 3
    # Annual transition matrices for the two monthly methods
    # averageMonthlyTransAnnual = Averaged monthly transition matrices
    # averageDataTransAnnual = Monthly transition matrices using averaged data (raw method)
    if (m==1){
      #Section 3.a
      #Store the FIRST average monthly transition matrices in the variable 'averageMonthlyTransAnnual'
      averageMonthlyTransAnnual   <- averageMonthlyTrans[[m]]

      #Section 3.b
      #Store the FIRST average monthly transition matrices in the variable 'averageMonthlyTransAnnualGM'
      averageMonthlyTransAnnualGM <- averageMonthlyTransGM[[m]]

      #Section 3.c
      #Calculate the FIRST monthly transition matrices
      averageDataTransAnnual      <- averageTrans[[m]]/averageCount[[m]]

    } else {

      #-------------------------------------Fixing the square matrix---------------------------------------
      dn      <- (averageTrans[[m]]/averageCount[[m]])
      #----------------------------------------------------------------------------------------------------

      #Section 3.a
      #Store the FIRST average monthly transition matrices in the variable 'averageMonthlyTransAnnual'
      averageMonthlyTransAnnual   <- averageMonthlyTransAnnual %*% averageMonthlyTrans[[m]]

      #Section 3.b
      #Store the FIRST average monthly transition matrices in the variable 'averageMonthlyTransAnnualGM'
      averageMonthlyTransAnnualGM <- averageMonthlyTransAnnualGM %*% averageMonthlyTransGM[[m]]


      averageDataTransAnnual      <- averageDataTransAnnual %*% dn

    }

  }



  #average of annual transition matrices
  for (y in 1:length(gYear)){

    #Annual transition matrices
    for (m in 1:gHorizon){

      if (m ==1){

        transAnnualMat[[y]] <- transCount[[y]][[m]]/initCount[[y]][[m]]

      } else {

        transAnnualMat[[y]] <- transAnnualMat[[y]] %*% dn

      }

    }


    # Average annual transition matrices
    if(y==1){

      transAnnual <- transWeight[[y]] * transAnnualMat[[y]]

    } else {

      transAnnual <- transAnnual + transWeight[[y]] * transAnnualMat[[y]]

    }


  }


  if (transtype=="AverageMonthlyTransitionsAnnual"){
    return(averageMonthlyTransAnnual)
  }  else if (transtype=="GeometricAverageMonthlyTransitionsAnnual"){
    return(averageMonthlyTransAnnualGM)
  }  else if (transtype=="RawDataAverageAnnual"){
    return(averageDataTransAnnual)
  }  else if (transtype=="AverageAnnualTransitions"){
    return (transAnnual)
  }  else if (transtype=="AverageMonthlyTransitions"){
    return (averageMonthlyTrans)
  }  else if (transtype=="AverageMonthlyTransitionsGM"){
    return (averageMonthlyTransGM)
  }


}

