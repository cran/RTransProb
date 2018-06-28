#' Cohort - Data Weighting and "TTC" Calculation
#'
#' @description Calculate \emph{Through-the-Cycle} transition matrices using the \emph{cohort method} transitions.
#'
#' @usage cohort.TTC(transCount, initCount)
#'
#' @param transCount transitions counts for each time-step
#' @param initCount start vector counts for each time-step
#'
#'
#' @return  \item{SAT}{\emph{Scaled Average Transitions} - compute a TTC transition matrix by first scaling and weighting the counts (start vector counts and transition counts) then calculate the transition matrices for each time-step, and finally averaging over all available time-steps. e.g., average January matrices, then February matrices or average Q1, then Q2 ...then obtain the average of the transition matrices}
#' @return  \item{SAPT}{\emph{Scaled Average Periodic Transitions} - compute a TTC transition matrix by weighting the transition percentages for each time-step (calculate the transition matrices for each time-step then weigh the percentages, and finally averaging over all available time-steps. e.g., average January matrices, then February matrices or average Q1, then Q2 ...then obtain the average of the transition matrices}
#' @return  \item{USAT}{\emph{Unscaled Average Transitions} - compute a TTC transition matrix by first obtaining unscaled transition matrices for each time-step then averaging over all available time-steps}
#' @return  \item{ATMP}{\emph{averageTransMatByPeriod} - returns the weighted the transition percentages for each time-step (calculate the transition matrices for each time-step then weigh the percentages}
#' @return  \item{ATP}{\emph{averageTransByPeriod} - returns the scaled transitions for each time-step}
#' @return  \item{ACP}{\emph{averageCountByPeriod} - returns the scaled start vector counts for each time-step}
#' 
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
#' 	\item Scale the number of transitions and firm counts using the a single year count to preserve dynamics, then average transitions and firms counts separately
#' 	\item Estimate the single-year quantities (estimate with transition matrices for each time-step), then average across years
#' 	\item Average annual transition matrices
#' }
#' The Markov property allows for direct weighting as each time-step can be regarded as distinct(independence).
#'
#' @examples
#'
#' \dontrun{
#' 
#' #Set parameters
#' startDate  <- "2000-01-01"
#' endDate    <- "2005-01-01"
#' method       <- "cohort"   
#' snapshots <- 4  
#' interval <-  .25
#' Example<-getPIT(data,startDate, endDate,method, snapshots, interval)
#' 
#' lstInit <- Example$lstInitVec[lapply(Example$lstInitVec,length)>0]
#' lstCnt <- Example$lstCntMat[lapply(Example$lstCntMat,length)>0]
#' ExampleTTC <- cohort.TTC(lstCnt,lstInit)
#'  
#' }

cohort.TTC <- function(transCount, initCount) {
  snapshots <-  gHorizon <- length(transCount[[1]])
  gYear <- length(lapply(transCount, length))
  
  validCount <- c(1, 4, 12)
  if (!isTRUE(length(transCount[[1]]) %in% validCount)) {
    stop(
      "Error: when calculating cohort through-the-cycle transition rates,'transCount' must contain transition count matrices for each time-step of the estimation window."
    )
  }
  
  # validMethod <- c("APTA", "GAPTA","RDAA","AAT")
  # if (!isTRUE(transtype %in% validMethod)){
  #   stop("Error: Invalid Averaging Method. Valid averaging methods are listed in the documentation")
  # }
  
  
  if (gYear < 1) {
    stop("Error: Invalid number of years. Time-step must be greater than 0")
  }
  
  
  # validHorizon <- c(1,2,3,4,6,12)
  # if (!isTRUE(snapshots %in% validHorizon)){
  #   stop("Error: Invalid horizon. Valid horizon numbers are 6 or 12")
  # }
  
  
  #Check the lists to see if the transition count matrix and start vector counts are valid
  for (k in 1:length(transCount)) {
    for (n in 1:length(transCount[[k]])) {
      cm.matrix.counts(transCount[[k]][[n]])
      cm.vector.counts(initCount[[k]][[n]])
      
    }
  }
  
  nStates_row <- nrow(as.data.frame(transCount[[1]][1]))
  nStates_col <- ncol(as.data.frame(transCount[[1]][1]))
  
  if (nStates_row == nStates_col) {
    nStates <- nStates_row
  } else{
    stop("Error: Transition matrix rows and columns must be equal")
  }
  
  if (nStates < 2 || nStates > 25) {
    stop(
      "Error: Invalid Number of 'Risk States'. Valid 'Number of Risk States' are between 2 and 25"
    )
  }
  
  
  
  averageTrans <- list()
  averagePeriodicTrans <- list()
  averagePeriodicTransGM <- list()
  averageCount <- list()
  glst_initCount <- list()
  glst_periodCount <- c()
  lst_mPeriod <- list()
  lst_mPeriod_cnt <- list()
  lst_averageTrans <- list()
  lst_initCount <- list()
  lst_averageCount <- list()
  mPeriod <- list()
  mPeriod_cnt <- list()
  mAnnual <- list()
  periodCount <- c()
  transAnnualMat <- list()
  scaler <- c()
  
  
  #Year Weights
  transWeight <- pracma::repmat(1 / gYear, gYear, 1)
  
  
  # Counts per time-step (periodCount)
  # Create nState x nState matrix with rows having state counts for that period/year combination (initCount)
  #******Basically get the total counts for each period
  for (jj in 1:gYear) {
    for (ii in 1:length(transCount[[jj]])) {
      initCount[[jj]][[ii]] <-
        pracma::repmat(matrix(initCount[[jj]][[ii]]), 1, nStates)
      periodCount[[ii]]      <- sum(initCount[[jj]][[ii]][, 1])
      gc()
    }
    
    glst_periodCount[[jj]] <- periodCount
    
  }
  
  # *****get the maximum 'total count per time-step'
  # Scalar based on maximun number of observations during given time-step
  for (i in 1:snapshots) {
    scales <- matrix(unlist(glst_periodCount), snapshots)
    scaler[i] <- max(scales[i, ])
    
  }
  
  
  
  for (m in 1:snapshots) {
    for (y in 1:gYear) {
      if (m <= length(transCount[[y]])) {
        #Section 1
        # Scale the counts (start vector counts and transition counts) then calculate transition matrices for each time-step
        # Calculate average initial (start vector) and transition counts using the scalar for each time-step ("raw" data method)
        if (y == 1) {
          #Section 1.a
          # (scale the weights of the transition counts)
          averageTrans[[m]] <-
            transWeight[y] * scaler[m] / glst_periodCount[[y]][[m]] * (transCount[[y]][[m]])
          #Section 1.b
          # (scale the weights of the Initial counts (start vector) )
          averageCount[[m]] <-
            transWeight[y] * scaler[m] / glst_periodCount[[y]][[m]] * (initCount[[y]][[m]])
          
        } else{
          #Section 1.a
          # (scale the weights of the transition counts and compound the scaled transition counts)
          averageTrans[[m]] <-
            averageTrans[[m]] + transWeight[y] * scaler[m] / glst_periodCount[[y]][[m]] *
            (transCount[[y]][[m]])
          #Section 1.b
          # (scale the weights of the initial counts (start vector) and compound the scaled initial counts (start vector) )
          averageCount[[m]] <-
            averageCount[[m]] + transWeight[y] * scaler[m] / glst_periodCount[[y]][[m]] *
            (initCount[[y]][[m]])
        }
        
        #Section 2
        # Scale the transition percentages (calculate the transition matrices for each time-step then scale the percentages)
        # Calculate transition matrices for each time-step using the averaged raw data
        if (y == 1) {
          #Section 2.a
          # (equally weight the each transition probability )
          averagePeriodicTrans[[m]] <-
            transWeight[y] * (transCount[[y]][[m]]) / (initCount[[y]][[m]])
          #Section 2.b
          # (use the scaled and weighted initial counts (start vector)  in section 1.b to calculate transition probability)
          averagePeriodicTransGM[[m]] <-
            ((transCount[[y]][[m]]) / (initCount[[y]][[m]])) ^ (1 / gYear)
          
        } else{
          #Section 2.a
          # (equally weight the each transition probability )
          averagePeriodicTrans[[m]] <-
            averagePeriodicTrans[[m]] + transWeight[y] * (transCount[[y]][[m]]) / (initCount[[y]][[m]])
          #Section 2.b
          # (use the scaled and weighted initial counts (start vector) in section 1.b to calculate transition probability)
          averagePeriodicTransGM[[m]] <-
            averagePeriodicTransGM[[m]] * ((transCount[[y]][[m]]) / (initCount[[y]][[m]])) ^
            (1 / gYear)
        }
        
      }
    }
    
    #Section 2.b.1 (returns the average transition matrices (%)  for each time-step i.e, average per month, average per quarter or average per year)
    # ----------Scale geometric average transition matrix to have row sum equal to 1-----------------
    # (correct the geometric average transition matrix to make sure the row sums equal to 1)
    averagePeriodicTransGM[[m]] <-
      averagePeriodicTransGM[[m]] / pracma::repmat(matrix(rowSums(averagePeriodicTransGM[[m]], 2)), 1, nStates)
    
    
    
    #Section 3
    # Annual transition matrices for the two methods
    # averagePeriodicTransAnnual = Averaged periodic transition matrices
    # averageDataTransAnnual = Periodic transition matrices using averaged data (raw method)
    if (m == 1) {
      #Section 3.a
      #Store the FIRST average transition matrices for each time-step in the variable 'averagePeriodicTransAnnual'
      averagePeriodicTransAnnual   <- averagePeriodicTrans[[m]]
      
      #Section 3.b
      #Store the FIRST average transition matrices for each time-step in the variable 'averagePeriodicTransAnnualGM'
      averagePeriodicTransAnnualGM <- averagePeriodicTransGM[[m]]
      
      #Section 3.c
      #Calculate the FIRST transition matrices for each time-step
      averageDataTransAnnual      <-
        averageTrans[[m]] / averageCount[[m]]
      
    } else {
      #Section 3.a
      #Store the FIRST average transition matrices for each time-step in the variable 'averagePeriodicTransAnnual'
      averagePeriodicTransAnnual   <-
        averagePeriodicTransAnnual %*% averagePeriodicTrans[[m]]
      
      #Section 3.b
      #Store the FIRST average transition matrices for each time-step in the variable 'averagePeriodicTransAnnualGM'
      averagePeriodicTransAnnualGM <-
        averagePeriodicTransAnnualGM %*% averagePeriodicTransGM[[m]]
      
      
      #-------------------------------------Fixing the square matrix---------------------------------------
      dn      <- (averageTrans[[m]] / averageCount[[m]])
      #----------------------------------------------------------------------------------------------------
      averageDataTransAnnual      <- averageDataTransAnnual %*% dn
      
    }
    
  }
  
  
  
  #average of annual transition matrices
  for (y in 1:gYear) {
    #Annual transition matrices
    for (m in 1:length(transCount[[y]])) {
      if (m == 1) {
        transAnnualMat[[y]] <- transCount[[y]][[m]] / initCount[[y]][[m]]
        
      } else {
        transAnnualMat[[y]] <- transAnnualMat[[y]] %*% dn
        
      }
      
    }
    
    
    # Average annual transition matrices
    if (y == 1) {
      transAnnual <- transWeight[[y]] * transAnnualMat[[y]]
      
    } else {
      transAnnual <- transAnnual + transWeight[[y]] * transAnnualMat[[y]]
      
    }
    
    
  }
  
  
  
  outPut  <- list(
    SAT = averageDataTransAnnual,
    SAPT = averagePeriodicTransAnnual,
    USAT = transAnnual,
    ATMP = averagePeriodicTrans,
    ATP = averageTrans,
    ACP = averageCount
  )
  
  return(outPut)
  
  
  
}

