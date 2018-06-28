#' Bootstrapped confidence intervals - Duration
#'
#' @description estimate confidence intervals for the transition probabilities using a bootstrapping procedure for duration method
#'
#' @usage duration.CI(genMat,portWgts,nHorizon,sim)
#'
#' @param genMat generator matrix
#' @param portWgts list containing weights of each rating class
#' @param nHorizon horizon
#' @param sim number of simulations
#'
#' @details
#' The general idea of bootstrapping is to use resampling methods to estimate features of the sampling distribution of
#' an estimator, especially in situations where asymptotic approximations may provide poor results. In the case of a
#' parametric bootstrap method one samples from the estimated distribution derived using maximum likelihood estimation.
#' In summary,
#'
#' \enumerate{
#' \item Estimate the distribution from the observed sample using maximum likelihood
#' \item Draw samples from the estimated distribution
#' \item Calculate the parameter of interest from each of the samples
#' \item Construct an empirical distribution for the parameter of interest
#' \item Select percentiles from the empirical distribution
#' }
#'
#' One can contrast this method with a \emph{nonparametric bootstrap} in which one samples with replacement from the
#' empirical cumulative distribution function of the observed sample.
#'
#' A parametric bootstrapping method is employed for the time-homogeneous continuous-time Markov model.  The elements of the
#' infinitesimal generator matrix, provide most of the information one needs to perform the parametric bootstrap. The outline of the
#' bootstrapping is provided below.
#'
#' For each obligor in a given assigned credit grade:
#' \enumerate{
#' \item Start by drawing a (sojourn) time from the exponential distribution with parameter, \eqn{-\hat{\lambda}_{kk}}
#' \item If the time is greater than or equal to the time left to horizon then stop
#' \item If the time is less than the time left to horizon
#'  \itemize{
#'     \item Draw from the multinomial distribution associated with the possible transition states using the vector of probabilities
#'     \item Determine the state to which the obligor moves, for example, \eqn{i}
#'     \item Repeat the process in 1. now using the diagonal element, \eqn{-\hat{\lambda}_{ii}}
#'     \item Continue until the sampled time exceeds the time to horizon
#' }
#'}
#'
#'
#' @return  Returns the default probabilites values for the n ratings at the 2.5, 5, 25, 50, 75, 95, 97.5 percentiles.
#'
#' @export
#'
#' @author  Abdoulaye (Ab) N'Diaye
#'
#' @references
#' Hanson, S. and Schuermann, T. 2005 Confidence Intervals for Probabilities of Default,
#' Federal Reserve Bank of New York
#'
#' Jafry, Y. and Schuermann, T. 2003 Metrics for Comparing Credit Migration Matrices,
#' Wharton Financial Institutions Working Paper 03-08.
#'
#' Loffler, G., P. N. Posch. 2007 Credit Risk Modeling Using Excel and VBA.
#' West Sussex, England, Wiley Finance
#'
#' Trueck, Stefan, (February 16, 2009) Simulating Dependent Credit Migrations. Available at SSRN:
#' https://ssrn.com/abstract=1344897
#'
#' @examples
#' \dontrun{
#' startDate  <- "2000-01-01"
#' endDate    <- "2005-01-01"
#' method       <- "duration" 
#' snapshots <- 4
#' interval  <-  0
#' Example1  <-getPIT(data,startDate, endDate,method, snapshots, interval)
#' 
#' lstInit <- Example1$lstInitVec[lapply(Example1$lstInitVec,length)>0]
#' lstCnt <-  Example1$lstCntMat[lapply(Example1$lstCntMat,length)>0]
#' ExampleTTC1<-duration.TTC(Example1$lstCntMat,Example1$lstInitVec)
#' 
#' 
#' genMat   <- ExampleTTC1$WGM
#' portWgts <- ExampleTTC1$SWFY[,1]
#' nHorizon <- length(ExampleTTC1$UUPTM[[1]])
#' sim      <- 100
#' 
#' tolerance_Duration <- duration.CI(genMat,portWgts,nHorizon,sim)
#' }
#'
duration.CI <- function(genMat, portWgts, nHorizon, sim) {
  if (nHorizon < 1) {
    stop("Error: Invalid Horizon. Valid Horizon numbers are greater than 0")
  }
  
  
  
  if (!is.numeric(portWgts)) {
    stop("Error: The initial counts vector (start vector) is not numeric")
  }
  
  
  if (0 %in% portWgts) {
    stop("Error: There is at least 1 zero in the initial counts vector (start vector)")
  }
  
  
  nStates_row <- nrow(genMat)
  nStates_col <- ncol(genMat)
  
  if (nStates_row == nStates_col) {
    nStates <- nStates_row
  } else{
    stop("Error: Transition matrix rows and columns must be equal")
  }
  
  
  if (length(portWgts) != nStates_row) {
    stop("Error: the number of weights must correspond to the number of States")
  }
  
  portWgts <- portWgts
  sim <- sim
  horizon <- nHorizon
  
  # Number of states and vector enumerating them
  nstates <- length(genMat[, 1])
  stateVec <- 1:nstates
  
  
  # Fix the last row
  sumLastRow <- sum(genMat[nstates, ])
  if (sumLastRow == 0) {
    genMat[nstates, ] <- 0
    genMat[nstates, nstates - 1] <- 0.0001
    genMat[nstates, nstates] <- -0.0001
  }
  
  
  # Probabilities for state movements
  probs = ((matrix(1, nstates, nstates) - diag(1, nstates)) * genMat) /
    matrix(pracma::repmat(rowSums((
      matrix(1, nstates, nstates)  - diag(1, nstates)
    ) * genMat), 1, nstates), nstates)
  
  
  # Fix the last row
  sumLastRow <- sum(probs[, nstates])
  if (is.nan(sumLastRow)) {
    probs[nstates, ] <- 0
    probs[nstates, nstates - 1] <- 1
    
  }
  
  # Default vectors
  defVec = matrix(0, sim, nstates)
  
  
  for (i in 1:sim) {
    transMat = matrix(0, nstates, nstates)
    stateTime = matrix(0, nstates, nstates)
    simResultsMat = c(0, 0, 0, 0, 0)
    
    
    for (j in 1:nstates) {
      for (k in 1:portWgts[j]) {
        # Starting values for the replication
        tempResultsMat <- matrix(0, 1, 5)
        currentState <- j
        timeToHorizon <- horizon
        
        
        while (timeToHorizon > 0) {
          # Draw from the relevant exponential distribution and write to results vector
          eDraw <-
            stats::rexp(1, 1 / (-1 / genMat[currentState, currentState]))
          tempResultsMat[, 1] <- i
          tempResultsMat[, 2] <- k
          tempResultsMat[, 3] <- currentState
          tempResultsMat[, 4] <- eDraw
          
          # # If sojourn time is less than time remaining to horizon draw from
          # # multinomial distribution defined by off-diagonal element of generator row
          # # and record time spent in "old" state and transition ("new") state
          # # Calculate time remaining to horizon
          if (eDraw < timeToHorizon) {
            timeToHorizon <- timeToHorizon - eDraw
            stateTime[j, currentState] = stateTime[j, currentState] + eDraw
            newState <-
              colSums(stats::rmultinom(1, size = 1, prob = probs[currentState, ]) * stateVec)
            
            transMat[currentState, newState] = transMat[currentState, newState] + 1
            tempResultsMat[5] = newState
            
            
            # If sojourn time is greater than or equal to time remaining to horizon
            # record time spent in old state and set time remaining to horizon to zero
          } else if (eDraw >= timeToHorizon) {
            stateTime[j, currentState] <-
              stateTime[j, currentState] + timeToHorizon
            tempResultsMat[5] = currentState
            newState <- currentState
            timeToHorizon <- 0
          }
          
          
          # Write temporary results vector to output matrix
          # Use this only to track the intermediate movements.
          
          if (i * j * k == 1) {
            simResultsMat <- tempResultsMat
          } else {
            simResultsMat <- cbind(simResultsMat, tempResultsMat)
          }
          
          currentState <- newState
          
        }
      }
    }
    
    # Calculate simulated generator and resulting default vector
    x =  t(matrix(pracma::repmat(colSums(stateTime, 1), nstates, 1), nstates))
    simGen = transMat / x
    simGen = simGen - diag(rowSums(simGen, 2))
    simTrans = expm::expm(simGen)
    defVec[i, ] = t(simTrans[, nstates])
    
  }
  
  outpcnt <-
    matrixStats::colQuantiles(defVec, probs = c(.025, .05, .25, .50, .75, .95, .975))
  
  return(outpcnt)
  
}


