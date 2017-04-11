
#' Bootstrapped confidence intervals - Cohort
#'
#' @description estimate confidence intervals for the transition probabilities using a bootstrapping procedure for cohort method.
#'
#' @usage cohort.CI(transMatrix,initCount,nMonths,nStates,sim)
#'
#' @param transMatrix list of 12 average monthly credit transitions.
#' @param initCount list of 12 average monthly initial counts (start vector).
#' @param nMonths number of months in the horizon
#' @param nStates number of rating categories.
#' @param sim number of simulations
#'
#'
#' @details
#' The general idea of bootstrapping is to use resampling methods to estimate features of the sampling distribution of
#' an estimator, especially in situations where 'asymptotic approximations' may provide poor results. In the case of a
#' \emph{parametric} bootstrap method one samples from the estimated distribution derived using maximum likelihood estimation.
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
#' empirical cumulative distribution function of the observed sample. Since there are grades with zero observed default
#' rates, resampling directly from the observed data will not produce meaningful confidence intervals in for credit transition
#' matrices where historically there are a limited number of defaults in higher credit quality buckets.
#'
#' The parametric bootstrap method modeled here generates 12-month paths for each obligor represented in the portfolio and
#' estimates the 12 monthly transition matrices to get a single observation. Annual paths (histories) are simulated using
#' the estimated monthly transition matrices. A consequence of this approach, is that it is computationally intensive, but once
#' the bootstrapped distributions of the PD values have been completed, it is simple to identify the percentiles of interest
#' for calculation of confidence intervals
#'
#' @return Returns the default probabilites values for the \emph{n} ratings at the 2.5, 5, 25, 50, 75, 95, 97.5 percentiles.
#'
#' @export
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
#' @author  Abdoulaye (Ab) N'Diaye
#'
#' @examples
#'
#' sim <- 1000                                 #number of simulation
#' nMonths <- 12                               #number of month in horizon (default=12)
#' nStates <- 8                                #number of ratings categories
#' initCount <- c(5,10,15,20,20,20,15,5)       #start vector (vector of initial counts)
#' \dontrun{
#' tolerance <-cohort.CI(transMat,initCount,nMonths,nStates,sim)
#' }

cohort.CI <- function(transMatrix,initCount,nMonths,nStates,sim){

  validMonths <- c(6,12)
  if (!isTRUE(nMonths %in% validMonths)){
    stop("Error: Invalid Months. Valid month numbers are 6 or 12")
  }


  if (nStates < 2 || nStates > 25){
    stop("Error: Invalid Number of 'Risk States'. Valid 'Number of Risk States' are between 2 and 25")
  }



  #Check the lists to see if the count matrix and initial counts (start vector) are valid
  for (k in 1:length(transMatrix)){

    cm.matrix(as.matrix(as.data.frame(transMatrix[k])))
  }


  if (!is.numeric(initCount)){
    stop("Error: The initial counts vector (start vector) is not numeric")
  }


  if(0 %in% initCount){
    stop("Error: There is at least 1 zero in the initial counts vector (start vector)")
  }



  nStates_row <- nrow(as.data.frame(transMatrix[[1]]))
  nStates_col <- ncol(as.data.frame(transMatrix[[1]]))

  if(nStates_row ==nStates_col){
    nStates <- nStates_row
  } else{
    stop("Error: Transition matrix rows and columns must be equal")
  }

  # Initialize output matrices
  resmat <-replicate(nMonths, matrix(0,nStates,nStates), simplify=F)
  results <- matrix(0,sim,nStates)
  initCount = 9*initCount;


  # Obtain dimensions of input transition array
  k <- length(transMatrix)        #get the number of matrices in the list 'X'
  m <- dim(transMatrix[[1]])[1]   #get the matrix row count
  n <- dim(transMatrix[[1]])[2]   #get the matrix column count


  # Scale rows to sum to one to account for rounding
  for ( i in 1:k){
    temp <- rowSums(transMatrix[[i]])
    transMatrix[[i]]<-  matrix(transMatrix[[i]],m)/matrix(pracma::repmat(temp,1,m),m)

  }


  #Simulation loop
  for (j in 1:sim){

    #Initialize arrays
    graden <-  matrix(0,1,m)
    prodmat <- diag(1,m)

    # Loop over months in year
    for (i in 1:k){

      if (i == 1){
        ndraw <-  initCount
      }else{
        ndraw <- graden;
      }

      # Multinomial random draws to produce next month
      # transition matrix. Post multiply previous months
      # to calculate annual transition matrix
      for (z in 1:m){

        mr1 <- matrix(stats::rmultinom(1,size=ndraw[z],prob=matrix(transMatrix[[i]],m)[z,]),1)

        if (z == 1){
          mr <-  mr1
        }else{
          mr <-  rbind(mr,mr1)
        }

      }

      graden <- rowSums(mr)
      resmat[[i]] <- mr/matrix(pracma::repmat(ndraw,1,m),m)
      prodmat <- prodmat%*%resmat[[i]]


    }

    results[j,] <- t(prodmat[,m])


  }


  # Calculate annual transition matrix
  testDiscrete <- diag(1,m)
  for (i in 1:nMonths){

    testDiscrete <- testDiscrete * matrix(transMatrix[[i]],m)

  }

  # Percentiles of PD distribution
  outpcnt <- matrixStats::colQuantiles(results,probs = c(.025, .05, .25, .50, .75, .95, .975))

  return(outpcnt)

}


