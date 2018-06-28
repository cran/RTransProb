#' estimate of transition probabilities.
#'
#' @description estimation of transition probabilities using a transition counts and start vector.
#'
#' @usage transitionprobbytotals(idTotCnt,snapshots,interval,method)
#'
#'
#' @param   idTotCnt a list structure containing \emph{m-by-m} matrices of transition counts, \emph{1-by-m} vectors start counts, and a string with values 'duration' or 'cohort'.
#' @param   snapshots integer indicating the number of credit-rating snapshots per year to be considered for the estimation. Valid values are 1, 4, or 12. The default value is 1, \emph{i.e., one snapshot per year}. This parameter is only used in the 'cohort' algorithm.
#' @param   interval the length of the transition interval under consideration, in years. The default value is 1, \emph{i.e., 1-year transition probabilities are estimated}.
#' @param   method estimation algorithm, in string format. Valid values are 'duration'  or 'cohort'.
#'
#' @return  Returns \emph{m-by-m} matrices of credit transition probabilities
#'
#' @export
#'
#'
#' @references
#' MathWorld.com (2011). Matlab Central \url{http://www.mathworks.com/matlabcentral/}. Mathtools.net \url{http://www.mathtools.net/}.
#'
#' @author  Abdoulaye (Ab) N'Diaye
#'

transitionprobbytotals <-
  function(idTotCnt, snapshots, interval, method) {
    totals = idTotCnt
    rm(idTotCnt)
    gc()
    snapshots = snapshots
    interval = interval
    
    
    #==================================================================
    
    rows_cols <- sqrt(lengths(totals$totalsMat[1]))
    cubelength    <- length(totals$totalsMat)
    
    
    totals_totalsMat <-
      array(totals$totalsMat, dim = c(rows_cols, rows_cols, cubelength))
    totals_totalsVec <-
      array(totals$totalsVec, dim = c(rows_cols, 1, cubelength))
    totals_method   <- array(unlist(totals$method))
    slices <- length(totals_totalsVec)
    
    sampleTotals = getSampleTotals(
      totals_totalsMat,
      totals_totalsVec,
      totals_method[1],
      cubelength,
      rows_cols,
      rows_cols
    )
    
    
    rm(totals_totalsMat)
    rm(totals_totalsVec)
    rm(totals)
    gc()
    
    #==================================================================/
    
    
    sampleTotals$totalsVec <- as.vector(sampleTotals$totalsVec)
    sampleTotals$totalsMat <- as.matrix(sampleTotals$totalsMat)
    transMat1 = getTransitionProbability(
      sampleTotals$totalsMat,
      sampleTotals$totalsVec,
      sampleTotals$method,
      snapshots,
      interval
    )
    
    
    
    if (method == "tnh") {
      if (snapshots != 1 || interval != 1) {
        transMat1$transMat <-
          expm::`%^%`(transMat1$transMat, (snapshots * interval))
      }
      
      
    } else if (method == "cohort") {
      if (snapshots != 1 || interval != 1) {
        transMat1$transMat <-
          expm::`%^%`(transMat1$transMat, (snapshots * interval))
      }
      
    }
    
    
    transMat1$transMat <- 100 * transMat1$transMat
    transMat = transMat1$transMat
    
    
    if (method == "tnh") {
      diag(transMat) <- 0
      diag(transMat) <- -rowSums(transMat)
      getI <- diag(x = 1, nrow(transMat), ncol(transMat))
      transMatI <- getI + (transMat / 100)
    }
    
    
    
    if (method == "duration") {
      genMat = transMat1$genMat
    }
    
    if (method == "cohort") {
      probbytotals <- list(sampleTotals = sampleTotals,
                           transMat = transMat)
    } else if (method == "duration") {
      probbytotals <- list(sampleTotals = sampleTotals,
                           transMat = transMat,
                           genMat = genMat)
    } else if (method == "tnh") {
      probbytotals <- list(transMat = transMat,
                           transMatI = transMatI)
    }
    
    
    return(probbytotals)
    
  }
