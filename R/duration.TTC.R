#' Duration - Data Weighting and "TTC" Calculation
#'
#' @description  Calculating \emph{Through-the-Cycle} generator matrix and transition counts using \emph{duration method}
#'
#' @usage duration.TTC(lstCnt,lstFirmYears)
#'
#' @param lstCnt off-diagonal transition counts (matrix) for each time-step
#' @param lstFirmYears firm years each time-step
#'  
#' 
#' @return \item{CLW}{\emph{Count Level Weighting} - Construct TTC transition matrix from aggregate scaled and weighted counts data (transitions and 'firm-years').}
#' @return \item{PTMLW}{\emph{Periodic Transition Matrix Level Weighting} - Construct TTC transition matrix using the average of the weighted transition matrices from each time-step (Scaling is performed at the transition matrix level for each time-step).} 
#' @return \item{PGMLW}{\emph{Periodic Generator Matrix Level Weighting} - Construct TTC transition matrix using the average of the weighted Generator matices from each time-step (Scaling is performed at the generator matrix level for each time-step).}
#' @return \item{UUPTM}{\emph{Unscaled and UnWeighted Periodic Transition Matrices} - Construction of unscaled and unweighted periodic transition matrices from unscaled and unweighted generator matrices for each time-step .}
#' @return \item{WGM}{\emph{Weighted Generator Matrix} - Average generator matrix from each time-step.}
#' @return \item{SWT}{\emph{Scaled and Weighted Transitions} - aggregate scaled and weighted transitions}
#' @return \item{SWFY}{\emph{Scaled and Weighted Firm Years} - aggregate scaled and weighted firm years}
#'
#'          
#' @export
#' @details
#' Given data representing \emph{x} off-diagonal transition counts for each time-step, this function combines those data to obtain average counts for each time-step,
#' in such a way as to preserve the information while implementing a weighting scheme that would allow for the weighting of the historical experiences.
#'
#' Let \eqn{T(m,y)} and \eqn{F(m,y)} represent the off-diagonal transition matrix and 'firm-years' vector, for month = \eqn{m} and year = \eqn{y},
#' respectively. Then,
#' \deqn{T(m,y) = \{T_{ij}(m,y)\}_{i,j\,=\,1,\ldots,K}}
#' \deqn{F(m,y) =  \{F_{i}(m,y)\}_{i\,=\,1,\ldots,K}}
#'
#' @author  Abdoulaye (Ab) N'Diaye
#'
#' @details
#' Many credit risk models require a \emph{long-run average} PD estimate. This has been interpreted as meaning the data from multiple years should be
#' combined and in a method capable of supporting some form of weighting of samples.
#' The three methods of weighting considered for data generated via the \emph{duration method} are:
#' \enumerate{
#' 	\item Scale the number of transitions and firm counts/years using the a single year count to preserve dynamics, then average transitions and firms
#' 	counts/years separately to create a generator matrix.
#' 	\item Estimate the single-year quantities \emph{(generator matrices for each time-step)}, then average across years
#' 	\item Average transition matrices from each time-step
#' }
#' The Markov property allows for direct weighting as each year can be regarded as distinct.
#'
#'
#' @examples
#' \dontrun{
#' 
#' #Set parameters
#' startDate  <- "2000-01-01"
#' endDate    <- "2005-01-01"
#' method       <- "duration"   
#' snapshots <- 4
#' interval <-  0
#' Example1<-getPIT(data,startDate, endDate,method, snapshots, interval)
#' 
#' ExampleTTC1<-duration.TTC(Example1$lstCntMat,Example1$lstInitVec)
#' 
#' }

duration.TTC <- function(lstCnt, lstFirmYears) {
  gHorizon <- length(lstCnt[[1]])
  
  if (gHorizon == 1) {
    snaps = "years"
  } else if (gHorizon == 4) {
    snaps = "quarters"
  } else if (gHorizon == 12) {
    snaps = "months"
  }
  
  nYear <- length(lstCnt)
  nStates <- nrow(as.data.frame(lstFirmYears[[1]][1]))
  
  
  
  
  if (nYear < 1) {
    stop("Error: Invalid number of years. Number of years must be greater than 0")
  }
  
  
  if (nStates <= 2 && nStates <= 25) {
    stop(
      "Error: Invalid Number of 'Risk States'. Valid 'Number of Risk States' are between 2 and 25"
    )
  }
  
  
  #Check the lists to see if the count matrix and firm years (start vector) are valid
  for (k in 1:length(lstCnt)) {
    for (n in 1:length(lstCnt[[k]])) {
      cm.matrix.counts(as.matrix(as.data.frame(lstCnt[[k]][n])))
      #cm.vector.counts(as.matrix(as.data.frame(lstFirmYears[[k]][n])))
    }
  }
  
  
  nStates_row <- nrow(as.data.frame(lstCnt[[1]][1]))
  nStates_col <- ncol(as.data.frame(lstCnt[[1]][1]))
  
  if (nStates_row == nStates_col) {
    nStates <- nStates_row
  } else{
    stop("Error: Transition matrix rows and columns must be equal")
  }
  
  
  nYear <- nYear
  transCount <- lstCnt  #lstStoreCntMatrices
  firmYear  <-  lstFirmYears #lstStoreInitMatrices
  
  # Year weights
  genWeight <- pracma::repmat(1 / nYear, nYear, 1) # Equally weighted
  
  # Initial matrices
  if (gHorizon <= 1) {
    genAnnual  <-
      rep(list(list()), length(lstCnt))   #rep(list(list()), gHorizon+1)  #
    transAnnual  <-
      rep(list(list()), length(lstCnt))  #rep(list(list()), gHorizon+1)
  } else {
    genAnnual  <-
      rep(list(list()), length(lstCnt))   #rep(list(list()), gHorizon)  #
    transAnnual  <-
      rep(list(list()), length(lstCnt)) #rep(list(list()), gHorizon)
  }
  genAverage  <- list()
  transAverage  <- list()
  
  
  # Initialize numerator and denominator for average data generator
  genRawNum  <- list()
  genRawDen  <- list()
  
  # #===============================================================================================
  
  firmYears <- t(matrix(unlist(firmYear), nStates))
  
  
  # Scale factors to normalize counts to latest year (assumes sort)
  nfirmYear <-
    rowSums(firmYears)  #convert firmYear to transposed matrix
  scaler <- nfirmYear / nfirmYear[nYear]
  
  
  # Use raw counts, taking weighted average of scaled transition counts and
  # firm years separately, then use ratio to construct average data generator
  
  
  # Produce weighted average transiton counts, firm years then calculate generators for each time-step
  # (average and not) and transition matrices
  
  
  
  for (i in 1:nYear) {
    for (l in 1:length(transCount[[i]])) {
      # Section 1
      # Scaled and Weighted counts (transition and firm years) for Averaged data generator components
      #
      if (i == 1 && l == 1) {
        # Section 1.a
        # (scale the weights of the off-diagonal transition counts)- genRawNum
        genRawNum <-
          ((genWeight[i] / gHorizon) / scaler[i]) * (as.matrix(as.data.frame(transCount[[i]][l])))
        # Section 1.b
        # (scale the weights of the off-diagonal firm years)- genRawDen
        genRawDen <-
          ((genWeight[i] / gHorizon) / scaler[i]) * pracma::repmat(matrix(firmYears[i, ], nStates), 1, nStates)
      } else {
        # Section 1.a
        # (scale the weights of the off-diagonal transition counts, apply that scaler to the off-diagonal transition counts and compound the results)- genRawNum
        genRawNum <-
          genRawNum + ((genWeight[i] / gHorizon) / scaler[i]) * (as.matrix(as.data.frame(transCount[[i]][l])))
        # Section 1.b
        # (scale the weights of the firm years, apply that scaler to the firm-years  and compound the results)- genRawDen
        genRawDen <-
          genRawDen + ((genWeight[i] / gHorizon) / scaler[i]) * pracma::repmat(matrix(firmYears[i, ], nStates), 1, nStates)
        
      }
      
      # Section 2
      # Weighted but Unscaled generator averaging for each time-step
      #
      # Section 2.a
      # (Create the generator matrix)- genAnnual
      genAnnual[[i]][[l]] <-
        as.matrix(as.data.frame(transCount[[i]][l])) / pracma::repmat(matrix(firmYears[i, ], nStates), 1, nStates)
      genAnnual[[i]][[l]] <-
        genAnnual[[i]][[l]] - diag(rowSums(genAnnual[[i]][[l]]))
      
      # Section 2.b
      # (equally weight and compound the generator matrix)- genAverage
      if (i == 1 && l == 1) {
        genAverage <-  (genWeight[i] / gHorizon) * genAnnual[[i]][[l]]
      } else{
        genAverage <-
          genAverage + (genWeight[i] / gHorizon) * genAnnual[[i]][[l]]
      }
      
      
      # Section 3
      # Average transition matrix (individual transition matrices for each time-step)
      #
      # Section 3.a
      # (for each generator matrix created in Section 2.a, create a corresponding transition matrix)
      transAnnual[[i]][[l]] = expm::expm(genAnnual[[i]][[l]])
      # Section 3.b
      # (weight and compound each transition matrix created in 3.a)
      if (i == 1 && l == 1) {
        transAverage <- (genWeight[[i]][l] / gHorizon) * transAnnual[[i]][[l]]
      } else{
        transAverage <-
          transAverage + (genWeight[[i]] / gHorizon) * transAnnual[[i]][[l]]
        
      }
      
    }
    
  }
  
  
  
  # Use the scales and weighted counts (transition and firm years) to construct a generator matrix and then transition matrix.
  genRaw <- genRawNum / genRawDen
  diagRaw <- rowSums(genRaw)
  genRaw <- genRaw - diag(diagRaw)
  transRaw <- expm::expm(genRaw)
  
  #Transition matrix using the average of the Generator matices  #Average of transition matrices
  transAverageGen <- expm::expm(genAverage)
  
  
  outPut  <-
    list(
      PTMLW = transAverage,
      #Construct TTC transition matrix using the average of all of the weighted transition matrices for each time-step(Scaling is performed at the transition matrix level for each time-step)
      PGMLW = transAverageGen,
      #Construct TTC transition matrix using the average of all of the weighted Generator matices for each time-step(Scaling is performed at the generator matrix level for each time-step)
      CLW = transRaw,
      #Construct TTC transition matrix from scaled and weighted counts data (transitions and firm years)
      UUPTM = transAnnual,
      #Construction of unscaled and unweighted transition matrices for each time-step from unscaled and unweighted generator matrices for each time-step
      WGM = genAverage,
      #weighted generators for each time-step
      SWT = genRawNum,
      #aggregate scaled and weighted transitions
      SWFY = genRawDen
    )                          #aggregate scaled and weighted firm years
  
  
  return(outPut)
  
  
}
