#' Duration - Data Weighting and "TTC" Calculation
#'
#' @description  Calculating \emph{Through-the-Cycle} generator matrix and transition counts using \emph{duration method}
#'
#' @usage duration.TTC(lstCnt,lstInit,gYear,snapshots,nStates,transtype,TotalDateRange)
#'
#' @param lstCnt annual off-diagonal transition counts (matrix)
#' @param lstInit Initial counts for each state (start vector)
#' @param gYear count of annual date vector.
#' @param snapshots integer indicating the number of credit-rating snapshots per year to be considered for the estimation. Valid values are 1, 4, or 12. The default value is 1, \emph{i.e., one snapshot per year}. This parameter is only used in the 'cohort' algorithm.
#' @param nStates number of rating categories.
#' @param TotalDateRange annual date vector.
#' @param transtype averaging method. The valid values
#' \itemize{
#' \item{\emph{transRaw}}
#' \item{\emph{genAverage}}
#' \item{\emph{transAverageGen}}
#' \item{\emph{transAverage}}
#' \item{\emph{individualAnnualTrans}}
#'}
#'
#' @return  \item{transRaw}{transition matrix created from generator matrix created using the average annual transition counts and average annual.}
#' @return  \item{genAverage}{average annual generator matrix.}
#' @return  \item{transAverageGen}{transition matrix from average annual generator matrix.}
#' @return  \item{transAnnual}{individual annual transition matrices.}
#'
#' @export
#' @details
#' Given data representing \emph{x} years of monthly off-diagonal transition counts, this function combines those data to obtain average monthly counts,
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
#' 	\item Estimate the single-year quantities \emph{(annual generator matrices)}, then average across years
#' 	\item Average annual transition matrices
#' }
#' The Markov property allows for direct weighting as each year can be regarded as distinct.
#'
#'
#' @examples
#'
#' TotalDateRange <- seq(as.Date("2000-01-01"), as.Date("2002-01-01"), "years")
#'
#' snapshots <- 1    #This uses a 1 year transition matrices
#' interval <- 1     #This gives a 1 year transition matrix
#'
#' lstCnt <-rep(list(list()), length(TotalDateRange)-1)
#' lstInit <-rep(list(list()), length(TotalDateRange)-1)
#' kk <- 1
#' k <- 1
#'
#' #Create transition and inital countsused as inputs to the function
#' for (l in 1:(length(TotalDateRange)-1)){
#'
#'   istartDate = POSIXTomatlab(as.POSIXlt(as.Date(TotalDateRange[l],format = "%Y-%m-%d")))
#'   iendDate = POSIXTomatlab(as.POSIXlt(as.Date(TotalDateRange[l+1],format = "%Y-%m-%d")))
#'   DateRange        <- as.Date(matlabToPOSIX(cfdates(istartDate,iendDate,snapshots)))
#'
#'   for(i in 1:(length(DateRange)-1)){
#'
#'     sDate  <- DateRange[i]       # i.e "3/31/1990"
#'     eDate    <- DateRange[i+1]     # i.e "6/30/1990"
#'
#'     t<-TransitionProb(data,sDate, eDate, 'duration', snapshots, interval)
#'
#'     lstCnt[[k]][[kk]] <- t$sampleTotals$totalsMat
#'     lstInit[[k]][[kk]]  <- t$sampleTotals$totalsVec
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
#' transtype <- "genAverage"
#' gYear <- utils::head(TotalDateRange,-1)
#' nStates <- nrow(as.data.frame(lstInit[[1]][1]))
#'
#' AverageGeneratorMatrices<-duration.TTC(lstCnt,lstInit,gYear,snapshots,nStates,transtype,
#' TotalDateRange)
#'

duration.TTC <- function(lstCnt,lstInit,gYear,snapshots, nStates,transtype, TotalDateRange){


  validMethod <- c("transAverage","transAverageGen","transRaw","genAverage")
  if (!isTRUE(transtype %in% validMethod)){
    stop("Error: Invalid Averaging Method. Valid averaging methods are listed in the documentation")
  }


  if (gYear < 1){
    stop("Error: Invalid number of years. Number of years must be greater than 0")
  }


  if (nStates <= 2 && nStates <= 25){
    stop("Error: Invalid Number of 'Risk States'. Valid 'Number of Risk States' are between 2 and 25")
  }


  #Check the lists to see if the count matrix and initial counts (start vector) are valid
  for (k in 1:length(lstCnt)){
    for(n in 1:length(lstCnt[[1]])){
      cm.matrix.counts(as.matrix(as.data.frame(lstCnt[[k]][n])))
      cm.vector.counts(as.matrix(as.data.frame(lstInit[[k]][n])))
    }
  }


  nStates_row <- nrow(as.data.frame(lstCnt[[1]][1]))
  nStates_col <- ncol(as.data.frame(lstCnt[[1]][1]))

  if(nStates_row ==nStates_col){
    nStates <- nStates_row
  } else{
    stop("Error: Transition matrix rows and columns must be equal")
  }


  nYear <- length(gYear)  #5
  transCount <- lstCnt  #lstStoreCntMatrices
  firmYear  <-  lstInit #lstStoreInitMatrices

  # Year weights
  genWeight <- pracma::repmat(1/nYear,nYear,1) # Equally weighted

  # Initial matrices
  if (snapshots <= 1){
    genAnnual  <- rep(list(list()), length(TotalDateRange)-1)   #rep(list(list()), snapshots+1)  #
    transAnnual  <- rep(list(list()), length(TotalDateRange)-1)  #rep(list(list()), snapshots+1)
  } else {
    genAnnual  <- rep(list(list()), length(TotalDateRange)-1)   #rep(list(list()), snapshots)  #
    transAnnual  <- rep(list(list()), length(TotalDateRange)-1) #rep(list(list()), snapshots)
  }
  genAverage  <- list()
  transAverage  <- list()


  # Initialize numerator and denominator for average data generator
  genRawNum  <- list()
  genRawDen  <- list()

  # #===============================================================================================

  #firmYears <- t(matrix(unlist(firmYear),9))
  firmYears <- t(matrix(unlist(firmYear),nStates))


  # Scale factors to normalize counts to latest year (assumes sort)
  nfirmYear <- rowSums(firmYears)  #convert firmYear to transposed matrix
  scaler <- nfirmYear/nfirmYear[nYear]


  # Use raw counts, taking weighted average of scaled transition counts and
  # firm years separately, then use ratio to construct average data generator


  # Produce weighted average transiton counts, firm years then calculate annual generators
  # (average and not) and transition matrices



  for (i in 1:nYear){

    for (l in 1:snapshots){

      # Section 1
      # Average data generator components
      #
      if(i==1 && l==1){
        # Section 1.a
        # (scale the weights of the off-diagonal transition counts)- genRawNum
        genRawNum <- ((genWeight[i]/snapshots)/scaler[i])*as.matrix(as.data.frame(transCount[[i]][l]))
        # Section 1.b
        # (scale the weights of the off-diagonal firm years)- genRawDen
        genRawDen <- ((genWeight[i]/snapshots)/scaler[i])*pracma::repmat(matrix(firmYears[i,],nStates),1,nStates)
      } else {
        # Section 1.a
        # (scale the weights of the off-diagonal transition counts, apply that scaler to the off-diagonal transition counts and compound the results)- genRawNum
        genRawNum <- genRawNum + ((genWeight[i]/snapshots)/scaler[i])*as.matrix(as.data.frame(transCount[[i]][l]))
        # Section 1.b
        # (scale the weights of the firm years, apply that scaler to the firm-years  and compound the results)- genRawDen
        genRawDen <- genRawDen + ((genWeight[i]/snapshots)/scaler[i])*pracma::repmat(matrix(firmYears[i,],nStates),1,nStates)

      }

      # Section 2
      # Annual generator averaging
      #
      # Section 2.a
      # (Create the generator matrix)- genAnnual
      genAnnual[[i]][[l]] <- as.matrix(as.data.frame(transCount[[i]][l]))/pracma::repmat(matrix(firmYears[i,],nStates),1,nStates)
      genAnnual[[i]][[l]] <- genAnnual[[i]][[l]] - diag(rowSums(genAnnual[[i]][[l]]));
      # Section 2.b
      # (equally weight and compound the generator matrix)- genAverage
      if(i==1 && l==1){
        genAverage <-  (genWeight[i]/snapshots)*genAnnual[[i]][[l]]
      } else{
        genAverage <-  genAverage + (genWeight[i]/snapshots)*genAnnual[[i]][[l]]
      }


      # Section 3
      # Average transition matrix (individual annual transition matrices)
      #
      # Section 3.a
      # (for each generator matrix created in Section 2.a, create a corresponding transition matrix)
      transAnnual[[i]][[l]] = expm::expm(genAnnual[[i]][[l]])
      # Section 3.b
      # (weight and compound each transition matrix created in 3.a)
      if(i==1 && l==1){
        transAverage <- (genWeight[[i]][l]/snapshots)*transAnnual[[i]][[l]]
      } else{
        transAverage <- transAverage + (genWeight[[i]]/snapshots)*transAnnual[[i]][[l]]

      }

    }

  }



  # Construct transition matrix from "average" data
  genRaw <- genRawNum/genRawDen
  diagRaw <- rowSums(genRaw)
  genRaw <- genRaw - diag(diagRaw)
  transRaw <- expm::expm(genRaw)

  # Average of transition matrices
  transAverageGen <- expm::expm(genAverage)


  if (transtype=="transAverage"){
    return(transAverage)
  }  else if (transtype=="transAverageGen"){
    return(transAverageGen)
  }  else if (transtype=="transRaw"){
    return(transRaw)
  }  else if (transtype=="genAverage"){
    return(genAverage)
  } else if (transtype=="individualAnnualTrans"){
    return(transAnnual)
  }

}

