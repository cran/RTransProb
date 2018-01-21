#' Estimation of credit transition probabilities
#'
#' @description This function is used to estimate Point-in-Time transition probabilities and counts given historical credit data (a.k.a., credit migration data).
#'
#' @usage getPIT(data, startDate, endDate, method, snapshots, interval)
#'
#' @param data              a table containing historical credit ratings data (i.e., credit migration data). A dataframe of size \emph{nRecords} x 3 where each row contains an ID (column 1), a date (column 2), and a credit rating (column 3); The credit rating is the rating assigned to the corresponding ID on the corresponding date.
#' @param startDate             start date of the estimation time window, in string or numeric format. The default start date is the earliest date in 'data'.
#' @param endDate             end date of the estimation time window, in string or numeric format. The default end date is the latest date in 'data'. The end date cannot be a date before the start date.
#' @param method            estimation algorithm, in string format. Valid values are 'duration'  or 'cohort'.
#' @param snapshots  integer indicating the number of credit-rating snapshots per year to be considered for the estimation. Valid values are 1, 4, or 12. The default value is 1, \emph{i.e., one snapshot per year}. This parameter is only used in the 'cohort' algorithm.
#' @param interval  the length of the transition interval under consideration, in years. The default value is 1, \emph{i.e., 1-year transition probabilities are estimated}.
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
#'
#' @author Abdoulaye (Ab) N'Diaye
#'
#' @examples
#' \dontrun{
#' snapshots <- 4     #This uses quarterly snapshots
#' interval <- .25     #This gives quarterly transition matrix
#' startDate  <- "2000-01-01"
#' endDate    <- "2005-01-01"
#' Example<-getPIT(data,startDate, endDate,'cohort', snapshots, interval)
#' }


getPIT <- function(data, startDate, endDate, method, snapshots, interval){
  
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
  
  lstCnt <-rep(list(list()), AnnualBlock-1)
  lstInit <-rep(list(list()), AnnualBlock-1)
  lstPct <-rep(list(list()), AnnualBlock-1)
  lstGen <-rep(list(list()), AnnualBlock-1)
  lstMethod <-rep(list(), AnnualBlock-1)
  
  #initialize counters
  n <- 1
  k <- 1
  
  
  #start progressbar
  total <- length(TotalDateRange)-1                           ############################
  pb <- tkProgressBar(title = "progress bar", min = 0,
                      max = total, width = 300)
  
  #get transition counts and percentages
  for (l in 1:(length(TotalDateRange)-1)){
    
    
    sDate  <- TotalDateRange[l]
    eDate  <- TotalDateRange[l+1]
    Example1<-TransitionProb(data,sDate, eDate, method, snapshots, interval)
    lstCnt[[k]][[n]] <- Example1$sampleTotals$totalsMat #list of quarterly transition counts
    lstInit[[k]][[n]] <- Example1$sampleTotals$totalsVec #list of annual initial counts
    
    if(method=="duration"){
      lstGen[[k]][[n]]   <- Example1$genMat
    }
    
    lstPct[[k]][[n]]   <- Example1$transMat
    lstMethod[[k]]     <- method
    
    n <- n+1
    if(n>snapshots){
      n <- 1
      k <- k+1
    }
    
    Sys.sleep(0.1)
    setTkProgressBar(pb, l, label=paste( round(l/total*100, 0),
                                         "% done"))
  }
  
  
  close(pb)    ############################
  
  if(method == "duration"){
    PIT  <- list(lstCntMat=lstCnt,
                 lstInitVec=lstInit,
                 lstPctMat=lstPct,
                 lstGen = lstGen,
                 lstMethod=lstMethod)
  } else {
    PIT  <- list(lstCntMat=lstCnt,
                 lstInitVec=lstInit,
                 lstPctMat=lstPct,
                 lstMethod=lstMethod)
  }
  
  return (PIT)
}