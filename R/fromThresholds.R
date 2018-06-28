#' Convert credit quality thresholds to probabilities.
#'
#' @description Use this function to transform credit quality thresholds into transition probabilities.
#'
#' @usage fromThresholds(thresh)
#'
#' @param thresh \emph{m-by-m} matrix of credit quality thresholds. In each row, the first element must be \emph{Inf} and the entries must satisfy the following monotonicity condition:
#'
#' @export
#'
#' @return Returns a \emph{m-by-m} matrix with transition probabilities, in percent.
#'
#' @references
#' MathWorld.com (2011). Matlab Central \url{http://www.mathworks.com/matlabcentral/}. Mathtools.net \url{http://www.mathtools.net/}.
#'
#' @author Abdoulaye (Ab) N'Diaye
#'
#'
#' @examples
#'
#' \dontrun{
#' rc <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "D")
#' t<- matrix(c(Inf,-1.3656,-2.1806,-3.0781,-3.5482,-4.1612,-4.2591,-4.8399,
#'              Inf, 1.5712,-1.5217,-2.3028,-2.6872,-3.5256,-3.7324,-4.1972,
#'              Inf, 2.6895, 1.3806,-1.2901,-2.3422,-2.8928,-3.0063,-3.7861,
#'              Inf, 3.1004, 2.5623, 1.4479,-1.5211,-2.1407,-2.434,	-3.2814,
#'              Inf, 3.4339, 2.6156, 2.4434, 1.4561,-1.4573,-1.9742,-2.4668,
#'              Inf, 2.5852, 2.5586, 2.4218, 2.268,	 1.6737,-1.6194,-2.252,
#'              Inf, 3.6953, 3.6362, 3.3406, 2.5019, 2.2394, 1.6263,-1.3853,
#'              Inf, Inf,	   Inf,	   Inf,	   Inf,	   Inf,	   Inf,	   Inf
#' ), 8,8, dimnames = list(rc,rc), byrow=TRUE)
#'
#'
#' transmatrix <- fromThresholds(t)
#'}
#'
fromThresholds <- function(thresh) {
  if (dim(thresh)[1] < 2 ||
      dim(thresh)[2] < 2 || dim(thresh)[1] != dim(thresh)[2])
    stop("Error: incorrect dimension of the matrix")
  
  if (!(is.numeric(thresh))) {
    stop("Error: non-numeric input")
  }
  
  # if( any(!is.infinite(data[,1]))){
  #   stop("Error: non 'Inf' in the first column")
  # }
  
  tolmono = 1e-6
  
  for (i in 1:nrow(thresh)) {
    j1 = min(which(thresh[i, ] != Inf))
    
    
    if (!is.null(j1)) {
      j2 = max(which(thresh[i, ] != Inf))
      
      
      if (j2 > j1) {
        diffTh = diff(as.vector(as.matrix(thresh[i, j1:j2])))
        
        
        if (any(any(diffTh > tolmono))) {
          
        } else if (any(any(diffTh > 0))) {
          
        }
      }
    }
    
  }
  
  # Get cum prob
  cumprob =  stats::pnorm(t(apply(thresh, 1, rev)))
  
  # Get transition probabilities
  trans = cumprob
  
  trans[, 2:nrow(trans)] = t(diff(t(cumprob)))
  
  # Flip and convert output matrix from decimal to percent
  trans = t(apply(trans, 1, rev)) * 100
}
