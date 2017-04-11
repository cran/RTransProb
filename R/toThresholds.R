#' Convert probabilities to credit quality thresholds.
#'
#' @description Use this function to transform transition probabilities into credit quality thresholds.
#'
#' @usage toThresholds(trans)
#'
#'
#' @param   trans a \emph{m-by-m} matrix with transition probabilities, in percent. Entries cannot be negative and cannot exceed 100, and all rows must sum up to 100.
#'
#' @return  Returns a \emph{m-by-m} matrix of credit quality thresholds
#'
#' @export
#'
#'
#' @references
#' MathWorld.com (2011). Matlab Central \url{http://www.mathworks.com/matlabcentral/}. Mathtools.net \url{http://www.mathtools.net/}.
#'
#' @author  Abdoulaye (Ab) N'Diaye
#'
#'
#' @examples
#'
#' rc <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "D")
#' t<- matrix(c(91.3969, 7.1423, 1.3566, 0.0848, 0.0178, 0.0006, 0.0010, 0.0001,
#'               5.8072, 87.7881, 5.3402, 0.7040, 0.3391, 0.0116, 0.0081, 0.0014,
#'               0.3578, 8.0124, 81.7798, 8.8916, 0.7675, 0.0587, 0.1246, 0.0077,
#'               0.0966, 0.4232, 6.8627, 86.2059, 4.7967, 0.8681, 0.6951, 0.0516,
#'               0.0297, 0.4156, 0.2821, 6.5406, 85.4804, 4.8337, 1.7363, 0.6815,
#'               0.4866, 0.0389, 0.2467, 0.3945, 3.5428, 90.0229, 4.0516, 1.2161,
#'               0.0110, 0.0029, 0.0280, 0.5759, 0.6389, 3.9374, 86.5074, 8.2987,
#'               0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 100.0000
#' ), 8,8, dimnames = list(rc,rc), byrow=TRUE)
#'
#'
#' thresholds<-toThresholds(t)
#'

toThresholds <- function(trans){

  cm.matrix(trans)

  if (!(is.numeric(trans))){
    stop("Error: non-numeric input")
  }


  tolbnd = 1e-3; # tenth of basis point
  tolsum = 1e-2; # basis point

  if (any(any(trans< (-tolbnd)))){
    stop("Error: Lower Bound Violation")
  }


  if (any(any(trans>(100+tolbnd)))){
    stop("Error: Upper Bound Violation")
  }


  if (any(abs(rowSums(trans)-100*matrix(1,nrow(trans),1))>(tolsum))){
    stop("Error: Sum By Rows Violation")
  }

  # Truncate small bound violations, and rescale to ensure rows add up to
  # exactly 1 (in decimal format to apply norminv)
  trans = pmax(trans,0);
  trans = pmin(trans,100);
  t<-diag(rowSums(trans))
  trans = solve(t)%*% as.matrix(trans)

  # Get cum prob and ensure they range from 0 to 1
  trans_rev<-rev(as.data.frame(trans))
  cumprob<-t(apply(trans_rev,1,cumsum))
  cumprob<-rev(as.data.frame(cumprob))


  cumprob = pmax(cumprob,0);
  cumprob = pmin(cumprob,1);
  r <- which(as.matrix(cumprob[,1])<1)

  if(length(r)!=0){
    s <- (as.matrix(cumprob[r,]) == as.data.frame(pracma::repmat(cumprob[r,1],1,ncol(cumprob))));
    s <-  1*s                #convert TRUE/FALSE to 0/1
    for (i in 1:length(r)){
      cumprob[r[i],s[i]]=1;
    }
  }

  # Transform into thresholds
  thresh = stats::qnorm(as.matrix(cumprob));

}

