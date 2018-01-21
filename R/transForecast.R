#' Forecast - using Credit Cycle
#'
#' @description This model implements the One-Parameter Representation method developed by Forest, Belkin and Suchower.
#'
#' @usage transForecast(genMat, isGenerator, creditIndex)
#'
#' @param genMat Generator Matrix
#' @param isGenerator a variable to determine if the input matrix is a generator matrix or a proper transition matrix. 
#' The default value is 'No'.
#' @param creditIndex a credit index.  The model relies on the assumption that credit migration matrices are driven by
#' a single parameter Z, which depicts the average financial health of corporate institutions (credit index). The degree
#' of 'shift' corresponds to a simple change in the transition probabilities from an average average matrix.
#'
#' @return The output consists of a forecasted (shifted) transition matrix.
#'
#' @details
#' The Vasicek (1987) Single Factor moodel \eqn{A_i = \sqrt{\rho_i}Z + \sqrt{1-\rho_i}\epsilon} presents a framework which Forest, Belkin
#' and Suchower (1998) used to developed the One-Parameter Representation method. In that model, migration behaviors are described
#' standard normal variables instead of transition probabilities without the loss of information. The transition through probabilites
#' are transformed to thresholds where the upper and lower bounds of the threshold values together represent bins. Therefore, when
#' a random variable falls within a particular bin that signifies a transition to the corresponding transition rating bucket.
#'
#' The advantage of representing transitions probabilities in terms of the threshold framework is that we can
#' now use the standard normal density curve to understand the behavior rating transitions.  The area under a
#' standard normal curve between the lower and upper bounds of a thresholds for a particular bin is the transition probability.
#' Therefore in the context of economic conditions, the shifting of curves (to the left or the right) under static
#' thresholds, informs us about the behavior of transitions matrices during benign and stressed periods.
#'
#' To the extent that we can represent economic conditions with a single variable, we can 'shift' the average
#' transition matrix by this amount to generate a forecast of the transition matrix.
#'
#' See Forest, Belkin and Suchower (1998) for a more detailed discussion
#'
#'
#' @export
#'
#' @author  Abdoulaye (Ab) N'Diaye
#'
#' Loffler, G., P. N. Posch. 2007 Credit Risk Modeling Using Excel and VBA.
#' West Sussex, England, Wiley Finance
#'
#' L. R. Forest, B. Belkin, and S. J. Suchower, 1998 A One-Parameter Representation of Credit Risk and Transition Matrices,
#' CreditMetrics Monitor. Q3
#'
#' Vasicek, O., 1987 Probability of loss on a loan portfolio. Working paper, KMV.
#'
#' @examples
#'
#' #Use the function 'TransitionProb' to estimate an annualized transition matrix which will
#' #then be used along with the appropriate creditIndex to forecast future period migration
#' #effects.
#'
#' \dontrun{
#' snapshots <- 4    #This uses quarterly transition matrices
#' interval <- 1    #This gives a 1 year transition matrix
#' startDate  <- "2000-01-01"
#' endDate    <- "2005-01-01"
#' Example9<-TransitionProb(data,startDate, endDate,'duration', period, snapshots, interval)
#' Example9.1 <- Example9$genMat
#' creditIndex <- -0.25
#'
#' 
#' Example10 <- transForecast(Example9.1, isGenerator, creditIndex)
#' }
#'
transForecast <- function(genMat,isGenerator, creditIndex){


  
  if (isGenerator=="Yes"){   #use generator
      Lambda <- genMat

      # correct diagonal
      D <- rep(0,dim(Lambda)[2])
      diag(Lambda) <- D
      rowsums <- apply(Lambda,1,sum)
      diag(Lambda) <- -rowsums

      transMat = expm::expm(Lambda)
      apply(transMat,1,sum)
  
  } else {
  
    transMat = genMat
}

  #graph the thresholds prior to forecasting

  transMat2 <- transMat[nrow(genMat):1,ncol(genMat):1]
  transMat2 <- transMat2[-1,]

  cumprobs <- t(apply(transMat2,1,function(v){cumsum(v)}))
  cumprobs
  thresholds<-stats::qnorm(cumprobs)
  thresholds

  opa <- graphics::par(mfrow=c(2,4))
  for (j in 1:nrow(thresholds))
  {
    graphics::plot(seq(from=-5,to=5,length=100),stats::dnorm(seq(from=-5,to=5,length=100)),type="l",
                   xlab="X",ylab="density",main=rownames(thresholds)[j])
    graphics::abline(v=thresholds[j,],col=1:length(thresholds[j,]))
  }
  graphics::par(opa)



  ## Now to perform the forecasting of the transition matrix


  #DO NOT Flip the states . . .
  transMat3<- transMat

  #for each loop get the row corresponding to the loop number get the cummulative probability of each
  #row (add up each cell starting from the first cell). . .
  #  . . .  get the inverse normal distribution for each cummulative probability (each cell) of each  row
  cumprobs_threshold <- matrix(transMat3, nrow =nrow(genMat), ncol = ncol(genMat))

  for (i in 1:dim(transMat3)[1]){

    for (j in dim(transMat3)[2]:1){

      cumprobs_threshold[i,j] <- stats::qnorm(sum(transMat3[i,j:dim(transMat3)[2]]))

    }

  }

  cumprobs_threshold[,1] <- 0

  thresholds_frcast_final <- cumprobs_threshold
  for (i in 1:dim(thresholds_frcast_final)[1]){

    for (j in dim(thresholds_frcast_final)[2]:1){

      if(j==dim(thresholds_frcast_final)[2]){
        t <- stats::pnorm(as.numeric(cumprobs_threshold[i,j])-creditIndex)
        thresholds_frcast_final[i,j] <-t
        
      } else if (j==1) {

        t <- 1- sum(as.numeric(thresholds_frcast_final[i,j:dim(thresholds_frcast_final)[2]]))
        thresholds_frcast_final[i,j] <-t


      } else {
        j_start <- j+1
        t <- stats::pnorm(as.numeric(cumprobs_threshold[i,j])-creditIndex) - sum(as.numeric(thresholds_frcast_final[i,j_start:dim(thresholds_frcast_final)[2]]))
        thresholds_frcast_final[i,j] <-t
        
      }


    }

  }



  #graph the thresholds after forecasting
  thresholds_frcast_final2 <- thresholds_frcast_final[nrow(genMat):1,ncol(genMat):1]
  thresholds_frcast_final2 <- thresholds_frcast_final2[-1,]

  cumprobs2 <- t(apply(thresholds_frcast_final2,1,function(v){cumsum(v)}))
  cumprobs2
  thresholds2<-stats::qnorm(cumprobs2)
  thresholds2

  opa <- graphics::par(mfrow=c(2,4))
  for (j in 1:nrow(thresholds2))
  {
    graphics::plot(seq(from=-5,to=5,length=100),stats::dnorm(seq(from=-5,to=5,length=100)),type="l",
                   xlab="X",ylab="density",main=rownames(thresholds2)[j])
    graphics::abline(v=thresholds2[j,],col=1:length(thresholds2[j,]))
  }
  graphics::par(opa)


  return(thresholds_frcast_final)

}

