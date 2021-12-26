#' @title Autocorrelation of a signal
#'
#' @description This function returns the autocorrelation (ac) estimates of a signal (time series) x of length N. 
#' Two types of estimators available : biased  (default) and unbiased. In addition,
#' this function has a plot utility to visualize the autocorrelation values with the default lag 50.
#'
#' @details
#' 
#' @param x, Numeric, complex or logical vector of length `N`
#' @param biased logical TRUE (default) for biased estimator
#' @param demean logical to demean signal x before finding the ac (default TRUE)
#' @param normalize logical to return the normalized ac values (default)
#' @param onesided logical return the two-sided (default) or one-sided ac values
#' @return A vector of autocorrelation values  of length `2N-1` (includes positive and negative lags.)
#'  
#' @seealso [corrtest::TimeCC()] .
#' @export
#' @examples
#' TimeAC(c(1,2,3,4,5,6))   
#' TimeAC(rnorm(20))    

TimeAC <- function(x,biased=TRUE,demean=TRUE,normalize=FALSE,onesided=FALSE)
{
  # This function Implements Eqn. (8.117) for biased estimator and Eqn. (8.119)
  # for unbiased estimator in Hayes  (assuming x is a zero-mean time series)
  
  N <-length(x)
  auto_corr <- array(0, dim = c(N))  # One-sided autocorrelation
  
  if(demean){
    x<-x-mean(x)
  }
  
  # Compute the one-sided autocorrelation
  
  if (biased){
    for (i in 1:N){
      auto_corr[i]<-(x[1:(N-i+1)]%*%x[i:N])/N    # Biased estimator
    }
  } else {
    for (i in 1:N){
      auto_corr[i]<-(x[1:(N-i+1)]%*%x[i:N])/(N-i+1) # Unbiased esitmator
    }
  }
  
  if(normalize){   # Max-normalize
    auto_corr<-auto_corr/max(auto_corr)
  }
  
  if(!onesided){  # two-sided autocorrelation
    auto_corr<-c(rev(auto_corr[2:N]),auto_corr[1:N])
  }
  return(auto_corr)
}

#as.numeric(acf(c(1,-1,0,4.5,4.2,-1,-2.3),plot = F)[[1]])
#TimeAC(c(1,-1,0,4.5,4.2,-1,-2.3),normalize = T)
