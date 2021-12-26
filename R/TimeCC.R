#' @title Crosscorrelation of a signal
#'
#' @description This function returns the crosscorrelation (cc) estimates of a signal (time series) x of length N. 
#' Two types of estimators available : biased  (default) and unbiased. In addition,
#' this function has a plot utility to visualize the autocorrelation values with the default lag 50.
#' 
#' @param xy Numeric, complex or logical vectors that indicate signals of length `N` each
#' @param biased logical TRUE (default) for biased estimator
#' @param demean logical to demean signal x before finding the ac (default TRUE)
#' @param normalize logical to return the normalized ac values (default)
#' @param onesided logical return the two-sided (default) or one-sided ac values
#' @return A vector of crosscorrelation values  of length `2N-1` (includes positive and negative lags.)
#'  
#' @seealso [corrtest::TimeAC()] .
#' @export
#' @examples
#' TimeCC(c(1,2,3,4,5,6),c(6,5,4,3,2,1))   
#' TimeCC(rnorm(20),rnorm(20))    

TimeCC<-function(x,y)
{
  # Cross-correlation estimation using asymptotically unbiased estimator
  # x and y are assumed to be of same length
  # Cross correlation is not symmetric (careful to deal with the negative lags)

  # Initializations
  M=length(x)
  rp <- array(0, dim = c(M))   # Positive part
  rn <- array(0, dim = c(M-1)) # Negative part

  # For positive lags
  for (i in 1:M)
  {
    rp[i]=(1/(M))*(x[i:M]%*%y[1:(M-i+1)])
  }

  # For negative lags
  for (i in 1:M)
  {
    rn[i]=(1/(M))*(y[i:M]%*%x[1:(M-i+1)])

  }

  # Combined negative and positive parts returned
  return(c(rev(rn[2:M]),rp))
}
