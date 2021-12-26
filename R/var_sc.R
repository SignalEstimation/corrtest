#' @title Variance of Pearson Correlation
#'
#' @description This function returns the variance of the Pearson correlation between two signals
#' 
#' @param xy Numeric vectors that indicate signals of length `N` 
#'  
#' @return scalar of variance 
#' 
#' @export
#' @examples
#' var_sc(c(1,2,3,4,5,6),c(6,5,4,3,2,1))   

var_sc<- function(x,y)
{
  # x and y are two time series
  N= length(x)

  # Initialization
  D<-array(0,dim=c(3,3))

  # Estimate the time series auto-correlation
  rx=TimeAC(x)  # of length (2N-1) with N as the zero lag
  ry=TimeAC(y)

  #Estimate the time series cross-correlation
  rxy=TimeCC(x,y)
  ryx=TimeCC(y,x)

  rx0=rx[N]  # rx(0) zero-lag at N-th index
  ry0=ry[N]  # ry(0)
  rxy0=rxy[N]  # rxy(0)

  # Formation of vector d  [Eqn. (6.1)]
  d=c(-rxy0/(2*(sqrt((rx0^3)*ry0))),
      -rxy0/(2*(sqrt(rx0*(ry0^3)))),
      1/sqrt(rx0*ry0))

  # Formation of matrix V  [Eqn. (6.2)]
  # Factor 2 is removed on the diagonals to have the correct transpose
  V[1,1]=sum(rx^2)
  V[1,2]=2*sum(rxy^2)
  V[1,3]=2*sum(rxy*rx)
  V[2,2]=sum(ry^2)
  V[2,3]=2*sum(rxy*ry)
  V[3,3]=0.5*(sum(rx*ry)+sum(rxy*ryx))
  V=(V+t(V))

  # Variance of the sample correlation
  va=(colSums(d* (V %*% d)))/N

  return(va)
}
