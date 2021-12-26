#' @title Check for positive definite matrix
#' @description The function checks the numeric matrix A for symmetricity (Hermitian for complex matrices)
#'first and then for non-zero eigenvalues
#' @param A Numeric matrix (real or complex) 
#' @return TRUE indicates that the matrix A is symmetric (Hermitian) positive definite
#' @examples
#' A<-matrix(c(1,2,3,4),2,2)  # not symmetric
#' is_sym_pos_def(A)   # FALSE
#' 
#' A<-matrix(c(1,2,2,1),2,2)  # Symmetric but an eigen value is not positive
#' is_sym_pos_def(A)   # FALSE
#' 
#' A<-matrix(c(2,1,1,2),2,2)  # Both symmetric and eigen values are positive
#' is_sym_pos_def(A)   # TRUE
#' 
#' A<-matrix(c(4,2-3i,2+3i,3),2,2)  # Hermitian but one eigen value is not positive
#' is_sym_pos_def(A)   # FALSE

is_sym_pos_def<- function(A) { 
  if (all(A == Conj(t(A)))) {  # first test symmetricity
    if (all(eigen(A)$values>0)) {
      TRUE
    } else {
      FALSE   # not positive definite
    } 
  } else {
    FALSE
  }  # not symmetric
}