
#' @title Euclidean distance between two vectors,
#' or between column vectors of two matrices.
#'
#' @description Quickly calculates and returns the Euclidean distances between m vectors
#' in one set and n vectors in another.  Each set of vectors is given as
#' the columns of a matrix.
#'
#'
#' @param a A d by m numeric matrix giving the first set of m vectors of dimension d
#'          as the columns of \code{a}.
#' @param b A d by n numeric matrix giving the second set of n vectors of dimension d
#'          as the columns of \code{b}.
#' @param df Indicator whether to force the diagonals of the returned matrix to
#'           be zero (\code{df = 1}) or not (the default \code{df = 0}).
#'
#' @return An m by n matrix containing the Euclidean distances between the column
#' vectors of the matrix \code{a} and the column vectors of the matrix \code{b}.
#'
#' @details
#' This fully vectorized (VERY FAST!) function computes the Euclidean distance between
#' two vectors by:
#'
#'     ||A-B|| = sqrt ( ||A||^2 + ||B||^2 - 2*A.B )
#'
#' Originally written as L2_distance.m for Matlab by Roland Bunschoten of
#' the University of Amsterdam, Netherlands.
#'
#' @examples
#' A <- matrix(rnorm(400), nrow = 10)
#' B <- matrix(rnorm(800), nrow = 10)
#' L2_distance(A[,1, drop = FALSE], B[,1, drop = FALSE])
#' d_AB <- L2_distance(A,B)
#' d_BB <- L2_distance(B,B, df = 1) # force diagonal to be zero
#' @seealso \code{\link{dist}}
#'
#' @export
#'
#' @author Roland Bunschoten (original), Adrian Waddell, Wayne Oldford
#'
L2_distance <- function(a, b, df = 0){
#    A - (DxM) matrix
#    B - (DxN) matrix
#    df = 1, force diagonals to be zero; 0 (default), do not force
#
# Returns:
#    E - (MxN) Euclidean distances between vectors in A and B
#
#
# Description :
#    This fully vectorized (VERY FAST!) m-file computes the
#    Euclidean distance between two vectors by:
#
#                 ||A-B|| = sqrt ( ||A||^2 + ||B||^2 - 2*A.B )
#
# Example :
#    A = rand(400,100); B = rand(400,200);
#    d = distance(A,B);

# Author   : Roland Bunschoten
#            University of Amsterdam
#            Intelligent Autonomous Systems (IAS) group
#            Kruislaan 403  1098 SJ Amsterdam
#            tel.(+31)20-5257524
#            bunschot@wins.uva.nl
# Last Rev : Wed Oct 20 08:58:08 MET DST 1999
# Tested   : PC Matlab v5.2 and Solaris Matlab v5.3

# Copyright notice: You are free to modify, extend and distribute
#    this code granted that the author of the original code is
#    mentioned as the original author of the code.

# Fixed by JBT (3/18/00) to work for 1-dimensional vectors
# and to warn for imaginary numbers.  Also ensures that
# output is all real, and allows the option of forcing diagonals to
# be zero.

  ## if a is a vector convert it to a matrix
  if(is.matrix(a)==FALSE){
    a <- matrix(a) # This should be column vector ... rwo ... NOT  t(matrix(a))
  }
  ## if b is a vector convert it to a matrix
  if(is.matrix(b)==FALSE){
    b <- matrix(b) # This should be column vector ... rwo ... NOT  t(matrix(b))
  }


  if(dim(a)[1] != dim(b)[1]){
    stop("L2_distance: A and B should be of same dimensionality")
  }


  if (all(is.numeric(a), is.numeric(b)) == FALSE){
      # stop('Warning: running distance.m with imaginary numbers.  Results may be off.')
      stop('Warning: L2_distance requires numeric arguments.')
  }

  D <- dim(a)[1]
  if(D == 1){
    a <- rbind(a,rep(0,D))
    b <- rbind(b,rep(0,D))
  }

  aa <- matrix(apply(a*a,2,sum)); bb <- matrix(apply(b*b,2,sum)); ab <- t(a)%*%b


  ## copied from: http://haky-functions.blogspot.com/search/label/matlab2R
  repmat = function(X,m,n){
    ##R equivalent of repmat (matlab)
    mx = dim(X)[1]
    nx = dim(X)[2]
    matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=TRUE)
  }


  ## look that elements are either zero or positive
  d <- sqrt(apply(repmat(aa,1,dim(bb)[1]) +
                      repmat(t(bb),dim(aa)[1],1)-
                      2*ab,
                  c(1,2),
                  function(x){if(x>0){x}else{0}}))


  # force 0 on the diagonal?
  if (df==1){
    diag(d) <- 0
  }
  return(d)
}
