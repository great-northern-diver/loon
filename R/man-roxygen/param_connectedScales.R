#' @param connectedScales Determines how the scales of the facets are to be connected depending
#' on which \code{layout} is used.  For each value of \code{layout}, the scales are connected
#' as follows:
#' \itemize{
#' \item{\code{layout = "wrap":}  Across all facets, when \code{connectedScales} is
#'    \itemize{
#'    \item{\code{"x"}, then  only the "x"  scales are connected}
#'    \item{\code{"y"}, then only the "y" scales are connected}
#'    \item{\code{"both"},  both "x" and "y" scales are connected}
#'    \item{\code{"none"},  neither "x" nor "y" scales are connected.}
#'    For any other value, only the "y" scale is connected.
#'    }
#'    }
#' \item{\code{layout = "grid":}  Across all facets, when \code{connectedScales} is
#'    \itemize{
#'    \item{\code{"cross"}, then only the scales in the same row and the same column are connected}
#'    \item{\code{"row"}, then both "x" and "y" scales of facets in the same row are connected}
#'    \item{\code{"column"}, then both "x" and "y" scales of facets in the same column are connected}
#'    \item{\code{"x"}, then all of the "x"  scales are connected (regardless of column)}
#'    \item{\code{"y"}, then all of the "y" scales are connected (regardless of row)}
#'    \item{\code{"both"},  both "x" and "y" scales are connected in all facets}
#'    \item{\code{"none"},  neither "x" nor "y" scales are connected in any facets.}
#'    }
#'    }
#'  }
