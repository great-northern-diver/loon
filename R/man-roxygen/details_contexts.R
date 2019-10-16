#' Contexts implement standard graph semantics. Common to all contexts is that
#' they sign up to the navigators state changes and will evaluate its (i.e. the
#' context's) command state. The contexts add substitution in the command
#' evaluation that are meaningful for the particular context. Currently the
#' \code{Context2d}, \code{Geodesic2d}, and \code{Slicing2d} are available as
#' part of the loon package.
#' 
#' Contexts are in more detail explained in the webmanual accessible with 
#' \code{\link{l_help}}. Please read the section on context by running 
#' \code{l_help("learn_R_display_graph.html#contexts")}.
