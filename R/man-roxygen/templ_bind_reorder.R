#' @title Reorder the <%=type%> binding evaluation sequence
#' 
#' @description The order the <%=type%> bindings defines how they get evaluated 
#'   once an event matches event patterns of multiple <%=type%> bindings.
#' 
#' @param ids new <%=type%> binding id evaluation order, this must be a 
#'   rearrangement of the elements returned by the
#'   \code{\link{l_bind_<%=type%>_ids}} function.
#' 
#' @details 
#' Bindings, callbacks, and binding substitutions are described in detail in
#' loon's documentation webpage, i.e. run \code{l_help("learn_R_bind")}
#' 
#' @return vector with binding id evaluation order (same as the \code{id} argument)
#' 
#' @seealso \code{\link{l_bind_<%=type%>}}, \code{\link{l_bind_<%=type%>_ids}},
#'   \code{\link{l_bind_<%=type%>_get}}, \code{\link{l_bind_<%=type%>_delete}}
#' 
