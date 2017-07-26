#' @title Add a <%=type%> binding
#' 
#' @description 
#' Creates a binding that evaluates a callback for particular changes in the
#' collection of <%=type%>s of a display.
#' 
#' @param callback callback function is an R function which is called by the Tcl
#'   interpreter if the event of interest happens. Note that in loon the 
#'   callback functions support different optional arguments depending on the
#'   binding type, read the details for more information
#' 
#' @details 
#' Bindings, callbacks, and binding substitutions are described in detail in
#' loon's documentation webpage, i.e. run \code{l_help("learn_R_bind")}
#' 
#' @return <%=type%> binding id
#' 
#' @seealso \code{\link{l_bind_<%=type%>_ids}}, \code{\link{l_bind_<%=type%>_get}},
#'   \code{\link{l_bind_<%=type%>_delete}}, \code{\link{l_bind_<%=type%>_reorder}}
#' 
