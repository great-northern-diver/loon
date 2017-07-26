#' @title Get the event pattern and callback Tcl code of a <%=type%> binding
#' 
#' @description This function returns the registered event pattern and the Tcl
#'   callback code that the Tcl interpreter evaluates after a event occurs that
#'   machtches the event pattern.
#' 
#' @param id <%=type%> binding id
#'
#' @details 
#' Bindings, callbacks, and binding substitutions are described in detail in
#' loon's documentation webpage, i.e. run \code{l_help("learn_R_bind")}
#' 
#' @return Character vector of length two. First element is the event pattern,
#'   the second element is the Tcl callback code.
#'   
#' @seealso \code{\link{l_bind_<%=type%>}}, \code{\link{l_bind_<%=type%>_ids}},
#'   \code{\link{l_bind_<%=type%>_delete}}, \code{\link{l_bind_<%=type%>_reorder}}
