#' @title Get the event pattern and Tcl code of a <%=type%> binding
#' 
#' @description This function returns the registered event pattern and the Tcl
#'   callback code that the Tcl interpreter evaluates after a event occurs that
#'   machtches the event pattern.
#' 
#' @param widget widget path as a string or as an object handle
#' @param id <%=type%> binding id
#'   
#' @return Character vector of length two. First element is the event pattern,
#'   the second element is the Tcl callback code.

