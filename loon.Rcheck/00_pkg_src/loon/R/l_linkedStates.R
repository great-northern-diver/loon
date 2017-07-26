
#' @title Modify States of a Plot that are Linked in Loon's Standard Linking 
#'   Model
#'   
#' @description Loon's standard linking model is based on three levels, the 
#'   \code{linkingGroup} and \code{linkingKey} states and the \emph{used
#'   linkable states}. See the details below.
#'   
#' @template param_widget
#' @param states used linkable state names, see in details below
#'   
#' @details Loon's standard linking model is based on two states, 
#'   \code{linkingGroup} and \code{linkingKey}. The full capabilities of the 
#'   standard linking model are described here. However, setting the 
#'   \code{linkingGroup} states for two or more displays to the same string is 
#'   generally all that is needed for linking displays that plot data from the 
#'   same data frame. Changing the linking group of a display is also the only 
#'   linking-related action available on the analysis inspectors.
#'   
#'   The first linking level is as follows: loon's displays are linked if they 
#'   share the same string in their \code{linkingGroup} state. The default 
#'   linking group \code{'none'} is a keyword and leaves a display un-linked.
#'   
#'   The second linking level is as follows. All \code{n}-dimensional states can
#'   be linked between displays. We call these states \emph{linkable}. Further, 
#'   only linkable states with the same name can be linked between displays. One
#'   consequence of this \emph{shared state name} rule is that, with the 
#'   standard linking model, the \code{linewidth} state of a serialaxes display 
#'   cannot be linked with the \code{size} state of a scatterplot display. Also,
#'   each display maintains a list that defines which of its linkable states 
#'   should be used for linking; we call these states the \emph{used linkable} 
#'   states. The default used linkable states are as follows
#'   
#'   \tabular{ll}{
#'   
#'   Display \tab Default \emph{used linkable} states\cr
#'   
#'   scatterplot \tab \code{selected}, \code{color}, \code{active}, 
#'   \code{size}\cr histogram \tab \code{selected}, \code{color}, 
#'   \code{active}\cr
#'   
#'   serialaxes \tab \code{selected}, \code{color}, \code{active}\cr
#'   
#'   graph \tab \code{selected}, \code{color}, \code{active}, \code{size}
#'   
#'   }
#'   
#'   If any two displays are set to be linked (i.e. they share the same linking 
#'   group) then the intersection of their \emph{used linkable} states are 
#'   actually linked.
#'   
#'   The third linking level is as follows. Every display has a 
#'   \code{n}-dimensional \code{linkingKey} state. Hence, every data point has 
#'   an associated linking key. Data points between linked plots are linked if 
#'   they share the same linking key.
#'   
#' @export
l_setLinkedStates <- function(widget, states) {
    invisible(as.character(tcl(widget, 'setLinkedStates', states)))
}


#' @title Query the States that are Linked with Loon's Standard Linking Model
#'   
#' @description Loon's standard linking model is based on three levels, the 
#'   \code{linkingGroup} and \code{linkingKey} states and the \emph{used 
#'   linkable states}. See the details in the documentation for
#'   \code{\link{l_setLinkedStates}}.
#'    
#' @template param_widget
#'   
#' @return vector with state names that are linked states
#'   
#' @seealso \code{\link{l_setLinkedStates}}
#'   
#' @export
l_getLinkedStates <- function(widget) {
    as.character(tcl(widget, 'getLinkedStates'))
}
