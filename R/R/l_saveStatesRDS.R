#' @title Save the info states of a loon plot widget as an RDS file
#'
#' @description \code{l_saveStatesRDS} uses \code{saveRDS()}
#' to save the info states of a loon plot as an \R object to the named file.
#' This is helpful, for example, when using RMarkdown or some other notebooking
#' facility to recreate an earlier saved loon plot so as to present it
#' in the document.
#'
#' @param p the `l_plot` object whose info states are to be saved.
#' @param states either the logical `TRUE` or a character vector of info states to be saved.
#' Default value `c("color", "active", "selected", "linkingKey", "linkingGroup")`
#' consists of `n` dimensional states that are common to many `l_plot`s
#' and which are most important to reconstruct the plot's display in any summary.
#' If `states` is the logical `TRUE`, by `names(p)` are saved.
#' @param file is a string giving the file name where the saved information'
#' will be written (custom suggests this file name end in the suffix `.rds`.
#' @param ... further arguments passed to \code{saveRDS()}.
#'
#' @return a list of class `l_savedStates` containing the states and their values.
#' Also has an attribute `l_plot_class` which contains the class vector of the
#' plot `p`
#'
#' @seealso \code{\link{l_copyStates}} \code{\link{l_info_states}} \code{\link{readRDS}} \code{\link{saveRDS}}
#'
#' @export
#'
#' @examples
#' #
#' # Suppose you some plot that you created like
#' p <- l_plot(iris, showGuides = TRUE)
#' # and coloured groups by hand (using the mouse and inspector)
#' # so that you ended up with these colours:
#' p["color"] <- rep(c( "lightgreen", "firebrick","skyblue"), each = 50)
#' # Having determined the colours you could save them (and other states)
#' # in a file of your choice, here some tempfile:
#' myFileName <- tempfile("myPlot", fileext = ".rds")
#' # Save the names states of p
#' l_saveStatesRDS(p,
#'                 states = c("color", "active", "selected"),
#'                 file = myFileName)
#' # These can later be retrieved and used on a new plot
#' # (say in RMarkdown) to set the new plot's values to those
#' # previously determined interactively.
#' p_new <- l_plot(iris, showGuides = TRUE)
#' p_saved_info <- readRDS(myFileName)
#' p_new["color"] <- p_saved_info$color
#' # The result is that p_new looks like p did
#' # (after your interactive exploration)
#' # and can now be plotted as part of the document
#' plot(p_new)
#'
l_saveStatesRDS <- function(p, states = c("color", "active", "selected",
                                          "linkingKey", "linkingGroup"),
                            file = stop("missing name of file"),
                            ...) {
  # ... are other arguments passed on to saveRDS()
  if (length(states) == 1) {
    if (states == TRUE)  {
      states <- "all"
    }
  }
  if (is(p, "l_compound")) {
    result <- lapply(l_getPlots(p),
                     function(plot) {
                       if ("all" %in% states) {
                         plot_states <- names(plot)
                       } else {
                         plot_states <- intersect(states, names(plot))
                       }
                       plot_result <- lapply(plot_states,
                                             function(state){
                                               plot[state]}
                       )
                       names(plot_result) <- plot_states
                       class(plot_result) <-  c("l_savedStates")
                       attr(plot_result, "l_plot_class") <- class(plot)
                       plot_result
                     })
    names(result) <- names(p)
  } else {
    if ("all" %in% states) {states <- names(p)}
    result <- lapply(states, function(state){p[state]})
    names(result) <- states
  }
  class(result) <- c("l_savedStates")
  attr(result, "l_plot_class") <- class(p)
  saveRDS(result, file = file, ...)
}
