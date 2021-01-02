#' @title loon tk top level
#' @description Create a loon tk top-level window
#' @param path A valid path name (character);
#' if missing, a valid path will be generated automatically
#' @return a tk top level widget
#' @export
#' @examples
#' if(interactive()) {
#'
#'   tt <- l_toplevel(".test")
#'   subwin <- l_subwin(tt, 'ts')
#'   tktitle(tt) <- paste("path:", subwin)
#'   parent <- as.character(tcl('frame', subwin))
#'
#'   # a loon widget
#'   p <- l_plot(rnorm(100), rnorm(100), parent = parent)
#'   # pack a refresh button (generate new data set)
#'   refresh_button <- as.character(
#'      tcltk::tcl('button',
#'                 as.character(l_subwin(parent, 'refresh button')),
#'                 text = "refresh",
#'                 bg = "grey80",
#'                 fg = "black",
#'                 borderwidth = 2,
#'                 relief = "raised"))
#'
#'   # layout
#'   tcltk::tkgrid(p,
#'                 row = 0,
#'                 column = 0,
#'                 rowspan = 10,
#'                 columnspan = 10,
#'                 sticky="nesw")
#'
#'   tcltk::tkgrid(refresh_button,
#'                 row = 10,
#'                 column = 0,
#'                 rowspan = 1,
#'                 columnspan = 1,
#'                 sticky="nesw")
#'   for(i in 0:10) {
#'      tcltk::tkgrid.rowconfigure(parent, i, weight=1)
#'   }
#'   for(i in 0:9) {
#'     tcltk::tkgrid.columnconfigure(parent, i, weight=1)
#'   }
#'
#'   update <- function(...) {
#'     l_configure(p,
#'                 x = rnorm(100),
#'                 y = rnorm(100))
#'     l_scaleto_world(p)
#'   }
#'
#'   # configure button (callback function)
#'   tcltk::tkconfigure(refresh_button,
#'                      command = update)
#'   # configure canvas size
#'   tcltk::tkconfigure(paste(p,".canvas", sep=''), width=200, height=200)
#'
#'   # pack widgets
#'   tkpack(parent, fill="both", expand=TRUE)
#' }
l_toplevel <- function(path) {

    if(missing(path)) {
        i <- valid_path()
        path <- paste('.l', i , sep="")
    }

    tt <- as.character(tcl('toplevel',path))
    tcl('wm', 'iconphoto', tt, default='::loon::loonIcon')

    return(tt)
}


valid_path <- function() {
    i <- 0
    child <- paste('.l', i , sep="")
    while(as.logical(tcl('winfo','exists', child))) {
        i <- i + 1
        child <- paste('.l', i , sep="")
    }
    return(i)
}
