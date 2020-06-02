# default aesthetics attributes in loon
default_color <- "grey60"
default_size <- 4
default_glyph <- "ccircle"
default_linewidth <- 1


l_setDefaultSync <- function(p = NULL, sync = NULL, ...) {

    if(!is.null(sync)) return(sync)

    if(is.null(p)) return("pull")

    UseMethod("l_setDefaultSync", p)
}

l_setDefaultSync.default <- function(p, sync = NULL, ...) {

    # defult sync
    sync <- "pull"

    linked_states <- if(inherits(p, "l_compound")) {
        unique(
            unlist(
                lapply(p,
                       function(plot) {
                           l_getLinkedStates(plot)
                       })
            )
        )
    } else {
        l_getLinkedStates(p)
    }

    args <- Filter(Negate(is.null), list(...))

    len_args <- length(args)
    if(len_args == 0) return(sync)

    modified_states <- lapply(names(args),
                              function(name) {
                                  modified <- FALSE
                                  arg <- args[[name]]

                                  unique_arg <- unique(arg)
                                  if(length(unique_arg) > 1)
                                      modified <- TRUE
                                  else {
                                      # global variables defined in namespace loon
                                      if(unique_arg != getFromNamespace(paste("default", name, sep = "_"), "loon"))
                                          modified <- TRUE
                                  }

                                  if(modified) return(name) else NULL
                              })

    modified_states <- unlist(modified_states)

    if(length(modified_states) == 0) return(sync)

    if(any(modified_states %in% linked_states))
        sync <- "push"

    return(sync)
}
