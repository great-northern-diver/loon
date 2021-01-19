l_linkingWarning <- function(widget, sync = "pull", args, l_className = NULL) {

    # If "sync" is push, no need to give warnings
    if(sync == "push") return(invisible())

    deprecatedLinkedVar <- l_getDeprecatedLinkedVar(widget, args, l_className)

    if(length(deprecatedLinkedVar) > 0)
        warning("The aesthetics ",
                paste(deprecatedLinkedVar, collapse = ", "),
                " will not be set, since they are pulled from the existing linked plots.",
                call. = FALSE)
}

l_getDeprecatedLinkedVar <- function(widget, args, l_className = NULL) {
    UseMethod("l_getDeprecatedLinkedVar", widget)
}

l_getDeprecatedLinkedVar.default <- function(widget, args, l_className = NULL) {

    if(is.null(l_className)) l_className <- class(widget)[1L]

    linkableStatesVar <- hasDefaultLinkableStatesVar(l_className)

    deprecatedLinkedVar <- linkableStatesVar[vapply(linkableStatesVar,
                                                    function(var) {
                                                        state <- args[[var]]
                                                        if(is.null(state)) return(FALSE)

                                                        uniqueState <- unique(state)
                                                        # Why warnings? Both conditions must be "required"
                                                        #   1. the input linked states are not default
                                                        #   2. the input linked states are not equal to the plot current states

                                                        widgetVar <- widget[var]
                                                        # convert color as hex code
                                                        if(var == "color") {

                                                            # `color` is not a real color
                                                            # it could be a substitution
                                                            con1IsViolated <- if(length(uniqueState) > 1) {
                                                                TRUE
                                                            } else {
                                                                tryCatch(
                                                                    expr = {
                                                                        as_hex6color(uniqueState) != as_hex6color(l_getOption(var))
                                                                    },
                                                                    error = function(e) {
                                                                        TRUE
                                                                    }
                                                                )
                                                            }

                                                            con2IsViolated <- tryCatch(
                                                                expr = {
                                                                    any(hex12tohex6(widgetVar) != as_hex6color(state))
                                                                },
                                                                error = function(e) {

                                                                    levels1 <- levels(factor(widgetVar))
                                                                    levels2 <- levels(factor(state))

                                                                    # state is not a real color, instead it is a substitution
                                                                    ## match factors
                                                                    any(vapply(seq(length(widgetVar)),
                                                                           function(i) {
                                                                               which(levels1 %in% widgetVar[i]) !=
                                                                                   which(levels2 %in% state[i])
                                                                           }, logical(1L)))
                                                                }
                                                            )


                                                        } else {

                                                            con1IsViolated <- if(length(uniqueState) > 1) {
                                                                TRUE
                                                            } else {
                                                                switch(var,
                                                                       "active" = {
                                                                           # default is TRUE
                                                                           !uniqueState
                                                                       },
                                                                       "selected" = {
                                                                           # default is FALSE
                                                                           uniqueState
                                                                       },
                                                                       {
                                                                           # default
                                                                           any(as.character(uniqueState) != l_getOption(var))
                                                                       }
                                                                )
                                                            }

                                                            con2IsViolated <- any(state != widgetVar)
                                                        }

                                                        con1IsViolated && con2IsViolated

                                                    }, logical(1L))]

    deprecatedLinkedVar
}


l_getDeprecatedLinkedVar.list <- function(widget, args, l_className = NULL) {

    deprecatedLinkedVars <- lapply(widget,
                                   function(w) {
                                       l_getDeprecatedLinkedVar.default(w, args, l_className)
                                   })

    unique(unlist(deprecatedLinkedVars))
}
