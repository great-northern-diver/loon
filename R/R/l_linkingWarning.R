l_linkingWarning <- function(widget, sync = "pull", args, modifiedLinkedStates = character(0L), l_className = NULL) {

    if(sync == "push") return(invisible())

    deprecatedLinkedVar <- l_getDeprecatedLinkedVar(widget, args, modifiedLinkedStates, l_className)

    if(length(deprecatedLinkedVar) > 0)
        warning("The aesthetics ",
                paste(deprecatedLinkedVar, collapse = ", "),
                " will not be set, since they are pulled from the existing linked plots.",
                call. = FALSE)
}

l_getDeprecatedLinkedVar <- function(widget, args, modifiedLinkedStates = character(0L), l_className = NULL) {
    UseMethod("l_getDeprecatedLinkedVar", widget)
}

l_getDeprecatedLinkedVar.default <- function(widget, args, modifiedLinkedStates = character(0L), l_className = NULL) {

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
                                                        con1IsViolated <- var %in% modifiedLinkedStates
                                                        # convert color as hex code

                                                        if(var == "color") {

                                                            # `color` is not a real color
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
                                                            con2IsViolated <- any(state != widgetVar)
                                                        }

                                                        con1IsViolated && con2IsViolated

                                                    }, logical(1L))]

    deprecatedLinkedVar
}


l_getDeprecatedLinkedVar.list <- function(widget, args, modifiedLinkedStates = character(0L), l_className = NULL) {

    deprecatedLinkedVars <- lapply(widget,
                                   function(w) {
                                       l_getDeprecatedLinkedVar.default(w, args, modifiedLinkedStates, l_className)
                                   })

    unique(unlist(deprecatedLinkedVars))
}
