# check whether the default linkable states are modified

l_modifiedLinkedStates <- function(l_className, args) {

    linkableStatesVar <- hasDefaultLinkableStatesVar(l_className)

    linkableStatesVar[vapply(linkableStatesVar,
                             function(var) {

                                 # if the state is missing
                                 # it must not be modified
                                 state <- args[[var]]
                                 if(is.null(state)) return(FALSE)

                                 switch(var,
                                        "color" = {
                                            tryCatch(
                                                expr = {
                                                    any(as_hex6color(state) != as_hex6color(l_getOption(var)))
                                                },
                                                error = function(e) {
                                                    # the state is substitution
                                                    # it must be modified
                                                    TRUE
                                                }
                                            )
                                        },
                                        "selected" = {
                                            # default is FALSE
                                            any(state)
                                        },
                                        "active" = {
                                            # default is TRUE
                                            !all(state)
                                        },
                                        {
                                            any(as.character(state) != l_getOption(var))
                                        }
                                 )
                             }, logical(1L))]
}
