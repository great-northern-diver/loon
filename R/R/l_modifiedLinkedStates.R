# check whether the default linkable states are modified

l_modifiedLinkedStates <- function(l_className, args) {

    linkableStatesVar <- hasDefaultLinkableStatesVar(l_className)

    linkableStatesVar[vapply(linkableStatesVar,
                             function(var) {

                                 !is.null(args[[var]])

                             }, logical(1L))]
}
