# check whether the default linkable states are modified

l_modifiedLinkedStates <- function(l_className, vars) {

    linkableStatesVar <- hasDefaultLinkableStatesVar(l_className)

    linkableStatesVar[vapply(linkableStatesVar,
                             function(var) {

                                 var %in% vars

                             }, logical(1L))]
}
