linkOneDimensionalStates <- function(plots, oneDimensionalStates = c("showScales", "showLabels", "showGuides")) {

    # 'n_dim_states_list' is a global variable in loon NAMESPACE
    nDimentionalStates <- unique(unlist(n_dim_states_list))
    if(any(oneDimensionalStates %in% nDimentionalStates))
        stop(oneDimensionalStates, " are not one dimensional states")

    lapply(plots,
           function(p) {
               l_bind_state(p, oneDimensionalStates,
                            function() {

                                for(s in oneDimensionalStates) {
                                    ps <- p[s]
                                    lapply(plots,
                                           function(pp) {
                                                   pp[s] <- ps
                                           })
                                }


                            }
               )
           })

    return(invisible())
}
