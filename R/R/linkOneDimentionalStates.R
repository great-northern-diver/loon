linkOneDimentionalStates <- function(plots, oneDimentionalStates = "showScales") {

    # 'n_dim_states_list' is a global variable in loon NAMESPACE
    nDimentionalStates <- unique(unlist(n_dim_states_list))
    if(any(oneDimentionalStates %in% nDimentionalStates))
        stop(oneDimentionalStates, " are not one dimensional states")

    # The outside loop may be not necessary
    # fix me if you can make it more efficient
    lapply(plots,
           function(p) {
               l_bind_state(p, oneDimentionalStates,
                            function() {
                                lapply(plots,
                                       function(pp) {
                                           for(s in oneDimentionalStates) {
                                               pp[s] <- p[s]
                                           }
                                       })
                            }
               )
           })

    return(invisible())
}
