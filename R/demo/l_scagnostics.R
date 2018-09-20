

if (requireNamespace("scagnostics", quietly = TRUE)) {
    
    local({
        ## Scatterplot Matrix of scagnostic measures
        data <- oliveAcids
        scags <- scagnostics::scagnostics(data)
        scdata <- as.data.frame(t(unclass(scags)))
        npairs <- dim(scdata)[1]
        variablePairs <- rownames(scdata)
        
        ps <- l_pairs(scdata, itemLabel=variablePairs)
        
        ## Scatterplot two variates
        p <- with(olive, l_plot(oleic~stearic, color=Area))
        
        ## select the above plot in the scatterplot matrix
        l_configure(ps[[1]], selected = TRUE, size = 12,
                    which = variablePairs=="stearic * oleic")
        
        scag2var <- function() {
            ## resize the point whose respective scatterplot is shown
            sel <- which(ps[[1]]['selected'])
            
            if (length(sel) > 0) {
                ## select the fist plot
                sel <- sel[1]
                varPair <- unlist(strsplit(variablePairs[sel], ' * ', fixed = TRUE))
                xvar <- varPair[1]
                yvar <- varPair[2]
                
                l_configure(p, x=data[[xvar]], y=data[[yvar]],
                            xlabel=xvar, ylabel=yvar)
                
                l_scaleto_world(p)
                
                ## otherwise you get nested configure evaluation for some plots
                l_after_idle(
                    function () {
                        ## enlarge point representing active scatterplot
                        size <- rep(4, npairs)
                        size[sel] <- 12
                        l_configure(ps[[1]], size=size)
                    })
            }
        }
        
        ## only need to add state binding for first scatterplot
        ## as all scatterplots are linked
        bnd <- l_bind_state(ps[[1]], 'selected', function() {scag2var()} )
        
    })
} else {
    cat(paste0('loon demo l_scagnostics should be evaluated by hand\nFind the demo file at: ',
               system.file('demo','l_scagnostics.R', package='loon'),'\n'))
}
