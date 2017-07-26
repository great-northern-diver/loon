
# Define a function power that creates a scatterplot with
# two scales to transform the axes.

local({
    
    power <- function(x, y, from=-5, to=5, ...) {
        
        tt <- tktoplevel()
        tktitle(tt) <- "Box-Cox Power Transformation"
        p <- l_plot(x=x, y=y, parent=tt, ...)
        lambda_x <- tclVar('1')
        lambda_y <- tclVar('1')
        sx <- tkscale(tt, orient='horizontal',
                      variable=lambda_x, from=from, to=to, resolution=0.1)
        sy <- tkscale(tt, orient='vertical',
                      variable=lambda_y, from=to, to=from, resolution=0.1)
        
        tkgrid(sy, row=0, column=0, sticky="ns")
        tkgrid(p, row=0, column=1, sticky="nswe")
        tkgrid(sx, row=1, column=1, sticky="we")
        tkgrid.columnconfigure(tt, 1, weight=1)
        tkgrid.rowconfigure(tt, 0, weight=1)
        
        powerfun <- function(x, lambda) {
            if (lambda == 0)
                log(x)
            else
                (x^lambda-1)/lambda
        }
        
        update <- function(...) {
            l_configure(p,
                        x = powerfun(x, as.numeric(tclvalue(lambda_x))),
                        y = powerfun(y, as.numeric(tclvalue(lambda_y))))
            l_scaleto_world(p)
        }
        
        tkconfigure(sx, command=update)    
        tkconfigure(sy, command=update)    
        
        invisible(p)    
    }
    
    p <- with(MASS::mammals, power(body, brain,
                                   xlabel="body weight",
                                   ylabel="brain weight",
                                   title="Brain and Body Weights for 62 Species of Land Mammals",
                                   itemLabel=rownames(MASS::mammals),
                                   showItemLabels=TRUE))
    
})
