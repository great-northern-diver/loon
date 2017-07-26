

# Define a function addRegressionLines that takes a scatterplot
# handle as argument and creates a GUI to fit regression lines
# of a specific order to the selected points

local({

    addRegressionLinesGUI <- function(p) {
        force(p)
        addRegressionLine <- function() {
            sel <- p['selected']
            if (sum(sel)==0) return()
            xs <- p['x'][sel]; ys <- p['y'][sel]
            fit <- lm(ys ~ poly(xs, as.numeric(tclvalue(degree))))
            xrng <- seq(min(xs), max(xs), length.out = 20)
            ypred <- predict(fit, newdata=data.frame(xs = xrng))
            l_layer_line(p, x=xrng, y=ypred, color=as.character(color),
                         linewidth = 4, index=0, label=paste("degree", tclvalue(degree)))
            l_configure(p, color=color, glyph='ocircle', which=sel)
        }
        
        updateColor <- function() {
            col <- as.character(tcl('tk_chooseColor', initialcolor=color))
            if (col!='') {
                tkconfigure(b_col, bg=col, activebackground=col)
                color <<- col
            }
        }
        
        tt <- tktoplevel()
        tktitle(tt) <- 'Add Regression Line'
        degree <- tclVar('1')
        color <- 'red'
        s <- tkscale(tt, orient='horizontal', variable=degree,
                     from=1, to=8, resolution=1)
        b_col <- tkbutton(tt, bg=color, activebackground=color, command=updateColor)
        b_add <- tkbutton(tt, text='add', command=addRegressionLine)
        tkgrid(tklabel(tt, text='degree:'), s, b_col, b_add, sticky='s', pady=5)
        tkgrid.columnconfigure(tt, 1, weight=1)
        tkgrid.configure(s, sticky='ew')
    }  
    
    
    ## For example, for generated data
    x <- runif(500)*7
    y <- sapply(x, function(x) {
        if (0 <= x && x < 2) {
            5*x + rnorm(1,0,1)
        } else if (2 <= x && x < 5) {
            8.6 + 2*x-.6*x^2 + rnorm(1,0,.5)
        } else {
            8.5 - log(x) + rnorm(1,0,.8)
        }
    })
    
    p <- l_plot(x,y)
    addRegressionLinesGUI(p)
})