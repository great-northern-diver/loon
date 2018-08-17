#' @title 2d navigation graph setup with with dynamic node fitering using a 
#'   slider
#'   
#' @description Generic function to create a navigation graph environment where 
#'   user can filter graph nodes using as slider to select 2d spaces based on 2d
#'   measures.
#'   
#' @inheritParams l_ng_plots
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section l_ng_ranges
#' @template see_l_help
#' 
#' @seealso \code{\link{l_ng_ranges.default}}, \code{\link{l_ng_ranges.measures}},
#'   \code{\link{l_ng_ranges.scagnostics}}, \code{\link{measures1d}},
#'   \code{\link{measures2d}}, \code{\link{scagnostics2d}},
#'   \code{\link{l_ng_ranges}}
#' 
#' @export
l_ng_ranges <- function(measures, ...) {
    UseMethod("l_ng_ranges")
}


#' @title Select 2d spaces with variable associated measures using a slider
#'   
#' @description Measures object is a matrix or data.frame with measures 
#'   (columns) for variable pairs (rows) and rownames of the two variates 
#'   separated by separator   
#'   
#' @inheritParams l_ng_plots.default
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section l_ng_ranges
#' @template see_l_help
#' 
#' @template return_l_ng
#'         
#' @seealso \code{\link{l_ng_ranges}}, \code{\link{l_ng_ranges.measures}}, 
#'   \code{\link{l_ng_ranges.scagnostics}}, \code{\link{measures1d}}, 
#'   \code{\link{measures2d}}, \code{\link{scagnostics2d}}, 
#'   \code{\link{l_ng_ranges}}
#' 
#' @export
#' 
#' @examples 
#' # Simple example with generated data
#' n <- 100
#' dat <- data.frame(
#'    A = rnorm(n), B = rnorm(n), C = rnorm(n),
#'    D = rnorm(n), E = rnorm(n)
#' )
#' m2d <- data.frame(
#'     cor = with(dat, c(cor(A,B), cor(A,C), cor(B,D), cor(D,E), cor(A,E))),
#'     my_measure = c(1, 3, 2, 1, 4),
#'     row.names = c('A:B', 'A:C', 'B:D', 'D:E', 'A:E')
#' )
#' 
#' # or m2d <- as.matrix(m2d)
#' 
#' nav <- l_ng_ranges(measures=m2d, data=dat)
#' 
#' # With 1d measures
#' m1d <- data.frame(
#'      mean = sapply(dat, mean),
#'      median =  sapply(dat, median),
#'      sd =  sapply(dat, sd),
#'      q1 = sapply(dat, function(x)quantile(x, probs=0.25)),
#'      q3 = sapply(dat, function(x)quantile(x, probs=0.75)),
#'      row.names = names(dat)
#' )
#' 
#' nav <- l_ng_ranges(m1d, dat) 



l_ng_ranges.default <- function(measures, data, separator=':', ...) {

    force(data)
    
    if (grepl(" ", separator, fixed=TRUE))
        stop("separator can not contain spaces")

    if (is.vector(measures)) {
        if(is.null(names(measures))) 
            stop("measure vector requires names")
        node_spaces <- names(measures)
    } else {
        if(is.null(row.names(measures))) 
            stop("measures require row names")
        node_spaces <- row.names(measures)        
    }
    
    if (any(grepl(" ", node_spaces, fixed=TRUE)))
        stop("row names of measures can not contain spaces")
    
    variables <- strsplit(node_spaces, separator, fixed=TRUE)
    if (any(!(unlist(variables) %in% names(data))))
        stop("variables in measures do not exist in the data")
    
    dim <- unique(vapply(variables, length, numeric(1)))
    
    if (length(dim)>1) 
        stop(paste('not all row names of the measures split into variable',
                   'spaces of same dimensions using the separator.'))
    
    if (dim != 1 && dim != 2)
        stop(paste('l_ng_ranges only knows how to deal with 1 or 2 dimensional',
                   'nodes. Found dimension is ', dim))
    
    ## -----------
    ## Create Graph, Navigator, Context, and Controls
    tt <- l_toplevel()

    tcl('wm', 'title', tt, 'Navgraph based on measures with a top fraction slider')
    
    controlFrame <- tcl('frame', l_subwin(tt, 'controls'))
    g <- l_graph(parent=tt)
    nav <- l_navigator_add(g)
    g2d <- l_context_add_geodesic2d(navigator = nav, data=data, separator=separator)
    p <- structure(unlist(strsplit(g2d['command'],' ', fixed=TRUE))[1], class=c ('l_plot', 'loon'))
    
    args <- list(...)
    if (length(args) > 0) {
        l_configure(p, x=data[,1], y=data[,2], ...)        
    } else {
        l_configure(p, x=data[,1], y=data[,2])
    }
    
    tcl('pack', controlFrame, side='right', fill='y', padx=2, pady=2)
    tcl('pack', g, side='right', fill='both', expand=TRUE)

    tcl('grid', 'propagate', controlFrame, 0)
    tcl(controlFrame, 'configure', width=200)
    

    labelMeasure <- tcl('label', l_subwin(controlFrame, 'mlabel'), text='Measures:') 
    listbox <- tcl('tk::listbox', l_subwin(controlFrame,'listbox'), selectmode='single')
    scrollListbox <- tcl('tk::scrollbar', l_subwin(controlFrame,'lvscroll'),
                         orient='vertical', command=paste(listbox,'yview'))
    tcl(listbox,'configure', yscrollcommand=paste(scrollListbox,'set'))
    labelFilter <- tcl('label', l_subwin(controlFrame, 'scalelabel'), text='Filter:')
    scale <- tcl('loon::minmax_scale', l_subwin(controlFrame,'scale'), min=0.8, max=1)
    labelTransition <- tcl('label', l_subwin(controlFrame, 'tl'), text='Transition Graph:')
    frameTransitionRadiobuttons <- tcl('frame', l_subwin(controlFrame,'transitionDim'))
    tclvalueTransitionDim <- tclVar('3d')
    rb_3d <- tcl('radiobutton', l_subwin(frameTransitionRadiobuttons, 'rb3d'), text='3d',
                 variable=tclvalueTransitionDim, value='3d')
    rb_4d <- tcl('radiobutton', l_subwin(frameTransitionRadiobuttons, 'rb4d'), text='4d',
                 variable=tclvalueTransitionDim, value='4d')
    rb_34d <- tcl('radiobutton', l_subwin(frameTransitionRadiobuttons, 'rb34d'), text='3d & 4d',
                  variable=tclvalueTransitionDim, value='34d')
    tcl('pack', rb_3d, rb_4d, rb_34d, side='left')
    

    frameFilterRadiobuttons <- tcl('frame', l_subwin(controlFrame,'filterSpec'))
    tclvalueFilter <- tclVar('value')
    rb_value <- tcl('radiobutton', l_subwin(frameFilterRadiobuttons, 'value'), text='value',
                 variable=tclvalueFilter, value='value')
    rb_percentile <- tcl('radiobutton', l_subwin(frameFilterRadiobuttons, 'percentile'), text='percentile',
                 variable=tclvalueFilter, value='percentile')
    tcl('pack', rb_value, rb_percentile, side='left')
        
    tcl('grid', labelMeasure, row=0, column=0, columnspan=2, sticky='w')
    tcl('grid', listbox, row=1, column=0, sticky='nsew')
    tcl('grid', scrollListbox, row=1, column=1, sticky='ns')
    
    tcl('grid', labelTransition, row=2, column=0, columnspan=2, sticky='w', pady = c(8,2))
    tcl('grid', frameTransitionRadiobuttons, row=3, column=0, columnspan=2, sticky='w')
    
    tcl('grid', labelFilter, row=4, column=0, columnspan=2, sticky='w', pady = c(8,0))
    tcl('grid', frameFilterRadiobuttons, row=5, column=0, columnspan=2, sticky='w')
    tcl('grid', scale, row=6, column=0, columnspan=2, sticky='ew')
    
    tcl('grid', 'rowconfigure', controlFrame, 1, weight= 1)
    
    tcl('grid', 'columnconfigure', controlFrame, 0, weight=1)
    tcl('grid', 'columnconfigure', controlFrame, 1, weight=0)
    
    ## Now Create Listbox Labels
    sapply(colnames(measures), function(measure) tcl(listbox, 'insert' , 'end', measure))
    ## -----updateScale
    
    i_last_select <- 0
    
    ## Now write function to update graph
    updateGraph <- function() {
        
        i_measure <- as.character(tcl(listbox, 'curselection'))
        if (length(i_measure) == 0) {
              tcl(listbox, 'selection', 'set', i_last_select)   
        } else {
            i_last_select <<- as.numeric(i_measure)[1]              
        }
        i_measure <- i_last_select + 1
        
        scale_min <- as.numeric(tkcget(scale, "-min"))
        scale_max <- as.numeric(tkcget(scale, "-max"))
        
        ## Create Graph
        y <- measures[,i_measure]
        
        if (tclvalue(tclvalueFilter) == "value") {
            spaces <- node_spaces[y >= scale_min & y <= scale_max]
        } else {
            quan <- stats::quantile(y, probs=c(scale_min, scale_max))
            spaces <- node_spaces[y >= quan[1] & y <= quan[2]]
        }
        
        if (dim == 1) {
            if(length(spaces) >= 2)
                nodes <- apply(utils::combn(spaces, 2), 2, function(col)paste(col, collapse=separator))
            else
                nodes <- ""
        } else {
            nodes <- spaces 
        }

        transitionDim <- tclvalue(tclvalueTransitionDim)
        switch(transitionDim,
               "3d" = {
                   G <- ndtransitiongraph(nodes = nodes, n=3, separator = separator)
               },
               "4d" = {
                   G <- ndtransitiongraph(nodes = nodes, n=4, separator = separator)
               }, 
               "34d" = {
                   G <- ndtransitiongraph(nodes = nodes, n=c(3, 4), separator = separator)
               },
               stop(paste0('transitionDim is wrong: ', transitionDim))
        )
        
        l_configure(g, nodes=G$nodes, from=G$from, to=G$to, isDirected=G$isDirected)
        l_scaleto_world(g)
        l_zoom(g, 0.8)
        
        # cat(paste0('update graph.\n  measure: ',names(measures)[i_measure],
        #            '\n  transition: ', transitionDim,
        #            '\n  top fraction: ', topfraction,'\n'))
    }
    
    
    updateScale <- function() {
        
        if (tclvalue(tclvalueFilter) == "value") {
        
            i_measure <- as.character(tcl(listbox, 'curselection'))
            if (length(i_measure) == 0) {
                  tcl(listbox, 'selection', 'set', i_last_select)   
            } else {
                i_last_select <<- as.numeric(i_measure)[1]               
            }
            i_measure <- i_last_select + 1

            ## Find some suitable settings for the min-max sacle
            y <- measures[,i_measure]
            y_min <- min(y)
            y_max <- max(y)
            
            r_y <- diff(range(y))
            
            y_res <- r_y/100
            
            if(y_res > 1)
                y_res <- floor(r_y)
            else if (y_res > 0.01)
                y_res <- 0.01
            else if (y_res > 0.001)
                y_res <- 0.001
            else if (y_res > 0.0001)
                y_res <- 0.0001
            
            s_max <- y_max
            s_min <- y_min + .8*r_y 

            if (s_min > s_max)
                s_min <- y_min
            
            #cat(paste0('from=', y_min,' to=', y_max,' min=', s_min,' max=', s_max,' resolution=',y_res, '\n'))
            tkconfigure(scale, from=y_min, to=y_max, min=s_min, max=s_max, resolution=y_res)
            
        } else {
            tkconfigure(scale, from=0, to=1, min=0.8, max=1, resolution=0.01)
        }
        
        # updateGraph()
    }

    
    tcl('bind', listbox, '<<ListboxSelect>>', function(...) {
        
        sel <- as.character(tcl(listbox, 'curselection'))
        if (length(sel) != 0 && !identical(i_last_select, sel)) {
            updateScale()
        }
    })
    
    
    sapply(c(rb_value, rb_percentile), function(rb) {
        tcl(rb, 'configure', command=function(...) updateScale())        
    })

    
    tcl(scale, 'configure', command=function(...) updateGraph())
    
    sapply(c(rb_3d, rb_4d, rb_34d), function(rb) {
        tcl(rb, 'configure', command=function(...) updateScale())        
    })

    
    callbackFunctions$general[[paste0(g,'.updateGraph')]] <- updateGraph
    callbackFunctions$general[[paste0(g,'.updateScale')]] <- updateScale
    
    tcl(listbox, 'selection', 'set', 0)

    
    updateScale()
    
    l_scaleto_world(p)
    
    plot <- list(
        graph=g,
        plot=p,
        navigator=nav,
        context=g2d,
        env=environment()
    )
    
    class(plot) <- c("l_ng_ranges", "l_navgraph", "l_compound", "loon")
    
    return(plot)
}


#' @title 2d Navigation Graph Setup with dynamic node fitering based on 
#'   scagnostic measures and using a slider
#'   
#' @description This method is useful when working with objects from the 
#'   \code{\link[scagnostics]{scagnostics}} function from the scagnostics \R 
#'   package. In order to dynamically re-calcultate the scagnostic measures for 
#'   a subset of the data use the \code{\link{scagnostics2d}} measures creature 
#'   function.
#'   
#' @inheritParams l_ng_plots.scagnostics
#' 
#' @templateVar page  learn_R_display_graph
#' @templateVar section l_ng_ranges
#' @template see_l_help
#'         
#' @template return_l_ng
#' 
#' @seealso \code{\link{l_ng_ranges}}, \code{\link{l_ng_ranges.default}}, 
#'   \code{\link{l_ng_ranges.measures}}, \code{\link{measures1d}}, 
#'   \code{\link{measures2d}}, \code{\link{scagnostics2d}}, 
#'   \code{\link{l_ng_ranges}}
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' if (requireNamespace("scagnostics", quietly = TRUE)) {
#'   s <- scahnostics::scagnostics(oliveAcids)
#'   ng <- l_ng_ranges(s, oliveAcids, color=olive$Area) 
#' } 
#' }
l_ng_ranges.scagnostics <- function(measures, data, separator=":", ...) {
    
    force(data)
    
    requireNamespace("scagnostics", quietly = TRUE) || stop("the scagnostics package is required for this method.")
    grid <- scagnostics::scagnosticsGrid(measures)
    
    measures <- t(unclass(measures))
    
    # Check if data and measures are consistent  
    node_spaces <- apply(grid, 1, function(row) {
        paste(names(data)[row], collapse = ' * ')
    })
    
    if (any(node_spaces != row.names(measures))) {
        stop('The data is not the same as used in the scagnostics function')
    }
    
    row.names(measures) <- sub(" * ", separator, row.names(measures), fixed = TRUE)
    
    l_ng_ranges.default(measures, data, separator, ...)
}



#' @title 2d Navigation Graph Setup with dynamic node fitering using a slider
#'   
#' @description Measures object is of class measures. When using measure objects
#'   then the measures can be dynamically re-calculated for a subset of the 
#'   data.
#'   
#' @inheritParams l_ng_plots.measures
#'   
#' @details Note that we provide the \code{\link{scagnostics2d}} function to 
#'   create a measures object for the scagnostics measures.
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section l_ng_ranges
#' @template see_l_help
#'   
#' @seealso \code{\link{measures1d}}, \code{\link{measures2d}}, 
#'   \code{\link{scagnostics2d}}, \code{\link{l_ng_ranges}},
#'   \code{\link{l_ng_plots}}
#' 
#' @template return_l_ng
#'
#' @export
#'
#' @examples 
#' # 2d measures
#' # s <- scagnostics2d(oliveAcids)
#' # nav <- l_ng_ranges(s, color=olive$Area)
#' 
#' # 1d measures
#' scale01 <- function(x){(x-min(x))/diff(range(x))}
#' m1d <- measures1d(sapply(iris[,-5], scale01),
#'      mean=mean, median=median, sd=sd,
#'      q1=function(x)as.vector(quantile(x, probs=0.25)),
#'      q3=function(x)as.vector(quantile(x, probs=0.75)))
#' 
#' m1d()
#' 
#' nav <- l_ng_ranges(m1d, color=iris$Species)
l_ng_ranges.measures <- function(measures, ...) {
    
    separator <- measures('separator')
    
    objs <- l_ng_ranges.default(measures = measures(),
                                data = measures('data'), 
                                separator = separator,
                                ...)
    
    ## ---- with measure table but not with measures2d
    envir <- objs$env
    updateMeasures <- function(basedOn) {
        cat('update Measures\n')
        switch(basedOn,
               active = {
                   if(sum(objs$plot['active']) == 0) {
                       cat("No points active\n")
                       return()
                   }
                   assign('measures', measures(objs$plot['active']), envir = envir)
               },
               selected = {
                   if(sum(objs$plot['selected']) == 0) {
                       cat("No points selected\n")
                       return()
                   }
                   assign('measures', measures(objs$plot['selected']), envir = envir)
               })
            do.call("updateGraph", list(), envir = envir)
            cat('   done\n')
    }
    
    
    callbackFunctions$general[[paste0(objs$graph,'.updateMeasures')]] <- updateMeasures

    controlFrame <- get('controlFrame', envir=objs$env)
    
    cback <- unlist(strsplit(.Tcl.callback(updateMeasures),' ', fixed=TRUE))[1:2]
    
    basedon_lab <- tcl('label', l_subwin(controlFrame, 'basedonlabel'), text='Recalulate Measures based on:')
    bframe <- tcl('frame', l_subwin(controlFrame,'basedon'))
    bactive <- tcl('button', l_subwin(bframe, 'active'), text='active',
                   command = paste(c(cback, 'active'), collapse = ' '))
    bselected <- tcl('button', l_subwin(bframe, 'selected'), text='selected',
                     command=paste(c(cback, 'selected'), collapse = ' '))
    tcl('pack', bactive, bselected, side='left')
    
   
    tcl('grid', basedon_lab, row=7, column=0, columnspan=2, sticky='w',
        pady = c(8,2) )
    tcl('grid', bframe, row=8, column=0, columnspan=2, sticky='w')
    
    
    return(objs)
}
