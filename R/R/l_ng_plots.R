
#' @title 2d navigation graph setup with with dynamic node fitering using a 
#'   scatterplot matrix
#'   
#' @description Generic function to create a navigation graph environment where 
#'   user can filter graph nodes by selecting 2d spaces based on 2d measures 
#'   displayed in a scatterplot matrix.
#'   
#' @param measures object with measures are stored
#' @param ... argument passed on to methods
#'   
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section l_ng_plots
#' @template see_l_help
#'   
#' @export
#' 
#' @seealso \code{\link{l_ng_plots.default}}, \code{\link{l_ng_plots.measures}},
#'   \code{\link{l_ng_plots.scagnostics}}, \code{\link{measures1d}},
#'   \code{\link{measures2d}}, \code{\link{scagnostics2d}},
#'   \code{\link{l_ng_ranges}}
#'   
l_ng_plots <- function(measures, ...) {
    UseMethod("l_ng_plots")
}

#' @title Select 2d spaces with variable associated measures displayed in 
#'   scatterplot matrix
#'   
#' @description Measures object is a matrix or data.frame with measures 
#'   (columns) for variable pairs (rows) and rownames of the two variates 
#'   separated by separator
#'   
#' @param measures matrix or data.frame with measures (columns) for variable 
#'   pairs (rows) and rownames of the two variates separated by separator
#' @param data data frame for scatterplot
#' @param separator a string that separates the variable pair string into the 
#'   individual variables
#' @param ... arguments passed on to configure the scatterplot
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section l_ng_plots
#' @template see_l_help
#'   
#' 
#' @template return_l_ng
#'   
#'   
#' @seealso \code{\link{l_ng_plots}}, \code{\link{l_ng_plots.measures}}, 
#'   \code{\link{l_ng_plots.scagnostics}}, \code{\link{measures1d}}, 
#'   \code{\link{measures2d}}, \code{\link{scagnostics2d}}, 
#'   \code{\link{l_ng_ranges}}
#'   
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' n <- 100
#' dat <- data.frame(
#'    A = rnorm(n), B = rnorm(n), C = rnorm(n),
#'    D = rnorm(n), E = rnorm(n)
#' )
#' m2d <- data.frame(
#'     cov = with(dat, c(cov(A,B), cov(A,C), cov(B,D), cov(D,E), cov(A,E))),
#'     measure_1 = c(1, 3, 2, 1, 4),
#'     row.names = c('A:B', 'A:C', 'B:D', 'D:E', 'A:E')
#' )
#' 
#' # or m2d <- as.matrix(m2d)
#' 
#' nav <- l_ng_plots(measures=m2d, data=dat)
#' 
#' # only one measure
#' m <- m2d[,1]
#' names(m) <- row.names(m2d)
#' nav <- l_ng_plots(measures=m, data=dat)
#' 
#' m2d[c(1,2),1]
#' 
#' # one d measures
#' m1d <- data.frame(
#'      mean = sapply(dat, mean),
#'      median =  sapply(dat, median),
#'      sd =  sapply(dat, sd),
#'      q1 = sapply(dat, function(x)quantile(x, probs=0.25)),
#'      q3 = sapply(dat, function(x)quantile(x, probs=0.75)),
#'      row.names = names(dat)
#' )
#' 
#' nav <- l_ng_plots(m1d, dat) 
#' 
#' ## more involved
#' q1 <- function(x)as.vector(quantile(x, probs=0.25))
#' 
#' # be carful that the vector names are correct
#' nav <- l_ng_plots(sapply(oliveAcids, q1), oliveAcids)
#' }
#' 
l_ng_plots.default <- function(measures, data, separator=":", ...) {

    if (grepl(" ", separator, fixed=TRUE))
        stop("separator can not contain spaces")
    
    if (is.vector(measures)) {
        if(is.null(names(measures))) 
            stop("measure vector requires names")
        node_spaces <- names(measures)
        hasOneMeasure <- TRUE
    } else {
        if(is.null(row.names(measures))) 
            stop("measures require row names")
        if(dim(measures)[2] == 1) 
            hasOneMeasure <- TRUE
        else
            hasOneMeasure <- FALSE
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
        stop(paste('l_ng_plots only knows how to deal with 1 or 2 dimensional',
                   'nodes. Found dimension is ', dim))
    
    
    ## ------
    ## GUI
    g <- l_graph()
    nav <- l_navigator_add(g)
    con <- l_context_add_geodesic2d(nav, data=data, separator=separator)
    p <- structure(unlist(strsplit(con['command'], ' ', fixed=TRUE))[1], class=c("l_plot", "loon"))

    
    args <- list(...)
    if (length(args) > 0) {
        l_configure(p, x=data[,1], y=data[,2], ...)        
    } else {
        l_configure(p, x=data[,1], y=data[,2])
    }
    
    transitionDimension <- tclVar('3d')
    parent <- tcl('winfo','parent',g)
    controlFrame <- tcl('frame', l_subwin(parent, 'control'))
    tcl('pack', 'forget', g)
    tcl('pack', controlFrame, side='bottom', fill='x', anchor='w', pady=5)
    tcl('pack', g, fill='both', expand=TRUE)
    
    labelDim <- tcl('label', l_subwin(controlFrame, 'label'), text='Dimension Transitions:')
    
    rb_3d <- tcl('radiobutton', l_subwin(controlFrame, 'rb3d'), text='3d',
                 variable=transitionDimension, value='3d')
    rb_4d <- tcl('radiobutton', l_subwin(controlFrame, 'rb4d'), text='4d',
                 variable=transitionDimension, value='4d')
    rb_34d <- tcl('radiobutton', l_subwin(controlFrame, 'rb34d'), text='3d & 4d',
                  variable=transitionDimension, value='34d')
    tcl('pack', labelDim , rb_3d, rb_4d,rb_34d, side='left')
    ## -----------
    
    ## Histogram or scatterplot matrix?

    if(hasOneMeasure) {
        tmp_sel <- c(1, rep(0, length(measures) - 1))
        histogram <- l_hist(measures, selected = tmp_sel)
        W <- histogram
    } else {
        tmp_sel <- c(1, rep(0, dim(measures)[1]-1)) ## select first point
        if(dim(measures)[2] == 2) {
            spmatrix <- l_plot(x = as.data.frame(measures),
                               itemLabel = rownames(measures),
                               selected = tmp_sel,
                               title = "measures")
            W <- spmatrix
        } else {
            spmatrix <- l_pairs(data = as.data.frame(measures),
                                selected = tmp_sel,
                                linkingGroup=paste0(g,"_measures"))
            lapply(spmatrix, function(plot) {
                l_configure(plot, itemLabel=rownames(measures), showItemLabels=TRUE)
            })
            W <- spmatrix[[1]]
        }
    }
    
    updateGraph <- function() {
        
        sel <- W['selected']
        if (dim == 1) {
            if(sum(sel) >= 2)
                nodes <- apply(utils::combn(node_spaces[sel], 2), 2,
                               function(col)paste(col, collapse=separator))
            else
                nodes <- ""
        } else {
            if (sum(sel) >= 1) 
                nodes <- node_spaces[sel]
            else 
                nodes <- ""
        }
        
        switch(tclvalue(transitionDimension),
               "3d" = {
                   G <- ndtransitiongraph(nodes, 3, separator)
               },
               "4d" = {
                   G <- ndtransitiongraph(nodes, 4, separator)
               },
               "34d" = {
                   G <- ndtransitiongraph(nodes, c(3,4), separator)
               }, 
               return()
        )
        
        l_configure(g, nodes=G$nodes, from=G$from, to=G$to, isDirected=G$isDirected)
        l_scaleto_world(g)
        l_zoom(g, 0.8)
    } 
    
    
    
    id <- as.character(tcl(W, 'bind', 'state', 'add', 'selected', updateGraph))
    
    sapply(c(rb_3d, rb_4d, rb_34d), function(rb) {
        tcl(rb, 'configure', command=updateGraph)        
    })
    
    
    callbackFunctions$state[[paste(W, id, sep="_")]] <- updateGraph
    
    updateGraph()

    l_scaleto_world(p)
    
    if(hasOneMeasure)
        tmp_plots <- W
    else
        tmp_plots <- spmatrix
    
    plot <- list(plots= tmp_plots,
                 graph=g,
                 plot=p,
                 navigator=nav,
                 context=con,
                 env=environment())
    class(plot) <- c("l_ng_plots", "l_navgraph", "l_compound", "loon")
    return(plot)
}



#' @title 2d Navigation Graph Setup with dynamic node fitering using a 
#'   scatterplot matrix
#'   
#' @description Measures object is of class measures. When using measure objects
#'   then the measures can be dynamically re-calculated for a subset of the 
#'   data.
#'   
#' @param measures object of class measures, see \code{\link{measures1d}}, 
#'   \code{\link{measures2d}}.
#' @param ... arguments passed on to configure the scatterplot
#'   
#'   
#' @details Note that we provide the \code{\link{scagnostics2d}} function to 
#'   create a measures object for the scagnostics measures.
#'   
#' @templateVar page  learn_R_display_graph
#' @templateVar section l_ng_plots
#' @template see_l_help
#' 
#'   
#' @template return_l_ng
#' 
#'     
#' @seealso \code{\link{measures1d}}, \code{\link{measures2d}}, 
#'   \code{\link{scagnostics2d}}, \code{\link{l_ng_plots}},
#'   \code{\link{l_ng_ranges}}
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' # 2d measures
#' scags <- scagnostics2d(oliveAcids, separator='**')
#' scags()
#' ng <- l_ng_plots(scags, color=olive$Area)
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
#' nav <- l_ng_plots(m1d, color=iris$Species)
#' 
#' # with only one measure
#' nav <- l_ng_plots(measures1d(oliveAcids, sd))
#' 
#' # with two measures
#' nav <- l_ng_plots(measures1d(oliveAcids, sd=sd, mean=mean))
#' }
l_ng_plots.measures <- function(measures, ...) {

    separator <- measures('separator')
    
    objs <- l_ng_plots.default(measures(), measures('data'), separator, ...)
    
    isHist <- FALSE
    isSplom <- FALSE
    if (is.list(objs$plots)) {
        W <- objs$plots[[1]]
        isSplom <- TRUE
    } else {
        ## For plot or histogram
        W <- objs$plots
        if(as.numeric(tcl('info', 'object', 'isa', 'typeof', W, '::loon::classes::Histogram_Widget')))
            isHist <- TRUE
    }
    pairsFrame <- tkwinfo('parent', W)
    
    
    ## With measures add scalto and recalculate
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
        

        if(isSplom) {
            ## Update measures in scatterplot
            nvar <- dim(envir$measures)[2]
            pair <- utils::combn(nvar, 2)
            for (i in 1:dim(pair)[2]) {
                ix <- pair[2,i]; iy <- pair[1,i]
                l_configure(objs$plots[[i]], x=envir$measures[,ix], y=envir$measures[,iy])
            }
        } else if (isHist) {
            l_configure(objs$plots, x=envir$measures)
        } else {
            l_configure(objs$plots, x=envir$measures[,1], y=envir$measures[,2])           
        }
        cat('   done\n')
    }
    
    callbackFunctions$general[[paste0(objs$graph,'.updateMeasures')]] <- updateMeasures
    
    if (!isHist) {
        resetZoomPan01 <- function() {
            lapply(objs$plots, function(p) {
                l_configure(p, deltaX=1, deltaY=1,
                            zoomX=.8, zoomY=.8,
                            panX=-0.125, panY=-.125)
            })
        }
        callbackFunctions$general[[paste0(objs$graph,'.resetZoomPan01')]] <- updateMeasures
        
        resetZoomPan <- function() {
            lapply(objs$plots, function(p) {
                l_scaleto_world(p)
            })
        }
        callbackFunctions$general[[paste0(objs$graph,'.resetZoomPan')]] <- updateMeasures
    }
    
    
    sep <- tcl('ttk::separator', l_subwin(pairsFrame, 'sep'))

    controlFrame <- tcl('frame', paste0(pairsFrame,'.controls'),
                        padx=5, pady=5)
    
    cback <- unlist(strsplit(.Tcl.callback(updateMeasures),' ', fixed=TRUE))[1:2]
    
    basedon_lab <- tcl('label', l_subwin(controlFrame, 'basedonlabel'), text='Recalulate Measures based on:')
    bframe <- tcl('frame', l_subwin(controlFrame,'basedon'))
    bactive <- tcl('button', l_subwin(bframe, 'active'), text='active',
                   command = paste(c(cback, 'active'), collapse = ' '))
    bselected <- tcl('button', l_subwin(bframe, 'selected'), text='selected',
                     command=paste(c(cback, 'selected'), collapse = ' '))
    tcl('pack', bactive, bselected, side='left')
    
    tcl('grid', basedon_lab, row=0, column=0, sticky='w')
    tcl('grid', bframe, row=1, column=0, sticky='w')
    
    if (!isHist) {
        scaleto_lab <- tcl('label', l_subwin(controlFrame, 'scaltolabel'), text='Scale to:')
        sbframe <- tcl('frame', l_subwin(controlFrame,'scaleto'))
        bworld <- tcl('button', l_subwin(sbframe, 'world'), text='world', command=resetZoomPan)
        b01 <- tcl('button', l_subwin(sbframe, 'zeroone'), text='[0,1]', command=resetZoomPan01)
        tcl('pack', bworld, b01, side='left')
        
        tcl('grid', scaleto_lab, row=0, column=2, sticky='w', padx=c(5,0))
        tcl('grid', sbframe, row=1, column=2, sticky='w', padx=c(5,0))
        
        tcl('grid', 'columnconfigure', controlFrame, '1', weight=1)
        tcl('grid', 'columnconfigure', controlFrame, '0', pad=5)
        tcl('grid', 'columnconfigure', controlFrame, '2', pad=5)
        
        tcl('grid', 'rowconfigure', controlFrame, '0', pad=5)
        tcl('grid', 'rowconfigure', controlFrame, '1', pad=5)
    }
    
    if(isSplom) {
        n_measures <- dim(envir$measures)[2]
        tkgrid(sep, columnspan=n_measures, sticky='ew')
        tkgrid(controlFrame, columnspan=n_measures, sticky='ew')
    } else {
        
        tkpack(sep, controlFrame, side='top', fill='x')
    }

    return(objs)
}


#' @title 2d Navigation Graph Setup with dynamic node fitering based on 
#'   scagnostic measures and by using a scatterplot matrix
#'   
#' @description This method is useful when working with objects from the 
#'   \code{\link[scagnostics]{scagnostics}} function from the scagnostics \R 
#'   package. In order to dynamically re-calcultate the scagnostic measures for 
#'   a subset of the data use the \code{\link{scagnostics2d}} measures creature 
#'   function.
#'   
#' @inheritParams l_ng_plots.default 
#' @param measures objects from the \code{\link[scagnostics]{scagnostics}} 
#'   function from the scagnostics \R package
#'   
#' @template return_l_ng
#'   
#'   
#' @seealso \code{\link{l_ng_plots}}, \code{\link{l_ng_plots.default}}, 
#'   \code{\link{l_ng_plots.measures}}, \code{\link{measures1d}}, 
#'   \code{\link{measures2d}}, \code{\link{scagnostics2d}}, 
#'   \code{\link{l_ng_ranges}}
#'   
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' library(scagnostics)
#'   scags <- scagnostics::scagnostics(oliveAcids)
#'   l_ng_plots(scags, oliveAcids, color=olive$Area)
#' }
l_ng_plots.scagnostics <- function(measures, data, separator=":", ...) {

    force(data)
    
    requireNamespace("scagnostics", quietly = TRUE) || stop("scagnostics package is required for this method") 
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
    
    l_ng_plots.default(measures, data, separator, ...)
}


#' @export
l_getPlots.l_ng_plots <- function(target){
    append(target$plots, (list(graph = target$graph, plot = target$plot)))
}