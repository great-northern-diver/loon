
local({
    
    highlight_knn <- function(p, data, k=5, method='euclidean') {

        if(!is(data, 'data.frame'))
            data <- as.data.frame(data)
        
        ## Create Custom Control Panel
        tt <- tktoplevel()
        
        onOff <- tclVar('1')
        tkgrid(tkcheckbutton(tt, text='on/off', variable=onOff), sticky='w')
        
        k <- tclVar(k)
        f1 <- tkframe(tt)
        
        e <- tkentry(f1, width=3, textvariable=k)
        tkbind(e, '<Return>', function()hNN())
        tkgrid(f1, sticky='w')
        tkpack(tklabel(f1, text='k='), e, side='left')
        
        tkgrid(tklabel(tt, text='Nearest to:'), sticky='w')
        distFrom <- tclVar('points')
        f2 <- tkframe(tt)
        tkgrid(f2, sticky='w')
        tkpack(tkradiobutton(f2, text='points', variable=distFrom,
                             value='points', command=function()hNN()),
               tkradiobutton(f2, text='mean', variable=distFrom,
                             value='mean', command=function()hNN()),
               side='left')
        
        tkgrid(tklabel(tt, text='Space:'), sticky='w')
        chbtns <- lapply(names(data), function(name) {
            bvar <- tclVar('1')
            b <- tkcheckbutton(tt, text=name, variable=bvar, 
                               command=function()hNN())
            tkgrid(b, sticky='w', padx=2)
            return(bvar)
        })
        
        
        ## Create Nearest neighbour highlighting Functionality
        
        if(!is(p, 'loon'))
            class(p) <- "loon"
        
        n <- nrow(data)
        ## Which variables are used for D
        cachedSpaceSelection <- rep(TRUE, ncol(data))
        D <- as.matrix(dist(data, method = method))
        I <- matrix(rep(1:n, n), ncol=n, byrow=TRUE)
        
        inds <- 1:n # used for subsetting
        
        ## Cache point gyph attributes that are used for highlighting
        glyphCache <- character(0)
        whichCache <- integer(0)
        sizeCache <- integer(0)
        
        ## Function that highlights nearest neighbours
        hNN <- function() {
            
            ## reset cached point glyphs attributes
            if (length(whichCache) > 0) {
                l_configure(p, glyph=glyphCache, size=sizeCache, which=whichCache)
                whichCache <<- integer(0)
            }
            
            if (tclvalue(onOff) == '0') return()
            
            isel <- which(p['selected'])
            if (length(isel) == 0 || length(isel) == n) return()
            
            spaceSelection <- vapply(chbtns,
                                     function(b)as.logical(as.numeric(tclvalue(b))),
                                     logical(1))
            
            if(tclvalue(distFrom)=='points') {
                if(!identical(cachedSpaceSelection, spaceSelection)) {
                    D <<- as.matrix(dist(data[, spaceSelection]))
                    cachedSpaceSelection <<- spaceSelection
                } 
                chng_which <- unique(c(I[isel, -isel])[order(c(D[isel, -isel]))])
            } else {
                p_mean <- apply(data[isel, spaceSelection], 2, mean)
                d <- apply(data[-isel, spaceSelection], 1, 
                           function(row) dist(rbind(row, p_mean)))
                
                chng_which <- (inds[-isel])[order(d)]
            }
            
            kval <- tclvalue(k)
            if (grepl('[[:digit:]]+', kval)) {
                kval <- as.numeric(kval)
            } else {
                kval <- 5
            }
            
            ksel <- min(length(chng_which),kval)
            
            whichCache <<- chng_which[1:ksel]
            glyphCache <<- p['glyph'][whichCache]
            sizeCache <<- p['size'][whichCache]
            l_configure(p, glyph='csquare', size=seq(25, 8, length.out = ksel), which=whichCache)
        }

        l_bind_state(p, 'selected', hNN)
    }  
    
    ## For example,
    sOiveAcids <- data.frame(scale(oliveAcids))
    p <- with(sOiveAcids, l_plot(oleic~stearic, color=olive$Area))
    highlight_knn(p, data=sOiveAcids, k=5)
    
    l_aspect(p) <- 1
    
    readline("press the return key to continue: next in a navigation graph setting")

    nav <- l_navgraph(oliveAcids, color=olive$Area)
    highlight_knn(nav$plot, sOiveAcids)
    l_aspect(nav$plot) <- 1
    
    
})
