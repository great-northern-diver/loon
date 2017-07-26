

require(MASS) || stop("MASS library required")
require(kernlab)  || stop("kernlab library required")
require(RDRToolbox)  || stop("RDRToolbox library required")
require(RColorBrewer) || stop("RColorBrewer library required")

local({
    
    ## PCA
    obj.pca  <- prcomp(oliveAcids, scale=TRUE)
    olive_pca <- obj.pca$x
    
    ## FDA
    obj.lda <- with(olive, MASS::lda(Area~palmitic+palmitoleic+stearic+oleic+ 
                                         linoleic+linolenic+arachidic+eicosenoic))
    olive_lda <- MASS:::predict.lda(obj.lda, newdata = olive)$x
    
    dim(olive_lda)
    
    ## Multidimensional scaling
    D <- dist(scale(oliveAcids))
    
    ## Metric
    olive_linear_mds <- cmdscale(d = D, k=5)
    
    ## Non-metric (Kruskal with Stress)
    olive_nonlinear_mds <- MASS::isoMDS(d = D, k=5)$points
    
    ## Kernel PCA
    obj.kpca <- kernlab::kpca(scale(oliveAcids),
                              kernel='polydot',
                              kpar=list(degree=3, offset=1),
                              features=5)
    
    olive_kpca <- rotated(obj.kpca)
    
    
    ##olive_lda Kernel Discriminant Analysis
    ##library(ks)
    
    ## Isomap
    olive_isomap <- RDRToolbox::Isomap(data=scale(oliveAcids), dim=5, k=6)$dim5
    
    
    ## LLE
    olive_LLE <- RDRToolbox::LLE(data=as.matrix(oliveAcids), dim=5, k=6)
    
    
    ## Now look at this using navigation graphs
    
    G <- completegraph(nodes=paste0('d', seq(1,5)))
    LG <- linegraph(G)
    LGnot <- loon::complement(LG)
    
    g <- l_graph(LG)
    gs <- l_graphswitch(parent=tkwinfo('toplevel', g), activewidget = g)
    tkpack.forget(g)
    tkpack(gs, side='right', fill='y')
    tkpack(g, side='right', fill='y', expand=TRUE)
    
    l_graphswitch_add(gs, LG, label='3d transition')
    l_graphswitch_add(gs, LGnot, label='4d transition')
    
    
    ## Navigators & contexts
    allData <- list(olive_pca,
                    olive_lda,
                    olive_linear_mds,
                    olive_nonlinear_mds,
                    olive_kpca,
                    olive_isomap,
                    olive_LLE)
    
    ndata <- function(dat, k=5) {
        d <- dat[, 1:5]
        colnames(d) <- paste0('d', seq(1,k)) 
        as.data.frame(d)
    }
    
    tt <- tktoplevel()
    tktitle(tt) <- "plots"
    
    objs <- Map( function(data, col, label) {
        p <- l_plot(parent=tt, linkingGroup='olive', title=label)
        nav <- l_navigator_add(g, color=col)
        nav['label'] <-  label
        con <- l_context_add_geodesic2d(nav, data=ndata(data),
                                        command = paste0(p, " configure -x %x -y %y -xlabel %xlabel -ylabel %ylabel"))
        l_configure(p, color=olive$Area)
        list(nav, con, p)    
    }, allData, brewer.pal(7, "Set2"), c('pca', 'lda', 'lmds', 'nlmds', 'kpca', 'isomap', 'lle'))
    
    plots <- lapply(objs, function(x) x[[3]]) 
    
    do.call(tkgrid, c(plots[1:4], sticky='nesw'))
    do.call(tkgrid, c(plots[5:7], sticky='nesw'))
    sapply(0:3, function(i)tkgrid.columnconfigure(tt, i, weight=1))
    sapply(0:1, function(i)tkgrid.rowconfigure(tt, i, weight=1))
    
    sapply(plots, function(p)l_scaleto_world(p))
    
    l_zoom(g, 0.9^2) ## zoom out to show the labels

    
    ## compare this with the original data    
    nav <- l_navgraph(oliveAcids, color=olive$Area,
                      linkingGroup='olive', sync='pull',
                      title='original data')
    
})
