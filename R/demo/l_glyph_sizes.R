
require(RnavGraphImageData) || stop("Neeed RnavGraphImageData package")

if (loon:::.withTclImg) {
    local({
        # Plot glyphsizes for different glyphs
        sizes <- c(0:10) # seq(15,25,by=5)
        ns <- length(sizes)
        
        glyphs <- c('sizes', 'circle', 'square', 'triangle', 'diamond',
                    'text', 'image', 'stars', 'parallel', 'polygon')
        ng <- length(glyphs)
        
        ## ns+1 for labels
        x <- rep(1:(ns+1), ng)
        y <- rep(c(9, 8, 7.5, 7, 6.5, 6, 5, 3, 1,-1), each=ns+1)
        
        p <- l_plot(x,y, showLabels=FALSE)
        
        
        for (i in seq_along(glyphs)) {
            assign(paste0('i_',glyphs[i]), seq((i-1)*(ns+1)+1,i*(ns+1)))
        }
        
        
        ## Size Labels
        labelsize <- 6
        psizes <- rep(c(labelsize, sizes), ng)
        
        c_psizes <- as.character(psizes)
        c_psizes[c_psizes=="0"] <- "<1" 
        
        g_sizes <- l_glyph_add_text(p, text = c_psizes, label="size labels")
        
        p['glyph'] <- g_sizes
        p['size'] <- psizes
        
        l_configure(p, color='black', size=labelsize, which=i_sizes)
        
        ## Primitive Glyphs
        l_configure(p, glyph='circle', which=i_circle)
        l_configure(p, glyph='square', which=i_square)
        l_configure(p, glyph='triangle', which=i_triangle)
        l_configure(p, glyph='diamond', which=i_diamond)
        
        
        ## text glyph
        g_text <- l_glyph_add_text(p, text = rep("aA", p['n']), label='text glyphs')
        l_configure(p, glyph=g_text, which=i_text)
        
        
        ## Images
        data(faces)
        faces.imgs <- l_image_import_array(faces, 64, 64, img_in_row = FALSE)
        faces.imgs[1]
        g_image <- l_glyph_add_image(p, image=rep(faces.imgs[1], p['n']), label='frey faces')
        l_configure(p, glyph=g_image, which=i_image)
        
        
        ## Stars
        g_stars <- l_glyph_add_serialaxes(p, data=oliveAcids,
                                          label='star glyphs', showArea=FALSE,
                                          showAxes = TRUE, showEnclosing = TRUE)
        l_configure(p, glyph=g_stars, which=i_stars)
        
        ## Parallel
        g_parallel <- l_glyph_add_serialaxes(p, data=oliveAcids,
                                             label='parallel coords', linewidth = 3, axesLayout = 'parallel',
                                             showAxes = TRUE, showEnclosing = TRUE, showArea=FALSE)
        l_configure(p, glyph=g_parallel, which=i_parallel)
        
        
        # Polygons 
        # hand drawn
        airplane_coords <- c(30.8,0.5,57.4,27.1,85.6,16.5,89.9,17,78.7,30.9,183.5,27.7,
                             223.5,6.4,234.6,7.4,222.9,22.3,240,21.8,253.8,26.1,264.5,
                             33.5,276.2,39.4,283.1,42,286.5,50.6,282,57.5,273.5,63.9,
                             260.2,69.7,246.9,72.4,217.1,76.1,176.6,78.8,151.6,78.8,
                             88.8,105.9,62.7,95.8,117,70.8,87.7,70.8,73.9,68.1,56.3,
                             63.3,44.6,53.2,20.7,61.2,11.6,57.5,34,44.2)
        x_ap <- airplane_coords[seq(1, length(airplane_coords), by=2)]
        y_ap <- airplane_coords[seq(2, length(airplane_coords), by=2)]
        ## center-scale
        d_ap <- diff(range(x_ap, y_ap))/5 # 5 is min width or height of airplane if size <= 1
        x_aps <- (x_ap-mean(x_ap))/d_ap 
        y_aps <- (y_ap-mean(y_ap))/d_ap
        
        g_polygon <- l_glyph_add_polygon(p, x=lapply(seq_len(p['n']), function(arg) x_aps),
                                         y = lapply(seq_len(p['n']), function(arg) y_aps),
                                         label='airplane')
        l_configure(p, glyph=g_polygon, which=i_polygon)
        
        
        ## Row Labels
        vapply(glyphs, function(g) {
            get(paste0('i_', g))[1]    
        }, numeric(1))## Row labeling
        
        g_row <- l_glyph_add_text(p, text=rep(glyphs, each= ns+1), label='glyph labels')
        
        i_rowlabels <- vapply(glyphs, function(g) {
            get(paste0('i_', g))[1]    
        }, numeric(1))
        
        l_configure(p, glyph=g_row, color='black', which=i_rowlabels)
        
    })  
} else {
    cat("need the tkimg tcl extension installed to run this demo.\n")
}


