#
# if (requireNamespace("testthat", quietly = TRUE)) {
#
#     library(testthat)
library(loon)
library(grid)
context("test_loonGrob")

test_that("plot layers have correct grid structure", {
    p <- with(iris,
              l_plot(x=Sepal.Length, y=Petal.Length,
                     color=Species, title="Iris Data"))
    ## Layer a Group
    l.g <- l_layer_group(p, label="Drawings", parent="root", index="end")

    l.pts <- l_layer_points(p,
                            x=c(4, 6, 6),
                            y=c(1, 2, 6),
                            color=c("green", "orange", "black"),
                            parent=l.g)

    l_scaleto_layer(p, l.pts)

    l_scaleto_world(p)

    l_configure(l.pts, color="blue")

    l_configure(l.pts,
                x=seq(from=4,to=8, length.out=20),
                y=seq(from=1,to=7, length.out=20),
                color="steelblue", size=20:39)

    l_layer_relabel(p, l.pts, "Different Sizes")
    l_layer_move(p, l.pts, parent="root")

    ## Polygon
    i <- with(iris, chull(Sepal.Length, Petal.Length))
    x.hull <- iris$Sepal.Length[i]
    y.hull <- iris$Petal.Length[i]
    l_layer_polygon(p, x.hull, y.hull, color="thistle",
                    linecolor="black", linewidth=4, parent=l.g)
    ## Rectangle
    l_layer_rectangle(p, x=c(5, 6), y=c(4, 6), linewidth=2)

    ## Oval
    l_layer_oval(p, x=c(5, 6), y=c(4, 7),
                 color="", linecolor="orange", linewidth=4)

    ## Line
    x <- with(iris, Sepal.Length[Species=="setosa"])
    y <- with(iris, Petal.Length[Species=="setosa"])

    fit <- lm(y~x)
    ##summary(fit)

    xr <- seq(from=min(x), to=max(x), length.out=20)
    yp <- predict(fit, data.frame(x=xr), interval="prediction")


    l.pi <- l_layer_polygon(p, x=c(xr, rev(xr)),
                            y=c(yp[,2],rev(yp[,3])),
                            color="lightgreen",
                            linecolor= "darkgreen", linewidth=2,
                            label="prediction interval west liguria")

    l.fit <- l_layer_line(p, x=xr, y=yp[,1],
                          color="darkgreen", linewidth=8,
                          label="fit west liguria")

    l_layer_move(p, l.pi, "root", "end")
    l_layer_raise(p, l.pi)


    ## Text (size does not work and color is gray)
    bbox <- l_layer_bbox(p, "root")

    l_layer_texts(p, x=seq(from=bbox[1], to=bbox[3], length.out=length(LETTERS)),
                  y=rev(seq(from=bbox[2], to=bbox[4], length.out=length(LETTERS))),
                  text=LETTERS, size=seq_along(LETTERS),
                  angle=seq_along(LETTERS)/length(LETTERS)*360)
    lgrob <- loonGrob(p)
    expect_equal(class(lgrob), c("gTree", "grob", "gDesc"))
    expect_equal(class(lgrob$children), c("gList"))
})

test_that("plots with different glyphs have correct structure", {

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

    # CRAN Seems incapable of finding data in loon.data when testing.
    #
    # ## Images
    # data(faces, package = "loon.data")
    # faces.imgs <- l_image_import_array(faces, 64, 64, img_in_row = FALSE)
    #
    # g_image <- l_glyph_add_image(p, image=rep(faces.imgs[1], p['n']), label='frey faces')
    # l_configure(p, glyph=g_image, which=i_image)


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

    ## centre-scale
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
    lgrob <- loonGrob(p)
    # grid.newpage()
    # grid.draw(lgrob)
    expect_equal(class(lgrob), c("gTree", "grob", "gDesc"))
    expect_equal(class(lgrob$children), c("gList"))

})

test_that("graphs have correct structure", {
    iris_ <- iris[seq(149), ]
    G <- completegraph(names(iris_[,-5]))
    LG <- linegraph(G)
    g <- l_graph(LG)

    nav0 <- l_navigator_add(g)
    l_configure(nav0, label = 0)
    con0 <- l_context_add_geodesic2d(navigator=nav0, data=iris_[,-5])

    nav1 <- l_navigator_add(g, from = "Sepal.Length:Petal.Width",
                            to = "Petal.Length:Petal.Width", proportion = 0.6)
    l_configure(nav1, label = 1)
    con1 <- l_context_add_geodesic2d(navigator=nav1, data=iris_[,-5])

    nav2 <- l_navigator_add(g, from = "Sepal.Length:Petal.Length",
                            to = "Sepal.Width:Petal.Length", proportion = 0.5)
    l_configure(nav2, label = 2)
    con2 <- l_context_add_geodesic2d(navigator=nav2, data=iris_[,-5])
    lgrob <- loonGrob(g)
    #grid.newpage(); grid.draw(lgrob)
    expect_equal(class(lgrob), c("gTree", "grob", "gDesc"))

})

test_that("histograms have correct structure", {
    iris_ <- iris[seq(149), ]
    h <- l_hist(iris_$Sepal.Length, color=iris_$Species)
    g <- loonGrob(h)
    #grid.newpage(); grid.draw(g)
    expect_equal(class(g), c("gTree", "grob", "gDesc"))

    h['showStackedColors'] <- TRUE
    g <- loonGrob(h)
    #grid.newpage(); grid.draw(g)
    expect_equal(class(g), c("gTree", "grob", "gDesc"))

    h['colorStackingOrder'] <- c("selected", unique(h['color']))
    g <- loonGrob(h)
    #grid.newpage(); grid.draw(g)
    expect_equal(class(g), c("gTree", "grob", "gDesc"))

    h['colorStackingOrder'] <- rev(h['colorStackingOrder'])
    g <- loonGrob(h)
    #grid.newpage(); grid.draw(g)
    expect_equal(class(g), c("gTree", "grob", "gDesc"))

})

test_that("navgation graphs have correct structure", {
    ng <- l_navgraph(iris, separator='-', color=iris$Species)

    lgrob <- grid.loon(ng, draw = FALSE)
    expect_equal(class(lgrob), c("gTree", "grob", "gDesc"))

})

test_that("l_pairs have correct structure", {
    iris_ <- iris[seq(149), ]
    p <- l_pairs(iris_[,-5], color=iris_$Species)
    lgrob <- loonGrob(p)
    #grid.newpage()
    #grid.draw(lgrob)
    expect_equal(class(lgrob), c("gTree", "grob", "gDesc"))
})

test_that("l_serialaxes have correct structure", {
    s <- l_serialaxes(data=iris, color=iris$Species, title="iris data")
    lgrob <- grid.loon(s, draw = FALSE)
    expect_equal(class(lgrob), c("gTree", "grob", "gDesc"))

    s['axesLayout'] <- 'parallel'
    lgrob <- grid.loon(s, draw = FALSE)
    expect_equal(class(lgrob), c("gTree", "grob", "gDesc"))
})

test_that("l_ts has correct structure", {
    decompose <- decompose(co2)
    # or decompose <- stl(co2, "per")
    p <- l_plot(decompose, title = "Atmospheric carbon dioxide over Mauna Loa")
    lgrob <- loonGrob(p)
    expect_equal(class(lgrob), c("gTree", "grob", "gDesc"))
})


#}
