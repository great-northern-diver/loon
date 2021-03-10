library(loon)
context("test_facets")

test_that("l_plot facets work with serialaxes glyph", {
    p <- with(quakes, l_plot(long, lat, linkingGroup = "quakes"))
    p["color"][quakes$mag < 5 & quakes$mag >= 4] <- "lightgreen"
    p["color"][quakes$mag < 6 & quakes$mag >= 5] <- "lightblue"
    p["color"][quakes$mag >= 6] <- "firebrick"
    gs <- l_glyph_add_serialaxes(p, data = quakes, showArea=FALSE)
    p['glyph'][1:100] <- gs
    f <- l_facet(p, by = "color", layout = "grid", linkingGroup = "quakes")
    expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    expect_equal(length(f), 3L)
})

test_that("l_plot facets work with  pointrange glyph", {
    p <- l_plot(x = rep(1:3, 2), color = rep(c('red', 'blue', 'green'), 2), showScales=TRUE)
    g <- l_glyph_add_pointrange(p, ymin=(1:6)-(1:6)/5, ymax=(1:6)+(1:6)/5)
    p['glyph'][1:2] <- g
    f <- l_facet(p, layout = "grid", by = "color")
    expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    expect_equal(length(f), 3L)
})
#
# test_that("l_plot facets work with  image glyph", {
#     p <- with(olive, l_plot(palmitic ~ stearic, color = Region))
#     img_paths <- list.files(file.path(find.package(package = 'loon'), "images"), full.names = TRUE)
#     imgs <- setNames(l_image_import_files(img_paths),
#                      tools::file_path_sans_ext(basename(img_paths)))
#     i <- pmatch(gsub("^[[:alpha:]]+-","", olive$Area), names(imgs), duplicates.ok = TRUE)
#
#     g <- l_glyph_add_image(p, imgs[i], label="Flags")
#     p['glyph'] <- g
#     f <- l_facet(p, layout = "grid", by = "color")
#     expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
#     expect_equal(length(f), 3L)
# })

test_that("l_plot facets work with  polygon glyph", {
    ########## l_glyph_add_polygon ##########
    x_star <-
        c(-0.000864304235090734, 0.292999135695765, 0.949870354364736,
          0.474503025064823, 0.586862575626621, -0.000864304235090734,
          -0.586430423509075, -0.474070872947277, -0.949438202247191,
          -0.29256698357822)
    y_star <-
        c(-1, -0.403630077787381, -0.308556611927398, 0.153846153846154,
          0.808556611927398, 0.499567847882455, 0.808556611927398,
          0.153846153846154, -0.308556611927398, -0.403630077787381)
    x_cross <-
        c(-0.258931143762604, -0.258931143762604, -0.950374531835206,
          -0.950374531835206, -0.258931143762604, -0.258931143762604,
          0.259651397291847, 0.259651397291847, 0.948934024776722,
          0.948934024776722, 0.259651397291847, 0.259651397291847)
    y_cross <-
        c(-0.950374531835206, -0.258931143762604, -0.258931143762604,
          0.259651397291847, 0.259651397291847, 0.948934024776722,
          0.948934024776722, 0.259651397291847, 0.259651397291847,
          -0.258931143762604, -0.258931143762604, -0.950374531835206)
    x_hexagon <-
        c(0.773552290406223, 0, -0.773552290406223, -0.773552290406223,
          0, 0.773552290406223)
    y_hexagon <-
        c(0.446917314894843, 0.894194756554307, 0.446917314894843,
          -0.447637568424085, -0.892754249495822, -0.447637568424085)

    p <- l_plot(1:6, 1:6, color = rep(c('red', 'blue', 'green'), 2))

    gl <- l_glyph_add_polygon(p, x = list(x_star, x_cross, x_hexagon, x_star, x_cross, x_hexagon),
                              y = list(y_star, y_cross, y_hexagon, y_star, y_cross, y_hexagon))

    p['glyph'] <- gl
    f <- l_facet(p, layout = "grid", by = "color")
    expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))

    # test hidden glyphs
    p['glyph'] <- "triangle"
    gl <- l_glyph_add_pointrange(p, ymin = 1:6 - 1/2,
                                 ymax = 1:6 + 1/2)
    f <- l_facet(p, by = "color")
    expect_equal(l_glyph_ids(f[[1]]), c("glyph0", "glyph1"))
    expect_equal(l_glyph_ids(f[[2]]), c("glyph0", "glyph1"))
    expect_equal(l_glyph_ids(f[[3]]), c("glyph0", "glyph1"))
})

test_that("l_plot facets work with  text glyph", {
    n <- 149
    ########## l_glyph_add_text ##########
    iris_ <- iris[seq(n), ]
    p <- l_plot(iris_, color = iris_$Species)
    g <- l_glyph_add_text(p, iris_$Species, "test_label")
    p['glyph'][1:100] <- g
    f <- l_facet(p, layout = "grid", by = "color")
    expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
})

test_that("test facet l_facet class:l_plot", {
    n <- 149
    iris_ <- iris[seq(n), ]
    ########## l_plot ##########
    p <- l_plot(iris_, color = iris_$Species)
    p['size'][1:20] <- 8
    p['size'][21:40] <- 12
    p['size'][41:60] <- 16
    f <- l_facet(p, layout = "wrap", by = c("color", "size"))
    expect_equal(class(f), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
    expect_equal(length(f), 12L)
    f <- l_facet(p, layout = "grid", by = c("color", "size"))
    expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    f <- l_facet(p, layout = "separate", by = c("color", "size"))
    expect_equal(class(f), c("l_facet",    "l_compound", "loon" ))
})

test_that("test facet l_facet class:l_plot3D", {
    n <- 149
    iris_ <- iris[seq(n), ]
    ########## l_plot3D ##########
    p <-  l_plot3D(iris_, color = iris_$Species)
    p['size'][1:20] <- 8
    p['size'][21:40] <- 12
    p['size'][41:60] <- 16
    f <- l_facet(p, layout = "wrap", by = c("color", "size"))
    expect_equal(class(f), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
    # f <- l_facet(p, layout = "grid", by = c("color", "size"))
    # expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    # f <- l_facet(p, layout = "separate", by = c("color", "selected"))
    # expect_equal(class(f), c("l_facet",    "l_compound", "loon" ))
})

test_that("test facet l_facet class:l_hist", {
    n <- 149
    iris_ <- iris[seq(n), ]
    ########## l_hist ##########
    h <- l_hist(iris_, color = iris_$Species)
    h['selected'][1:30] <- TRUE
    h['selected'][31:60] <- TRUE
    # f <- l_facet(h, layout = "wrap", by = c("color", "selected"))
    # expect_equal(class(f), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
    f <- l_facet(h, layout = "grid", by = c("color", "selected"))
    expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    # f <- l_facet(h, layout = "separate", by = c("color", "selected"))
    # expect_equal(class(f), c("l_facet",    "l_compound", "loon" ))
})

test_that("test facet l_facet class:l_serialaxes", {
    n <- 149
    iris_ <- iris[seq(n), ]
    ########## l_serialaxes ##########
    s <- l_serialaxes(iris_, color = iris_$Species)
    s['selected'][1:30] <- TRUE
    s['selected'][31:60] <- TRUE
    # f <- l_facet(s, layout = "wrap", by = c("color", "selected"))
    # expect_equal(class(f), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
    # f <- l_facet(s, layout = "grid", by = c("color", "selected"))
    # expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    f <- l_facet(s, layout = "separate", by = c("color", "selected"))
    expect_equal(class(f), c("l_facet",    "l_compound", "loon" ))
})

test_that("test some facet args in l_plot", {
    # p <- with(mtcars, l_plot(wt, mpg, by = data.frame(am = am, gear = gear, cyl  = cyl, vs = vs),
    #                          labelLocation = c("bottom", "left"),
    #                          labelBackground = "lightblue", labelForeground = "red",
    #                          labelBorderwidth = 5, labelRelief = "flat"))
    # expect_equal(class(p), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    p <- with(mtcars, l_plot(wt, mpg, by = data.frame(am = am, gear = gear, cyl  = cyl),
                             color = "blue",
                             layout = "wrap",
                             labelLocation = c("bottom"),
                             labelBackground = "lightblue", labelForeground = "red",
                             labelBorderwidth = 5, labelRelief = "flat"))
    expect_equal(class(p), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
    # p <- with(mtcars, l_plot(wt, mpg, by = data.frame(am = am, gear = gear, cyl  = cyl),
    #                          color = "blue",
    #                          layout = "separate",
    #                          labelLocation = c("bottom"),
    #                          labelBackground = "lightblue", labelForeground = "red",
    #                          labelBorderwidth = 5, labelRelief = "flat"))
    # expect_equal(class(p), c("l_facet",    "l_compound", "loon" ))
})


test_that("test some facet args in l_plot3D", {
    p <- with(mtcars, l_plot3D(wt, mpg, hp, by = data.frame(am = am, gear = gear, cyl  = cyl),
                               labelLocation = c("bottom", "left"),
                               labelBackground = "lightblue", labelForeground = "red",
                               labelBorderwidth = 5, labelRelief = "flat"))
    expect_equal(class(p), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    # p <- with(mtcars, l_plot3D(wt, mpg, hp, by = data.frame(am = am, gear = gear, cyl  = cyl),
    #                            color = "blue",
    #                            layout = "wrap", labelLocation = c("bottom"),
    #                            labelBackground = "lightblue", labelForeground = "red",
    #                            labelBorderwidth = 5, labelRelief = "flat"))
    # expect_equal(class(p), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
})


test_that("test some facet args in l_hist", {
    p <- with(mtcars, l_hist(mpg, by = data.frame(am = am, gear = gear, cyl  = cyl),
                             labelLocation = c("bottom", "left"),
                             labelBackground = "lightblue", labelForeground = "red",
                             labelBorderwidth = 5, labelRelief = "flat"))
    expect_equal(class(p), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    p <- with(mtcars, l_hist(mpg, by = data.frame(am = am, gear = gear, cyl  = cyl), layout = "wrap",
                             labelLocation = c("bottom"),
                             labelBackground = "lightblue", labelForeground = "red",
                             labelBorderwidth = 5, labelRelief = "flat"))
    expect_equal(class(p), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
})

test_that("test some facet args in l_serialaxes", {
    n <- 149
    iris_ <- iris[seq(n), ]
    s <- l_serialaxes(iris_, sequence = sample(colnames(iris_), 10, replace = TRUE),
                      by = iris_$Species, scaling = "observation")
    expect_equal(class(s), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    s <- l_serialaxes(iris_, sequence = sample(colnames(iris_), 10, replace = TRUE),
                      by = iris_$Species, scaling = "observation", axesLayout = "parallel",
                      layout = "wrap")
    expect_equal(class(s), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
})

test_that("test all possible 'by's", {
    n <- 149
    iris_ <- iris[seq(n), ]
    p <- l_plot(iris_)

    # by is a data.frame
    fp <- l_facet(p, layout = "grid", by = data.frame(iris_$Species, iris_$Species))
    expect_equal(class(fp), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))


    # by is a list
    fp <- l_facet(p, layout = "grid", by = list(iris_$Species, iris_$Species))
    expect_equal(class(fp), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    p['color'][sample(1:n, 70)] <- "red"
    fp <- l_facet(p, by = list("color", iris_ = iris_$Species))
    expect_equal(class(fp), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    p['size'][sample(1:n, 70)] <- 8
    fp <- l_facet(p, by = c("color", "size"))
    expect_equal(class(fp), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    expect_warning(l_facet(p, by = list("color", 1:10)))
    expect_warning(l_facet(p, by = list("foo", "color")))

    # by is a vector
    h <- l_hist(iris_)
    fp <- l_facet(h, layout = "wrap", by = iris_$Species)
    expect_equal(class(fp), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
})

test_that("test separate layouts", {
    n <- 149
    iris_ <- iris[seq(n), ]
    p <- l_plot(iris_, by = iris_$Species,
                layout = "separate")
    expect_equal(class(p), c("l_facet",    "l_compound", "loon" ))
    p <- l_plot3D(iris_, by = iris_$Species,
                  layout = "separate")
    expect_equal(class(p), c("l_facet",    "l_compound", "loon" ))
    p <- l_hist(iris_, by = iris_$Species,
                layout = "separate")
    expect_equal(class(p), c("l_facet",    "l_compound", "loon" ))
    s <- l_serialaxes(iris_, by = iris_$Species,
                      layout = "separate")
    expect_equal(class(s), c("l_facet",    "l_compound", "loon" ))
})

test_that("test layers inherits", {
    p <- l_plot(rnorm(10), rnorm(10))
    p['color'][1:5] <- "red"
    group <- l_layer_group(p)
    line <- l_layer_line(p, 1:5, c(1:3, 3,4), parent = group)
    rect <- l_layer_rectangle(p, x = c(2,3), y = c(3,4))
    l_layer_hide(p, rect)
    fp <- l_facet(p, by = "color")
    layers <- l_layer_getChildren(fp[[1]])
    expect_equal(length(layers), 3)
    layer1 <- l_create_handle(c(p, layers[1]))
    expect_equal(class(layer1)[1], "l_layer_rectangle")
    expect_false(l_layer_isVisible(layer1))
    layer2 <- l_create_handle(c(p, layers[2]))
    expect_equal(class(layer2)[1], "l_layer_group")
    expect_equal(length(l_layer_getChildren(layer2)), 1)
})

test_that("test formula by", {
    n <- 149
    iris_ <- iris[seq(n), ]
    ps <- l_serialaxes(iris_,
                       by = linewidth ~ color,
                       linewidth = sample(c(1,3), size = n, replace = TRUE),
                       color = sample(c("red", "green"), size = n, replace = TRUE))
    expect_equal(length(ps), 4)

    pp <- l_plot(x = 1:6, y = 1:6,
                 by = size ~ color,
                 size = c(rep(50, 2), rep(25, 2), rep(50, 2)),
                 color = c(rep("red", 3), rep("green", 3)))
    expect_equal(length(pp), 4)


    on <- data.frame(size = c(rep(50, 2), rep(25, 2), rep(50, 2)),
                     color = c(rep("red", 3), rep("green", 3)),
                     glyph = c("ocircle", "ccircle", "ocircle", "ccircle", "ocircle", "ccircle"))

    p <- l_plot(x = 1:6, y = 1:6,
                glyph = c("ocircle", "ccircle", "ocircle", "ccircle", "ocircle", "ccircle"),
                size = c(rep(50, 2), rep(25, 2), rep(50, 2)),
                color = c(rep("red", 3), rep("green", 3)),
                by = size ~ color,
                on = on)

    # avoid hex code in tests
    # it is because, in solaris X64 system, the 12 digit hex code is slightly different from that in
    # windows and mac
    # expect_equal(p[[1]]['color'], l_hexcolor("green")) # green
    expect_equal(p[[1]]['size'], 25)

    # expect_equal(p[[2]]['color'], c(l_hexcolor("green"), l_hexcolor("green"))) # green
    expect_equal(p[[2]]['size'], c(50, 50))


    size <- c(rep(50, 2), rep(25, 2), rep(50, 2))
    color <- c(rep("red", 3), rep("green", 3))
    glyph <- c("ocircle", "ccircle", "ocircle", "ccircle", "ocircle", "ccircle")

    p <- l_plot(x = 1:6, y = 1:6,
                glyph = c("ocircle", "ccircle", "ocircle", "ccircle", "ocircle", "ccircle"),
                size = c(rep(50, 2), rep(25, 2), rep(50, 2)),
                color = c(rep("red", 3), rep("green", 3)),
                by = size ~ color + glyph)

    # expect_equal(p[[1]]['color'], l_hexcolor("green")) # green
    expect_equal(p[[1]]['size'], 25)
    expect_equal(p[[1]]['glyph'], "ccircle")


    # expect_equal(p[[8]]['color'], l_hexcolor("red")) # red
    expect_equal(p[[8]]['size'], 50)
    expect_equal(p[[8]]['glyph'], "ocircle")


    p <- l_plot(x = 1:6, y = 1:6,
                glyph = c("ocircle", "ccircle", "ocircle", "ccircle", "ocircle", "ccircle"),
                size = c(rep(50, 2), rep(25, 2), rep(50, 2)),
                color = c(rep("red", 3), rep("green", 3)))

    g <- l_glyph_add_text(p, text = 1:6)
    p['glyph'] <- g
    f <- l_facet(p, by = color ~ size, layout = "wrap")

    expect_equal(f[[1]]['size'], 25)
    # expect_equal(f[[1]]['color'], l_hexcolor("green")) # green

    expect_equal(f[[2]]['size'], 25)
    # expect_equal(f[[2]]['color'], l_hexcolor("red")) # red

    f <- l_facet(p, by = color ~ size)

    expect_equal(f[[1]]['size'], 25)
    # expect_equal(f[[1]]['color'], l_hexcolor("green")) # green

    expect_equal(f[[2]]['size'], 25)
    # expect_equal(f[[2]]['color'], l_hexcolor("red")) # red


    on <- data.frame(Factor1 = c(rep("A", 3), rep("B", 3)),
                     Factor2 = rep(c("C", "D"), 3))

    f <- l_facet(p, by = Factor1 ~ Factor2, on = on)
    expect_true(all(c("l_facet",    "l_compound", "loon" ) %in% class(f)))


    # by with NA
    by <- iris_$Species
    by[1:10] <- NA
    expect_warning(
        p <- l_plot(iris_, by = by, linkingGroup = "foo")
    )
    expect_equal(length(p$x1y1['x']), 40)
    expect_warning(
        s <- l_serialaxes(iris_, by = by)
    )
    expect_equal(nrow(s$x1y1['data']), 40)
})
