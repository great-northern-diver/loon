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
})

test_that("l_plot facets work with  text glyph", {
    ########## l_glyph_add_text ##########
    p <- l_plot(iris, color = iris$Species)
    g <- l_glyph_add_text(p, iris$Species, "test_label")
    p['glyph'][1:100] <- g
    f <- l_facet(p, layout = "grid", by = "color")
    expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
})

test_that("test facet l_facet class:l_plot", {
    ########## l_plot ##########
    p <- l_plot(iris, color = iris$Species)
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
    ########## l_plot3D ##########
    p <-  l_plot3D(iris, color = iris$Species)
    p['size'][1:20] <- 8
    p['size'][21:40] <- 12
    p['size'][41:60] <- 16
    f <- l_facet(p, layout = "wrap", by = c("color", "size"))
    expect_equal(class(f), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
    f <- l_facet(p, layout = "grid", by = c("color", "size"))
    expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    f <- l_facet(p, layout = "separate", by = c("color", "selected"))
    expect_equal(class(f), c("l_facet",    "l_compound", "loon" ))
})

test_that("test facet l_facet class:l_hist", {
    ########## l_hist ##########
    h <- l_hist(iris, color = iris$Species)
    h['selected'][1:30] <- TRUE
    h['selected'][31:60] <- TRUE
    f <- l_facet(h, layout = "wrap", by = c("color", "selected"))
    expect_equal(class(f), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
    f <- l_facet(h, layout = "grid", by = c("color", "selected"))
    expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    f <- l_facet(h, layout = "separate", by = c("color", "selected"))
    expect_equal(class(f), c("l_facet",    "l_compound", "loon" ))
})

test_that("test facet l_facet class:l_serialaxes", {
    ########## l_serialaxes ##########
    s <- l_serialaxes(iris, color = iris$Species)
    s['selected'][1:30] <- TRUE
    s['selected'][31:60] <- TRUE
    f <- l_facet(s, layout = "wrap", by = c("color", "selected"))
    expect_equal(class(f), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
    f <- l_facet(s, layout = "grid", by = c("color", "selected"))
    expect_equal(class(f), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    f <- l_facet(s, layout = "separate", by = c("color", "selected"))
    expect_equal(class(f), c("l_facet",    "l_compound", "loon" ))
})

test_that("test some facet args in l_plot", {
    p <- with(mtcars, l_plot(wt, mpg, by = data.frame(am = am, gear = gear, cyl  = cyl, vs = vs),
                             labelLocation = c("bottom", "left"),
                             labelBackground = "lightblue", labelForeground = "red",
                             labelBorderwidth = 5, labelRelief = "flat"))
    expect_equal(class(p), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    p <- with(mtcars, l_plot(wt, mpg, by = data.frame(am = am, gear = gear, cyl  = cyl),
                             color = "blue",
                             layout = "wrap",
                             labelLocation = c("bottom"),
                             labelBackground = "lightblue", labelForeground = "red",
                             labelBorderwidth = 5, labelRelief = "flat"))
    expect_equal(class(p), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
    p <- with(mtcars, l_plot(wt, mpg, by = data.frame(am = am, gear = gear, cyl  = cyl),
                             color = "blue",
                             layout = "separate",
                             labelLocation = c("bottom"),
                             labelBackground = "lightblue", labelForeground = "red",
                             labelBorderwidth = 5, labelRelief = "flat"))
    expect_equal(class(p), c("l_facet",    "l_compound", "loon" ))
})


test_that("test some facet args in l_plot3D", {
    p <- with(mtcars, l_plot3D(wt, mpg, hp, by = data.frame(am = am, gear = gear, cyl  = cyl),
                               labelLocation = c("bottom", "left"),
                               labelBackground = "lightblue", labelForeground = "red",
                               labelBorderwidth = 5, labelRelief = "flat"))
    expect_equal(class(p), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    p <- with(mtcars, l_plot3D(wt, mpg, hp, by = data.frame(am = am, gear = gear, cyl  = cyl),
                               color = "blue",
                               layout = "wrap", labelLocation = c("bottom"),
                               labelBackground = "lightblue", labelForeground = "red",
                               labelBorderwidth = 5, labelRelief = "flat"))
    expect_equal(class(p), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
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
    s <- l_serialaxes(iris, sequence = sample(colnames(iris), 10, replace = TRUE),
                      by = iris$Species, scaling = "observation")
    expect_equal(class(s), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    s <- l_serialaxes(iris, sequence = sample(colnames(iris), 10, replace = TRUE),
                      by = iris$Species, scaling = "observation", axesLayout = "parallel",
                      layout = "wrap")
    expect_equal(class(s), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
})

test_that("test all possible 'by's", {
    p <- l_plot(iris)
    # by is a data.frame
    fp <- l_facet(p, layout = "grid", by = data.frame(iris$Species, iris$Species))
    expect_equal(class(fp), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    # by is a list
    fp <- l_facet(p, layout = "grid", by = list(iris$Species, iris$Species))
    expect_equal(class(fp), c("l_facet_grid", "l_facet",    "l_compound", "loon" ))
    h <- l_hist(iris)
    # by is a vector
    fp <- l_facet(h, layout = "wrap", by = iris$Species)
    expect_equal(class(fp), c("l_facet_wrap", "l_facet",    "l_compound", "loon" ))
})

test_that("test separate layouts", {
    p <- l_plot(iris, by = iris$Species,
                layout = "separate")
    expect_equal(class(p), c("l_facet",    "l_compound", "loon" ))
    p <- l_plot3D(iris, by = iris$Species,
                  layout = "separate")
    expect_equal(class(p), c("l_facet",    "l_compound", "loon" ))
    p <- l_hist(iris, by = iris$Species,
                layout = "separate")
    expect_equal(class(p), c("l_facet",    "l_compound", "loon" ))
    s <- l_serialaxes(iris, by = iris$Species,
                      layout = "separate")
    expect_equal(class(s), c("l_facet",    "l_compound", "loon" ))
})
