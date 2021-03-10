#if (requireNamespace("testthat", quietly = TRUE)) {
#
#    library(testthat)
library(loon)
context("test_NA")

test_that("a warning is given when some data are removed", {

    x <- c(rnorm(20), rep(NA, 5))[sample(1:25, 25)]
    y <- c(1:20, rep(NA, 5))[sample(1:25, 25)]
    expect_warning(l_plot(x, y), "Removed")
    expect_warning(l_plot3D(x, y, y), "Removed")
    expect_warning(l_hist(x), "Removed")
    expect_warning(l_serialaxes(data = data.frame(x = x, y = y)), "Removed")
})

test_that("color, size, linkingKey, glyph, all match when NAs are present", {
    x <- c(rnorm(20), rep(NA, 5))[sample(1:25, 25)]
    y <- c(1:20, rep(NA, 5))[sample(1:25, 25)]
    n <- length(x)
    color <- sample(c("red", "blue", "green"), n, replace = TRUE)
    size <- sample(1:10, n, replace = TRUE)
    linkingKey <- paste0("key ",  1:n)
    itemLabels <- paste0("point ",  1:n)
    glyphs <- sample(c("triangle", "circle", "square"), n, replace = TRUE)

    expect_warning(p <- l_plot(x, y, color = color, size = size,  linkingKey = linkingKey,
                               itemLabel = itemLabels, glyph = glyphs), "Remove")
    expect_warning(p_3d <- l_plot3D(x, y, y, color = color, size = size, linkingKey = linkingKey,
                                    itemLabel = itemLabels, glyph = glyphs), "Remove")
    expect_equal(p["x"], p_3d["x"])
    # expect_equal(p["color"], p_3d["color"])
    expect_equal(p["size"], p_3d["size"])
    expect_equal(p["glyph"], p_3d["glyph"])
    expect_equal(p["linkingKey"], p_3d["linkingKey"])
    expect_equal(p["itemLabel"], p_3d["itemLabel"])
    expect_warning(h <- l_hist(x + y, color = color, linkingKey = linkingKey), "Remove")
    # expect_equal(p["color"], h["color"])
    expect_equal(p["linkingKey"], h["linkingKey"])
    expect_warning(sa <- l_serialaxes(data = data.frame(x = x, y = y), color = color, linkingKey = linkingKey),
                   "Remove")
    # expect_equal(p["color"], sa["color"])
    expect_equal(p["linkingKey"],sa["linkingKey"])

})


test_that("layers warn when NAs are removed", {
    x <- rnorm(25)
    y <- rnorm(25)
    n <- length(x)
    x[3:4] <- NA
    y[4:5] <- NA
    p <- l_plot()
    group <- rep(1:5, each = 5)
    rgroup <- sample(1:5, n, replace = TRUE)
    expect_warning(l_layer_polygons(p,
                                    x = x,
                                    y = y,
                                    group = group), "Removed")
    expect_warning(l_layer_lines(p,
                                 x = x,
                                 y = y,
                                 group = group), "Removed")

    expect_warning(l_layer_rectangles(p,
                                      x = list(c(0,1), c(1,2), c(2,NA), c(5,6)),
                                      y = list(c(0,1), c(NA,2), c(0,1), c(3,4)),
                                      color = c('red', 'blue', 'green', 'orange'),
                                      linecolor = "black"), "Removed")

    expect_warning(l_layer_texts(p,
                                 x = x,
                                 y = y,
                                 text = sample(month.abb, n, replace = TRUE)), "Removed")


    expect_warning(l_layer_points(p,
                                  x = x,
                                  y = y), "Removed")
    xx <- co2
    xx[100] <- NA
    xx[200] <- NA
    expect_warning(l_layer_line(p, 1:length(xx), xx)
    )


    expect_error(l_layer_polygon(p, x = x, y = y))


    x <- rep(NA, 10)
    y <- 1:10
    color <- rep("red", 10)
    expect_warning(l_plot(x, y, color = color), "No valid input")
    expect_warning(l_plot3D(x, y, x, color = color),  "No valid input")

    levels <- LETTERS[1:4]

    foo <- factor(rep(NA, n), levels = levels)
    selection <- sample(1:n, 9, replace = FALSE)
    for (i in selection) {
        foo[i] <- sample(levels, 1)
    }
    foo
    expect_warning(l_hist(foo), "Removed")
})


#}
