if (requireNamespace("testthat", quietly = TRUE)) {

    library(testthat)
    library(loon)
    context("test_NA")

    test_that("example works", {

        x <- c(rnorm(90), rep(NA, 10))[sample(1:100, 100)]
        y <- c(1:90, rep(NA, 10))[sample(1:100, 100)]
        color <- sample(c("red", "blue", "green"), 100, replace = TRUE)[sample(1:100, 100)]
        size <- sample(1:10, 100, replace = TRUE)[sample(1:100, 100)]
        linkingKey <- 1:100
        group1 <- sample(1:50, 100, replace = TRUE)[sample(1:100, 100)]
        group2 <- sample(1:2, 100, replace = TRUE)[sample(1:100, 100)]
        ########## l_plot ##########
        expect_warning(l_plot(x, y, color = color, size = size))
        ########## l_plot3D ##########
        expect_warning(l_plot3D(x, y, y, color = color, size = size))
        ########## l_hist ##########
        expect_warning(l_hist(x, color = color))
        ########## l_serialaxes ##########
        expect_warning(l_serialaxes(data = data.frame(x = x, y = y),
                                    linkingKey = linkingKey))
        ########## l_layer_polygons ##########
        p <- l_plot()
        expect_warning(l_layer_polygons(p,
                                        x = x,
                                        y = y,
                                        group = group1)
                       )
        ########## l_layer_lines ##########
        expect_warning(l_layer_lines(p,
                                     x = x,
                                     y = y,
                                     group = group2)
        )
        ########## l_layer_rectangles ##########
        expect_warning(l_layer_rectangles(p,
                                          x = x,
                                          y = y,
                                          group = rep(1:50, 2))
        )
        ########## l_layer_texts ##########
        expect_warning(l_layer_texts(p,
                                     x = x,
                                     y = y,
                                     text = as.character(x),
                                     color = color,
                                     size = size)
        )
        ########## l_layer_points ##########
        expect_warning(l_layer_points(p,
                                      x = x,
                                      y = y,
                                      color = color,
                                      size = size)
        )
        ########## l_layer_line ##########
        xx <- co2
        xx[100] <- NA
        xx[200] <- NA
        expect_warning(
            l_layer_line(p, 1:length(xx), xx)
        )
        ########## l_layer_polygon ##########
        expect_error(l_layer_polygon(p,
                                       x = x,
                                       y = y)
        )
        ############################## weird case
        x <- rep(NA, 10)
        y <- 1:10
        color <- rep("red", 10)
        expect_warning(l_plot(x, y, color = color),
                       "No valid input")
        expect_warning(l_plot3D(x, y, x, color = color),
                       "No valid input")
        ############################ l_hist.factor
        n <- 10
        levels <- LETTERS[1:4]
        foo <- factor(rep(NA, n), levels = levels)
        selection <- sample(1:n, 9, replace = FALSE)
        for (i in selection) {
            foo[i] <- sample(levels, 1)
        }
        foo
        l_hist(foo)
    })


}
