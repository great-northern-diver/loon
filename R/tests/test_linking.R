if (requireNamespace("testthat", quietly = TRUE)) {

    library(testthat)
    library(loon)
    context("test_linking")

    test_that("example works", {

        ########################################## l_plot ##########################################
        p <- l_plot(iris, color = iris$Species, linkingGroup = "iris")
        color <- p['color']
        ########## l_plot ##########
        pp <- l_plot(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_plot3D ##########
        pp <- l_plot3D(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_hist ##########
        pp <- l_hist(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_serialaxes ##########
        pp <- l_serialaxes(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########################################## l_plot3D ##########################################
        p <- l_plot3D(iris, color = iris$Species, linkingGroup = "iris")
        color <- p['color']
        ########## l_plot ##########
        pp <- l_plot(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_plot3D ##########
        pp <- l_plot3D(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_hist ##########
        pp <- l_hist(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_serialaxes ##########
        pp <- l_serialaxes(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########################################## l_hist ##########################################
        p <- l_hist(iris, color = iris$Species, linkingGroup = "iris")
        color <- p['color']
        ########## l_plot ##########
        pp <- l_plot(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_plot3D ##########
        pp <- l_plot3D(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_hist ##########
        pp <- l_hist(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_serialaxes ##########
        pp <- l_serialaxes(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########################################## l_serialaxes ##########################################
        p <- l_serialaxes(iris, color = iris$Species, linkingGroup = "iris")
        color <- p['color']
        ########## l_plot ##########
        pp <- l_plot(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_plot3D ##########
        pp <- l_plot3D(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_hist ##########
        pp <- l_hist(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
        ########## l_serialaxes ##########
        pp <- l_serialaxes(iris, linkingGroup = "iris")
        expect_equal(pp['color'], color)
    })


}
