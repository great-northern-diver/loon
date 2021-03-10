context("test_sync")

test_that("test pull", {
    n <- 149
    iris_ <- iris[seq(n), ]
    p <- l_plot(as.character(iris_$Species), color = "red") # red
    # expect_equal(unique(p['color']), l_hexcolor("red"))

    l_configure(p, linkingGroup = "iris_", sync = "push")

    ########################### "pull" (default sync) ###########################
    expect_warning(
        q <- l_plot(iris_$Petal.Width,
                    linkingGroup = "iris_",
                    color = "green", # green
                    size = 6,
                    selected = TRUE,
                    active = sample(c(T, F), size = n, replace = TRUE),
                    glyph = "triangle")
    )
    # color, size, selected and active are linked
    # expect_equal(unique(q['color']), l_hexcolor("red"))

    size <- unique(q['size'])
    expect_equal(as.character(size), l_getOption("size"))

    selected <- all(!q['selected'])
    expect_true(selected)

    active <- all(q['active'])
    expect_true(active)

    # glyph are not linked
    glyph <- unique(q['glyph'])
    expect_true(glyph != l_getOption("glyph"))

    ################## histogram
    expect_warning(h <- l_hist(iris_$Petal.Length,
                               linkingGroup = "iris_",
                               color = "red", # red
                               selected = TRUE))

    selected <- all(!h['selected'])
    expect_true(selected)

    # expect_equal(unique(h['color']), l_hexcolor("red"))

    ################## serialaxes
    expect_warning(s <- l_serialaxes(iris_[, -5],
                                     linkingGroup = "iris_",
                                     color = "green", # green
                                     active = FALSE))

    active <- all(s['active'])
    expect_true(active)

    # expect_equal(unique(s['color']), l_hexcolor("red"))

    ################## graph
    pp <- l_plot(1:4, linkingGroup = "foo", sync = "pull",
                color = c("red", "blue", "green", "yellow"))
    gg <- loongraph(
        nodes = c("A", "B", "C", "D"),
        from = c("A", "A", "B", "B", "C"),
        to   = c("B", "C", "C", "D", "D")
    )

    # create a loon graph plot
    expect_warning(qq <- l_graph(gg, linkingGroup = "foo", size = 8))
    size <- unique(qq['size'])
    expect_equal(size, as.numeric(l_getOption("size")))

    ################## facets
    ### facets
    expect_warning(
        pf <- l_plot(iris_$Sepal.Length, iris_$Petal.Length,
                     linkingGroup = "iris_",
                     by = iris_$Species,
                     size = 10)
    )
    size <- unique(unlist(pf['size']))
    expect_equal(size, as.numeric(l_getOption("size")))

    ### pairs
    expect_warning(
        ppairs <- l_pairs(iris_[, 1:3],
                          showHistograms = TRUE,
                          showSerialAxes = TRUE,
                          linkingGroup = "iris_",
                          color = "green") # green
    )

    # expect_equal(unique(unlist(ppairs['color'])), l_hexcolor("red"))

    ### ts
    expect_warning(
        pts <- l_plot(decompose(co2),
                      linkingGroup = "iris_",
                      color = "green") # green
    )
    # expect_equal(unique(pts[[1]]['color'][1:n]), l_hexcolor("red"))
})


test_that("test push", {
    n <- 149
    iris_ <- iris[seq(n), ]
    color <- sample(c("red", "blue", "green"),
                    size = n,
                    replace = TRUE)
    size <- sample(2:10, size = n, replace = TRUE)
    p <- l_plot(iris_, linkingGroup = "iris_1")
    p['color'] <- color
    p["size"] <- size

    q <- l_plot(iris_, by = iris_$Species,
                color = "black",
                linkingGroup = "iris_1", sync = "push")
    # only the color is modified
    # expect_true(all(q[[1]]['color'] == l_hexcolor("black")))
    # the size of each plot should be the same with the random sample
    expect_true(all(q[[1]]['size'] == size[1:50]))
    expect_true(all(q[[2]]['size'] == size[51:100]))
    expect_true(all(q[[3]]['size'] == size[101:n]))

    expect_message(p1 <- l_plot(iris_, linkingGroup = "iris_1", sync = "push"))
    expect_true(all(p['size'] == p1['size']))
    expect_false(all(p['size'] == size))

    p['size'] <- size
    q1 <- l_plot(iris_, by = rep(1, n),
                 select = TRUE,
                 linkingGroup = "iris_1",
                 sync = "push")
    expect_true(all(p['selected']))
    expect_true(all(p['size'] == size))

    # serialaxes
    s <- l_serialaxes(iris_, color = color,
                      linewidth = size,
                      selected = FALSE, # default setting it will not be pushed
                      linkingGroup = "iris_1",
                      sync = "push")

    expect_true(all(p['size'] == size))
    expect_false(all(p['selected']))
    # expect_true(all(p['color'] == l_hexcolor(color)))

    s1 <- l_serialaxes(iris_, by = iris_$Species,
                       linkingGroup = "iris_1",
                       color = "black",
                       sync = "push")


    # expect_true(all(s['color'] == l_hexcolor("black")))
    expect_true(all(s['linewidth'] == size))

    pair <- l_pairs(iris_[, 1:3], linkingGroup = "iris_1", sync = "push",
                    color = "red")

    expect_true(all(s['linewidth'] == size))
    # expect_true(all(s['color'] == l_hexcolor("red")))

    # push test
    # the states of the new plot are the default
    p1 <- l_plot(1:10,
                 color = "red",
                 linkingGroup = "group1",
                 size = 10,
                 selected = TRUE)
    p2 <- l_plot(1:10,
                 color = l_getOption("color"),
                 linkingGroup = "group1",
                 size = l_getOption("size"),
                 sync = "push",
                 selected = FALSE)

    # expect_true(all(p1['color'] == l_hexcolor("gray60")))
    expect_true(all(p1['size'] == 4))
    expect_false(any(p1['selected']))
})
