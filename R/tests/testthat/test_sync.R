context("test_sync")

test_that("test default sync", {

    p <- l_plot(as.character(iris$Species), color = "#FF0000") # red
    col <- hex12tohex6(unique(p['color']))
    expect_equal(col, "#FF0000")

    l_configure(p, linkingGroup = "iris", sync = "push")

    ########################### "pull" (default sync) ###########################
    expect_warning(
        q <- l_plot(iris$Petal.Width,
                    linkingGroup = "iris",
                    color = "#008000", # green
                    size = 6,
                    selected = TRUE,
                    active = sample(c(T, F), size = 150, replace = TRUE),
                    glyph = "triangle")
    )
    # color, size, selected and active are linked
    col <- hex12tohex6(unique(q['color']))
    expect_equal(col, "#FF0000")

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
    expect_warning(h <- l_hist(iris$Petal.Length,
                               linkingGroup = "iris",
                               color = "#FF0000", # red
                               selected = TRUE))

    selected <- all(!h['selected'])
    expect_true(selected)

    col <- hex12tohex6(unique(h['color']))
    expect_equal(col, "#FF0000")

    ################## serialaxes
    expect_warning(s <- l_serialaxes(iris[, -5],
                                     linkingGroup = "iris",
                                     color = "#008000", # green
                                     active = FALSE))

    active <- all(s['active'])
    expect_true(active)

    col <- hex12tohex6(unique(s['color']))
    expect_equal(col, "#FF0000")

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
        pf <- l_plot(iris$Sepal.Length, iris$Petal.Length,
                     linkingGroup = "iris",
                     by = iris$Species,
                     size = 10)
    )
    size <- unique(unlist(pf['size']))
    expect_equal(size, as.numeric(l_getOption("size")))

    ### pairs
    expect_warning(
        ppairs <- l_pairs(iris[, 1:3],
                          showHistograms = TRUE,
                          showSerialAxes = TRUE,
                          linkingGroup = "iris",
                          color = "#008000") # green
    )
    col <- hex12tohex6(unique(unlist(ppairs['color'])))
    expect_equal(col, "#FF0000")

    ### ts
    expect_warning(
        pts <- l_plot(decompose(co2),
                      linkingGroup = "iris",
                      color = "#008000") # green
    )
    col <- hex12tohex6(unique(pts[[1]]['color'][1:150]))
    expect_equal(col, "#FF0000")

    ########################### "push" ###########################
    q1 <- l_plot(iris$Petal.Length, linkingGroup = "iris",
                 sync = "push",
                 color = "#008000", # green
                 size = 8,
                 glyph = "ocircle")

    col <- hex12tohex6(unique(q['color']))
    expect_equal(col, "#008000")

    size <- unique(q['size'])
    expect_equal(size, 8)

    glyph <- unique(q['glyph'])
    expect_true(glyph != "ocircle")
    expect_true(glyph == "triangle")

    ###################### test function `l_getDeprecatedLinkedVar`
    getDeprecatedLinkedVar <- l_getDeprecatedLinkedVar(q1,
                                                       args = list(
                                                           color = rep("green", 150),
                                                           selected = sample(c(T, F), size = 150,
                                                                             replace = TRUE),
                                                           active = rep(FALSE, 150),
                                                           size = rep(6, 150)
                                                       )
    )

    expect_equal(getDeprecatedLinkedVar,
                 c("selected", "active", "size"))
})
