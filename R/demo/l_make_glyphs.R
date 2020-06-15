
# Produce any statistical glyph from any drawing function(on png)
# Add these glyphs as the point symbols for the loon scatterplot.
#
# Here we look at Italian growing areas corresponding to the
# olive oil data.
# The location of each area is a point in the loon plot
# We introduce a new glyph for each point/growing area based on a
# scatterplot of the fatty acid pair (linoleic, arachidic)
# for all olive oils in that growing area.
# Then a map of Italy is layered on the scatterplot.

if(requireNamespace("maps", quietly = TRUE)) {
    local({

        p <- with(oliveLocations,
                  l_plot(x = long, y = lat,
                         xlabel = "longitude", ylabel = "latitude",
                         showGuides = TRUE, color = "darkolivegreen4",
                         itemLabel = Area, showItemLabels = TRUE,
                         title = "Olive oil growing areas")
        )

        readline("Hit <Return> to construct the statistical images (9 in total)")
        #
        # Now we construct statistical glyphs for each area.
        # First split the olive data into a list by Area
        # in the order of the Area from the scatterplot
        #
        oliveByArea <- lapply(oliveLocations$Area,
                              FUN = function(area) {
                                  subset(olive,
                                         Area == area)})

        # Want the scatterplot glyphs to all be on the
        # same range and to have the Area superimposed
        # on top.
        # We'll use base graphics to construct the glyphs
        # Will be producing a scatterplot of two fatty acids
        # for each growing area
        xlim <- extendrange(olive[,"linoleic"], f = .25)
        ylim <- extendrange(olive[,"arachidic"], f = .25)
        xlim10 <- extendrange(olive[,"linoleic"], f = .10)
        ylim10 <- extendrange(olive[,"arachidic"], f = .10)
        pointcol <- adjustcolor("steelblue", 0.5)
        textcol <- "steelblue"
        #
        # Now construct the statistical images
        scatterplot_images <-
            l_make_glyphs(
                oliveByArea,  # data
                width = 300, height = 300,  # image size
                draw_fun = function(area) { # draw function for each Area
                    # Following line included *only* for demo
                    devAskNewPage(ask = FALSE)
                    # get rid of borders for the png device
                    par(mai = rep(0,4),
                        mar = rep(0,4),
                        oma = rep(0,4))

                    # Draw the plot we want
                    # Just a scatterplot of two of the fatty
                    # acids
                    plot(area$linoleic, area$arachidic,
                         xlim = xlim, ylim = ylim,
                         ann = FALSE, xaxt = "n", yaxt = "n", bty = "n",
                         pch=19, cex = 2, col = pointcol)
                    # Add text of Area name
                    text(mean(xlim), min(ylim10), # Bottom of plot
                         paste(area$Area[1]),
                         cex = 3.5, col = textcol)
                }
            )

        readline("Hit <Return> to get images for viewer")
        # Look at the images
        l_imageviewer(scatterplot_images)

        readline("Hit <Return> to assign statistical images as point glyphs")

        # Create the glyphs from the images for the l_plot
        glyphs <- l_glyph_add_image(p,
                                    scatterplot_images,
                                    # A meaningful label for this glyph
                                    label = "plot(linoleic, arachidic)"
        )
        # Change the plot glyphs to these image glyphs
        p["glyph"] <- glyphs

        readline("Hit <Return> to finish plot by layering a map of Italy")

        # Now layer a map of Italy from the maps package
        l_layer(p,
                maps::map('italy', fill = FALSE, plot = FALSE),
                label = "Italy",
                color = "cornsilk",
                index = "end")
        # Scale the plot to the all layers
        l_scaleto_world(p)

    }) # End local
}
