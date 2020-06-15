
# This uses l_make_glyphs to build standard periodic table labels
# for glyphs of the elements data from loon.data package.
#

if(requireNamespace("loon.data", quietly = TRUE)) {
    local({
        data("elements", package = "loon.data")

        # A draw function for each element
        draw_element_box <- function(symbol,
                                     name, number,
                                     mass_number,
                                     mass, col) {
            if (missing(col)) col <- "white"
            oldPar <-  par(bg = col, mar = rep(1, 4))
            #
            # Following line included *only* for demo
            devAskNewPage(ask = FALSE)

            plot(NA, xlim = c(0,1), ylim = c(0, 1), axes=FALSE, ann = FALSE)
            text(0.5, 0.6, labels = symbol, cex = 18)
            text(0.15, 1, labels = number, cex = 6, adj= c(0.5,1))
            text(0.5, 0.25, labels = name, cex = 6)
            text(0.5, 0.11, labels = mass_number, cex = 3)
            text(0.5, 0.01, labels = mass, cex = 3)
            box()

            par(oldPar)
        }

        # Get the categories
        colIDs <- paste(elements$Category, elements$Subcategory)
        # Get a loon palette function
        colFn <- color_loon()
        # Get colors identified with categories
        tableCols <- colFn(colIDs)
        #
        # A function to an element box image for each element.

        make_element_boxes <- function(elements,
                                       cols,
                                       width = 500,
                                       height = 500) {
            if (missing(cols)) cols <- rep("white", nrow(elements))
            listOfElements <- lapply(1:nrow(elements),
                                     FUN = function(i) {
                                         list(vals = elements[i,],
                                              col = cols[i])
                                     })
            # glyphs created here
            l_make_glyphs(listOfElements,
                          draw_fun = function(element){
                              x <- element$vals
                              col <- element$col
                              draw_element_box(symbol = x$Symbol,
                                               name = x$Name,
                                               number = x$Number,
                                               mass_number = x$Mass_number,
                                               mass = x$Mass,
                                               col = col)
                          },
                          width = width,
                          height = height)
        }

        # Construct the glyphs
        boxGlyphs <- make_element_boxes(elements, cols = tableCols)

        readline("Hit <Return> to scroll through the elements")
        # Look at the images
        l_imageviewer(boxGlyphs)

        readline("Hit <Return> to lay them out in the periodic table.")

        # Get a couple of plots
        periodicTable <- l_plot(x = elements$x, y = elements$y,
                                xlabel = "", ylabel = "",
                                title = "Periodic Table of the Elements",
                                linkingGroup = "elements",
                                color = tableCols)

        # Add the images as possible glyphs

        bg <- l_glyph_add_image(periodicTable,
                                images = boxGlyphs,
                                label = "Symbol box")

        # Set this to be the glyph
        periodicTable['glyph'] <- bg

        message("Enlarge the window so that all elements can be seen. \n")

        readline(
            "Hit <Return> to construct a plot of each element's Density versus its Mass.")
        #
        # Get a second plot that shows the periodicity
        #
        # First some itemlabels
        elementLabels <- with(elements,
                              paste("   ", Number, Symbol, "\n",
                                    "   ", Name, "\n",
                                    "   ", Mass
                              )
        )

        periodicPlot  <- l_plot(x = elements$Mass, y = elements$Density,
                                xlabel = "Mass", ylabel = "Density",
                                itemLabel = elementLabels,
                                showItemLabels = TRUE,
                                linkingGroup = "elements",
                                color = tableCols)


        bg2 <- l_glyph_add_image(periodicPlot,
                                 images = boxGlyphs,
                                 label = "Symbol box")


        readline(
            "Hit <Return> to show the noble gases by their 'Symbol Box' glyph")

        nobleGases <- elements$Subcategory == "Noble gas"
        periodicPlot["glyph"][nobleGases] <- "glyph0"
        readline(
            "Hit <Return> to show the reactive nonmetals by their 'Symbol Box' glyph")
        reactiveNonMetals <- elements$Subcategory == "Reactive nonmetal"
        periodicPlot["glyph"][reactiveNonMetals] <- "glyph0"

        readline("Hit <Return> to focus on the noble gases and the reactive nonmetals")
        periodicPlot["active"] <- nobleGases | reactiveNonMetals
        l_scaleto_active(periodicPlot)
        periodicPlot["active"] <- TRUE

        readline("Hit <Return> to return plot to closed circles and scale.")
        periodicPlot["glyph"] <- "ccircle"
        l_scaleto_world(periodicPlot)

        message(paste0("Suggestions: \n",
                       "     - Brush/Select in either plot to explore the periodicity of the elements. \n",
                       "     - On the table brushing whole rows or whole columns is interesting. \n",
                       "     - Try selecting by colour and setting the glyph to the symbol boxes. \n",
                       "     - So activating only selected colours and panning and zooming in the Mass/Density plot. \n",
                       "     - Activate only selected colours and then try panning and zooming in the Mass/Density plot. \n"))

        message()


    }) # End local
}
