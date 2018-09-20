
## The suported point glyph primitives are: circle, square, triangle,
## and square. These glyphs are available for every scatterplot.
##
## So a call
##
## p['glyph'] <- "square"
##
## will change every point glyph to a square.
##
## Generally glyphs can not be linked among scatterplots, as nothing
## guarantees that a particular (image, text, boxplot, etc...) glyph
## is available for a scatterplot. However primitive shapes can be
## linked, but it must be done manually as this demo shows.
##
## An simpler way, but less secure, is to set glyph to be a linked
## state:
##
## states <- c(l_getLinkedStates(p1), 'glyph')
## l_setLinkedStates(p1, states)
## l_setLinkedStates(p2, states)


local({

    p1 <- with(iris,
               l_plot(Sepal.Length, Petal.Width, color=Species,
                      linkingGroup="iris"))
    p2 <- with(iris,
               l_plot(Petal.Length, Sepal.Width, color=Species,
                      linkingGroup="iris"))

    l_bind_state(p1, "glyph", function(W)syncglyphs(W))
    l_bind_state(p2, "glyph", function(W)syncglyphs(W))


    tosync <- c(p1, p2)

    isbusy <- FALSE
    syncglyphs <- function(widget) {
        if (isbusy) {return()}
        
        glyph <- l_cget(widget, "glyph")
        if (any(!(glyph %in% c("circle", "square",
                               "triangle", "diamond", "ocircle",
                               "osquare", "otriangle", "odiamond")))) {
            stop(paste("Widget", widget,
                       "has a glyph that is not a primitive glyph shape."))
        }
        
        isbusy <- TRUE
        for(p in tosync) {
            if (p != widget) {
                l_configure(p, glyph=glyph)
            }
        }
        isbusy <- FALSE
    }
    
    cat(paste("This demonstrates how a change in the shape of a primitive glyph",
              "can be forced on a pair of scatterplots. \n",
              "\t Select points in one plot, change the glyph to",
              "a different shape; second plot will follow course."))
    
}
)
