\name{l_plot_arguments}
\alias{l_plot_arguments}
\title{Arguments common to l_plot functions}
\description{
Like \code{\link{plot}} in \R, \code{\link{l_plot}} is
the generic plotting function for objects in \code{loon}.

This is the workhorse of \code{loon} and is often a key part of many
other displays (e.g. \code{\link{l_pairs}} and \code{\link{l_navgraph}})

Because plots in \code{loon} are interactive, the functions which create them
have many arguments in common.  The value of these arguments become `infostates` once the plot is instantiated.  These can be accessed and set using the usual \code{R} square bracket operators `[]`  and `[]<-` using the statename as a string.  The state names can be found from an instantiated \code{loon} plot either via \code{\link{l_info_states}()} or, more in keeping with the \code{R} programming style, via \code{\link{names}()} (uses the method \code{\link{names.loon}()} for \code{loon} objects).

The same state names can be passed as arguments with values to a \code{l_plot()} call.
As arguments many of the common ones are desribed below.
}
\details{
    The interactive displays permit a number of direct interactions using the
    mouse and keyboard, these include: zooming towards the mouse cursor using
    the mouse wheel, panning by right-click dragging and various selection
    methods using the left mouse button such as sweeping, brushing and
    individual point selection. See the documentation for \code{\link{l_plot}}
    for more details about the interaction gestures.
}
\arguments{
    \item{x}{the \code{x} and \code{y} arguments provide the x and y coordinates for the plot.  Any reasonable way of defining the coordinates is acceptable.  See the function \code{\link{xy.coords}} for details.  If supplied separately, they must be of the same length.}

    \item{y}{argument description is as for the \code{x} argument above.}

    \item{by}{loon plots can be separated by some variables into multiple panels. This argument can take a \code{\link{formula}}, n dimensional state names (see \code{\link{l_nDimStateNames}}) an n-dimensional vector and data.frame or a list of same lengths n as input.}

    \item{on}{if the \code{x} or \code{y} is a formula, an optional data frame containing the variables in the \code{x} or \code{by}. If the variables are not found in data, they are taken from environment, typically the environment from which the function is called.}

    \item{layout}{layout facets as 'grid', 'wrap' or 'separate'}

    \item{connectedScales}{Determines how the scales of the facets are to be connected depending on which layout is used.}

    \item{linkingGroup}{a string naming a group of plots to be linked.
    All plots with the same \code{linkingGroup} will have the same values of their
    linked states (see \code{\link{l_getLinkedStates}()}
    and \code{\link{l_setLinkedStates}()}).}

    \item{linkingKey}{an \code{n}-dimensional character vector of unique entries.
    The entries identify which points match other points in other plots.
    Default is \code{c("0", "1", ..., "n-1")} (for numerical \code{n}).}

    \item{itemLabel}{an \code{n}-dimensional character vector whose values are
    displayed in a pop-up box over any point whenever the mouse hovers over that
    point (provided \code{showItemLabels = TRUE}).

    This action is commonly known as providing a "tool tip".
    Note that all objects drawn in any layer of a plot (e.g. maps) will have an \code{itemLabel}.}

    \item{showItemLabels}{a logical (default \code{FALSE}) which indicates
    whether the "tool tip" \code{itemLabel} is to be displayed whenever
    the mouse hovers over it.}

    \item{color}{colours of points (default "grey60"); colours are repeated until matching the number points,}
    \item{glyph}{\describe{the visual representation of the point. Argument values can be any of
    \item{the string names of primitive glyphs:}{\describe{\item{circles}{"circle", "ccircle", "ocircle",}\item{squares or boxes}{"square", "csquare", "osquare",}\item{triangles}{"triangle", "ctriangle", "otriangle",}\item{diamonds}{"diamond", "cdiamond", or "odiamond".} Note that prefixes "c" and "o" may be thought of as closed and open, respectively. The set of values are returned by \code{\link{l_primitiveGlyphs}()}. }
    }
    \item{the string names of constructed glyphs}{\describe{
        \item{text as glyphs}{see \code{\link{l_glyph_add_text}()}}
        \item{point ranges}{see \code{\link{l_glyph_add_pointrange}()}}
        \item{polygons}{see \code{\link{l_glyph_add_polygon}()}}
        \item{parallel coordinates}{see \code{\link{l_glyph_add_serialaxes}()}}
        \item{star or radial axes}{see \code{\link{l_glyph_add_serialaxes}()}}
        \item{or any plot created using \code{R}}{see \code{\link{l_make_glyphs}()}}

       Note that glyphs are constructed and given a stringname to be used in the inspector.}
       }
       }
       }

    \item{size}{size of the symbol (roughly in terms of area)}

    \item{active}{a logical determining whether points appear or not
        (default is TRUE for all points). If a logical vector is given of length
        equal to the number of points, then it identifies which points appear (TRUE)
        and which do not (FALSE).}

    \item{selected}{a logical determining whether points appear selected at first
        (default is FALSE for all points). If a logical vector is given of length
        equal to the number of points, then it identifies which points are (TRUE)
        and which are not (FALSE).}

    \item{xlabel}{Label for the horizontal (x) axis. If missing,
        one will be inferred from \code{x} if possible.}

    \item{ylabel}{Label for the vertical (y) axis. If missing,
        one will be inferred from \code{y} (or \code{x}) if possible.}

    \item{title}{Title for the plot, default is an empty string.}

    \item{minimumMargins}{the minimal size (in pixels) of the margins around the plot (bottom, left, top, right)}

    \item{showLabels}{logical to determine whether axes label (and title) should
        be presented.}

    \item{showScales}{logical to determine whether numerical scales should
        be presented on both axes.}

    \item{showGuides}{logical to determine whether to present background guidelines
        to help determine locations.}

    \item{guidelines}{colour of the guidelines shown when \code{showGuides = TRUE} (default "white").}

    \item{guidesBackground}{colour of the background to the guidelines shown when
        \code{showGuides = TRUE} (default "grey92").}

    \item{foreground}{foreground colour used by all other drawing (default "black").}

    \item{background}{background colour used for the plot (default "white")}

    \item{parent}{a valid Tk parent widget path. When the parent widget is
        specified (i.e. not \code{NULL}) then the plot widget needs to be placed using
        some geometry manager like \code{\link{tkpack}} or \code{\link{tkplace}} in
        order to be displayed. See the examples below.}

    \item{...}{named arguments to modify plot states.}
}
\examples{
   \dontrun{
    # default use as scatterplot

    p1 <- with(iris, l_plot(x = Sepal.Length,
                            y = Sepal.Width,
                            color=Species,
                            title = "Sepal sizes"))

    # The names of the info states that can be
    # accessed or set.  They can also be given values as
    # arguments to l_plot.default()
    names(p1)
    versicolor <- (iris$Species == "versicolor")
    p1["size"] <- 10
    p1["glyph"][versicolor]<- "csquare"
    p1["minimumMargins"][1] <- 100
  }
}
\seealso{the demos \code{demo(l_glyph_sizes, package = "loon")}, \code{demo(l_glyphs, package = "loon")},
    and \code{demo(l_make_glyphs, package = "loon")}.
}
\concept{two-dimensional plotting functions}
\concept{loon interactive states}
