% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/l_create_handle.R
\name{l_create_handle}
\alias{l_create_handle}
\title{Create a loon object handle}
\usage{
l_create_handle(target)
}
\arguments{
\item{target}{loon object specification (e.g. \code{".l0.plot"})}
}
\description{
This function can be used to create the loon object handles from
  a vector of the widget path name and the object ids (in the order of the
  parent-child relationships).
}
\details{
loon's plot handles are useful to query and modify plot states
  via the command line.

For more information run: \code{l_help("learn_R_intro.html#re-creating-object-handles")}
}
\examples{
if(interactive()){


# plot handle
p <- l_plot(x=1:3, y=1:3)
p_new <- l_create_handle(unclass(p))
p_new['showScales']

# glyph handle
gl <- l_glyph_add_text(p, text=LETTERS[1:3])
gl_new <- l_create_handle(c(as.vector(p), as.vector(gl)))
gl_new['text']

# layer handle
l <- l_layer_rectangle(p, x=c(1,3), y=c(1,3), color='yellow', index='end')
l_new <- l_create_handle(c(as.vector(p), as.vector(l)))
l_new['color']

# navigator handle
g <- l_graph(linegraph(completegraph(LETTERS[1:3])))
nav <- l_navigator_add(g)
nav_new <- l_create_handle(c(as.vector(g), as.vector(nav)))
nav_new['from']

# context handle
con <- l_context_add_context2d(nav)
con_new <- l_create_handle(c(as.vector(g), as.vector(nav), as.vector(con)))
con_new['separator']

}

}
\seealso{
\code{\link{l_getFromPath}}
}
