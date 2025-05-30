% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loonFacets.R
\name{loonFacets}
\alias{loonFacets}
\title{A helper function that produces facetted plots at the time of constructing a loon plot.}
\usage{
loonFacets(
  type,
  by,
  args,
  on,
  bySubstitute,
  layout = "grid",
  connectedScales = "both",
  byArgs,
  linkingGroup,
  sync,
  parent,
  factory_tclcmd,
  factory_path,
  factory_window_title,
  xlabel = "",
  ylabel = "",
  title = "",
  modifiedLinkedStates = character(0L),
  ...
)
}
\arguments{
\item{type}{the class name of the loon plot}

\item{by}{loon plot can be separated by some variables into mutiple panels.
This argument can take a \code{vector}, a \code{list} of same lengths or a \code{data.frame} as input.}

\item{args}{named list of N-dimensional arguments (e.g., x, y, selected, etc.)}

\item{on}{if the \code{by} is a formula, an optional data frame containing the variables in the \code{by}.
If variables in \code{by} is not found in data, the variables are taken from environment(formula),
typically the environment from which the function is called.}

\item{bySubstitute}{effectively a call of \code{substitute(by)} on the \code{by} arguments
used to generate warnings or errors.}

\item{layout}{layout facets as \code{'grid'}, \code{'wrap'} or \code{'separate'}}

\item{connectedScales}{Determines how the scales of the facets are to be connected depending
on which \code{layout} is used.  For each value of \code{layout}, the scales are connected
as follows:
\itemize{
\item{\code{layout = "wrap":}  Across all facets, when \code{connectedScales} is
   \itemize{
   \item{\code{"x"}, then  only the "x"  scales are connected}
   \item{\code{"y"}, then only the "y" scales are connected}
   \item{\code{"both"},  both "x" and "y" scales are connected}
   \item{\code{"none"},  neither "x" nor "y" scales are connected.}
   For any other value, only the "y" scale is connected.
   }
   }
\item{\code{layout = "grid":}  Across all facets, when \code{connectedScales} is
   \itemize{
   \item{\code{"cross"}, then only the scales in the same row and the same column are connected}
   \item{\code{"row"}, then both "x" and "y" scales of facets in the same row are connected}
   \item{\code{"column"}, then both "x" and "y" scales of facets in the same column are connected}
   \item{\code{"x"}, then all of the "x"  scales are connected (regardless of column)}
   \item{\code{"y"}, then all of the "y" scales are connected (regardless of row)}
   \item{\code{"both"},  both "x" and "y" scales are connected in all facets}
   \item{\code{"none"},  neither "x" nor "y" scales are connected in any facets.}
   }
   }
 }}

\item{byArgs}{further arguments to be used in determining how \code{by} will be used.}

\item{linkingGroup}{the string naming the group of plots to be linked to the facets.}

\item{sync}{string identifying how to synchronize the aesthetics with
linked plots (i.e., "push" or "pull", "pull" by default).}

\item{parent}{the parent loon widget}

\item{factory_tclcmd}{the tcl command to be given to loonPlotFactory.  For example, the
loon histogram tcl command is the string '::loon::histogram'}

\item{factory_path}{the tcl path given to loonPlotFactory to identify the widgets; for example,
the string 'hist' to prefix histograms.}

\item{factory_window_title}{the window title to be given to loonPlotFactory to better identify
the window for the user; for example, the string 'loon histogram'.}

\item{xlabel}{string to label the x direction of the facets}

\item{ylabel}{string to label the y direction of the facets}

\item{title}{string providing a title for the collection of facets}

\item{modifiedLinkedStates}{states of the plot to be synchronized with plots
in the same linking group.  Used with \code{sync = "push"}.}

\item{...}{named arguments to modify the `loon` widget states}
}
\description{
Facets across multiple panels can be created from an existing loonplot via \code{l_facet} or
directly at the time of the original loon plot call (without constructing the unfacetted loon plot itself).
\code{loonFacets} is that helper function called by the various loon plot creation function
(e.g., \code{l_plot}, \code{l_hist}, etc.).  at the time of their creation to
produce the facets. It should rarely be called directly by the user.

The function makes use of the general \code{loonPlotFactory} interface to tcl.

For detailed information on its common arguments, see the arguments of \code{\link{l_facet}} or those of the
loon plot constructor (e.g., \code{\link{l_plot}}, etc.)
}
\seealso{
\code{\link{l_facet}} and, for example, \code{\link{l_plot}}, \code{\link{l_hist}}, \code{\link{l_serialaxes}}
}
\keyword{internal}
