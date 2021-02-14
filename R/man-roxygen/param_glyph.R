#' @param glyph the visual representation of the point. Argument values can be any of
#' \itemize{
#' \item{the string names of primitive glyphs:
#'    \itemize{
#'    \item{\code{circles}: "circle", "ccircle", "ocircle";}
#'    \item{\code{squares or boxes}: "square", "csquare", "osquare";}
#'    \item{\code{triangles}: "triangle", "ctriangle", "otriangle";}
#'    \item{\code{diamonds}: "diamond", "cdiamond", or "odiamond".}
#'    }
#'    Note that prefixes "c" and "o" may be thought of as closed and open, respectively.
#'    The set of values are returned by \code{\link{l_primitiveGlyphs}()}.
#'    }
#' \item{the string names of constructed glyphs:
#'    \itemize{
#'    \item{\code{text as glyphs}: see \code{\link{l_glyph_add_text}()}}
#'    \item{\code{point ranges}: see \code{\link{l_glyph_add_pointrange}()}}
#'    \item{\code{polygons}: see \code{\link{l_glyph_add_polygon}()}}
#'    \item{\code{parallel coordinates}: see \code{\link{l_glyph_add_serialaxes}()}}
#'    \item{\code{star or radial axes}: see \code{\link{l_glyph_add_serialaxes}()}}
#'    \item{\code{or any plot created using R}: see \code{\link{l_make_glyphs}()}}
#'    }
#'    Note that glyphs are constructed and given a stringname to be used in the inspector.
#'    }
#' }
