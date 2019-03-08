#' @title List the valid Tk color names
#'
#' @description The core of Loon is implemented in Tcl and Tk. Hence, when
#'   defining colors using color names, Loon uses the Tcl color representation
#'   and not those of R. The colors are taken from the Tk sources:
#'   \code{doc/colors.n}.
#'
#'
#'   If you want to make sure that the color names are represented exactly as
#'   they are in R then you can convert the color names to hexencoded color
#'   strings, see the examples below.
#'
#' @export
#'
#' @examples
#' # check if R colors names and TK color names are the same
#' setdiff(tolower(colors()), tolower(tkcolors()))
#' setdiff(tolower(tkcolors()), tolower(colors()))
#'
#' # hence there are currently more valid color names in Tk
#' # than there are in R
#'
#' # Let's compare the colors of the R color names in R and Tk
#' tohex <- function(x) {
#'     sapply(x, function(xi) {
#'         crgb <- as.vector(col2rgb(xi))
#'         rgb(crgb[1], crgb[2], crgb[3], maxColorValue = 255)
#'     })
#' }
#'
#' df <- data.frame(
#'     R_col = tohex(colors()),
#'     Tcl_col = hex12tohex6(l_hexcolor(colors())),
#'     row.names = colors(),
#'     stringsAsFactors = FALSE
#' )
#'
#' df_diff <- df[df$R_col != df$Tcl_col,]
#'
#' if (requireNamespace("grid", quietly = TRUE)) {
#'   grid::grid.newpage()
#'   grid::pushViewport(grid::plotViewport())
#'
#'   x_col <- grid::unit(0, "npc")
#'   x_R <- grid::unit(6, "lines")
#'   x_Tcl <- grid::unit(10, "lines")
#'
#'   grid::grid.text('color', x=x_col, y=grid::unit(1, "npc"),
#'                   just='left', gp=grid::gpar(fontface='bold'))
#'   grid::grid.text('R', x=x_R, y=grid::unit(1, "npc"), just='center',
#'                    gp=grid::gpar(fontface='bold'))
#'   grid::grid.text('Tcl', x=x_Tcl, y=grid::unit(1, "npc"), just='center',
#'                    gp=grid::gpar(fontface='bold'))
#'   for (i in 1:nrow(df_diff)) {
#'       y <- grid::unit(1, "npc") - grid::unit(i*1.2, "lines")
#'       grid::grid.text(rownames(df_diff)[i], x=x_col, y=y, just='left')
#'       grid::grid.rect(x=x_R, y=y, width=grid::unit(3, "line"),
#'                 height=grid::unit(1, "line"), gp=grid::gpar(fill=df_diff[i,1]))
#'       grid::grid.rect(x=x_Tcl, y=y, width=grid::unit(3, "line"),
#'                 height=grid::unit(1, "line"), gp=grid::gpar(fill=df_diff[i,2]))
#'   }
#' }
tkcolors <- function() {

    return(c("alice", "blue", "AliceBlue", "antique", "white", "AntiqueWhite",
             "AntiqueWhite1", "AntiqueWhite2", "AntiqueWhite3", "AntiqueWhite4",
             "aquamarine", "aquamarine1", "aquamarine2", "aquamarine3", "aquamarine4",
             "azure", "azure1", "azure2", "azure3", "azure4", "beige", "bisque",
             "bisque1", "bisque2", "bisque3", "bisque4", "black", "blanched",
             "almond", "BlanchedAlmond", "blue", "blue", "violet", "blue1",
             "blue2", "blue3", "blue4", "BlueViolet", "brown", "brown1", "brown2",
             "brown3", "brown4", "burlywood", "burlywood1", "burlywood2",
             "burlywood3", "burlywood4", "cadet", "blue", "CadetBlue", "CadetBlue1",
             "CadetBlue2", "CadetBlue3", "CadetBlue4", "chartreuse", "chartreuse1",
             "chartreuse2", "chartreuse3", "chartreuse4", "chocolate", "chocolate1",
             "chocolate2", "chocolate3", "chocolate4", "coral", "coral1",
             "coral2", "coral3", "coral4", "cornflower", "blue", "CornflowerBlue",
             "cornsilk", "cornsilk1", "cornsilk2", "cornsilk3", "cornsilk4",
             "cyan", "cyan1", "cyan2", "cyan3", "cyan4", "dark", "blue", "dark",
             "cyan", "dark", "goldenrod", "dark", "gray", "dark", "green",
             "dark", "grey", "dark", "khaki", "dark", "magenta", "dark", "olive",
             "green", "dark", "orange", "dark", "orchid", "dark", "red", "dark",
             "salmon", "dark", "sea", "green", "dark", "slate", "blue", "dark",
             "slate", "gray", "dark", "slate", "grey", "dark", "turquoise",
             "dark", "violet", "DarkBlue", "DarkCyan", "DarkGoldenrod", "DarkGoldenrod1",
             "DarkGoldenrod2", "DarkGoldenrod3", "DarkGoldenrod4", "DarkGray",
             "DarkGreen", "DarkGrey", "DarkKhaki", "DarkMagenta", "DarkOliveGreen",
             "DarkOliveGreen1", "DarkOliveGreen2", "DarkOliveGreen3", "DarkOliveGreen4",
             "DarkOrange", "DarkOrange1", "DarkOrange2", "DarkOrange3", "DarkOrange4",
             "DarkOrchid", "DarkOrchid1", "DarkOrchid2", "DarkOrchid3", "DarkOrchid4",
             "DarkRed", "DarkSalmon", "DarkSeaGreen", "DarkSeaGreen1", "DarkSeaGreen2",
             "DarkSeaGreen3", "DarkSeaGreen4", "DarkSlateBlue", "DarkSlateGray",
             "DarkSlateGray1", "DarkSlateGray2", "DarkSlateGray3", "DarkSlateGray4",
             "DarkSlateGrey", "DarkTurquoise", "DarkViolet", "deep", "pink",
             "deep", "sky", "blue", "DeepPink", "DeepPink1", "DeepPink2",
             "DeepPink3", "DeepPink4", "DeepSkyBlue", "DeepSkyBlue1", "DeepSkyBlue2",
             "DeepSkyBlue3", "DeepSkyBlue4", "dim", "gray", "dim", "grey",
             "DimGray", "DimGrey", "dodger", "blue", "DodgerBlue", "DodgerBlue1",
             "DodgerBlue2", "DodgerBlue3", "DodgerBlue4", "firebrick", "firebrick1",
             "firebrick2", "firebrick3", "firebrick4", "floral", "white",
             "FloralWhite", "forest", "green", "ForestGreen", "gainsboro",
             "ghost", "white", "GhostWhite", "gold", "gold1", "gold2", "gold3",
             "gold4", "goldenrod", "goldenrod1", "goldenrod2", "goldenrod3",
             "goldenrod4", "gray", "gray0", "gray1", "gray2", "gray3", "gray4",
             "gray5", "gray6", "gray7", "gray8", "gray9", "gray10", "gray11",
             "gray12", "gray13", "gray14", "gray15", "gray16", "gray17", "gray18",
             "gray19", "gray20", "gray21", "gray22", "gray23", "gray24", "gray25",
             "gray26", "gray27", "gray28", "gray29", "gray30", "gray31", "gray32",
             "gray33", "gray34", "gray35", "gray36", "gray37", "gray38", "gray39",
             "gray40", "gray41", "gray42", "gray43", "gray44", "gray45", "gray46",
             "gray47", "gray48", "gray49", "gray50", "gray51", "gray52", "gray53",
             "gray54", "gray55", "gray56", "gray57", "gray58", "gray59", "gray60",
             "gray61", "gray62", "gray63", "gray64", "gray65", "gray66", "gray67",
             "gray68", "gray69", "gray70", "gray71", "gray72", "gray73", "gray74",
             "gray75", "gray76", "gray77", "gray78", "gray79", "gray80", "gray81",
             "gray82", "gray83", "gray84", "gray85", "gray86", "gray87", "gray88",
             "gray89", "gray90", "gray91", "gray92", "gray93", "gray94", "gray95",
             "gray96", "gray97", "gray98", "gray99", "gray100", "green", "green",
             "yellow", "green1", "green2", "green3", "green4", "GreenYellow",
             "grey", "grey0", "grey1", "grey2", "grey3", "grey4", "grey5",
             "grey6", "grey7", "grey8", "grey9", "grey10", "grey11", "grey12",
             "grey13", "grey14", "grey15", "grey16", "grey17", "grey18", "grey19",
             "grey20", "grey21", "grey22", "grey23", "grey24", "grey25", "grey26",
             "grey27", "grey28", "grey29", "grey30", "grey31", "grey32", "grey33",
             "grey34", "grey35", "grey36", "grey37", "grey38", "grey39", "grey40",
             "grey41", "grey42", "grey43", "grey44", "grey45", "grey46", "grey47",
             "grey48", "grey49", "grey50", "grey51", "grey52", "grey53", "grey54",
             "grey55", "grey56", "grey57", "grey58", "grey59", "grey60", "grey61",
             "grey62", "grey63", "grey64", "grey65", "grey66", "grey67", "grey68",
             "grey69", "grey70", "grey71", "grey72", "grey73", "grey74", "grey75",
             "grey76", "grey77", "grey78", "grey79", "grey80", "grey81", "grey82",
             "grey83", "grey84", "grey85", "grey86", "grey87", "grey88", "grey89",
             "grey90", "grey91", "grey92", "grey93", "grey94", "grey95", "grey96",
             "grey97", "grey98", "grey99", "grey100", "honeydew", "honeydew1",
             "honeydew2", "honeydew3", "honeydew4", "hot", "pink", "HotPink",
             "HotPink1", "HotPink2", "HotPink3", "HotPink4", "indian", "red",
             "IndianRed", "IndianRed1", "IndianRed2", "IndianRed3", "IndianRed4",
             "ivory", "ivory1", "ivory2", "ivory3", "ivory4", "khaki", "khaki1",
             "khaki2", "khaki3", "khaki4", "lavender", "lavender", "blush",
             "LavenderBlush", "LavenderBlush1", "LavenderBlush2", "LavenderBlush3",
             "LavenderBlush4", "lawn", "green", "LawnGreen", "lemon", "chiffon",
             "LemonChiffon", "LemonChiffon1", "LemonChiffon2", "LemonChiffon3",
             "LemonChiffon4", "light", "blue", "light", "coral", "light",
             "cyan", "light", "goldenrod", "light", "goldenrod", "yellow",
             "light", "gray", "light", "green", "light", "grey", "light",
             "pink", "light", "salmon", "light", "sea", "green", "light",
             "sky", "blue", "light", "slate", "blue", "light", "slate", "gray",
             "light", "slate", "grey", "light", "steel", "blue", "light",
             "yellow", "LightBlue", "LightBlue1", "LightBlue2", "LightBlue3",
             "LightBlue4", "LightCoral", "LightCyan", "LightCyan1", "LightCyan2",
             "LightCyan3", "LightCyan4", "LightGoldenrod", "LightGoldenrod1",
             "LightGoldenrod2", "LightGoldenrod3", "LightGoldenrod4", "LightGoldenrodYellow",
             "LightGray", "LightGreen", "LightGrey", "LightPink", "LightPink1",
             "LightPink2", "LightPink3", "LightPink4", "LightSalmon", "LightSalmon1",
             "LightSalmon2", "LightSalmon3", "LightSalmon4", "LightSeaGreen",
             "LightSkyBlue", "LightSkyBlue1", "LightSkyBlue2", "LightSkyBlue3",
             "LightSkyBlue4", "LightSlateBlue", "LightSlateGray", "LightSlateGrey",
             "LightSteelBlue", "LightSteelBlue1", "LightSteelBlue2", "LightSteelBlue3",
             "LightSteelBlue4", "LightYellow", "LightYellow1", "LightYellow2",
             "LightYellow3", "LightYellow4", "lime", "green", "LimeGreen",
             "linen", "magenta", "magenta1", "magenta2", "magenta3", "magenta4",
             "maroon", "maroon1", "maroon2", "maroon3", "maroon4", "medium",
             "aquamarine", "medium", "blue", "medium", "orchid", "medium",
             "purple", "medium", "sea", "green", "medium", "slate", "blue",
             "medium", "spring", "green", "medium", "turquoise", "medium",
             "violet", "red", "MediumAquamarine", "MediumBlue", "MediumOrchid",
             "MediumOrchid1", "MediumOrchid2", "MediumOrchid3", "MediumOrchid4",
             "MediumPurple", "MediumPurple1", "MediumPurple2", "MediumPurple3",
             "MediumPurple4", "MediumSeaGreen", "MediumSlateBlue", "MediumSpringGreen",
             "MediumTurquoise", "MediumVioletRed", "midnight", "blue", "MidnightBlue",
             "mint", "cream", "MintCream", "misty", "rose", "MistyRose", "MistyRose1",
             "MistyRose2", "MistyRose3", "MistyRose4", "moccasin", "navajo",
             "white", "NavajoWhite", "NavajoWhite1", "NavajoWhite2", "NavajoWhite3",
             "NavajoWhite4", "navy", "navy", "blue", "NavyBlue", "old", "lace",
             "OldLace", "olive", "drab", "OliveDrab", "OliveDrab1", "OliveDrab2",
             "OliveDrab3", "OliveDrab4", "orange", "orange", "red", "orange1",
             "orange2", "orange3", "orange4", "OrangeRed", "OrangeRed1", "OrangeRed2",
             "OrangeRed3", "OrangeRed4", "orchid", "orchid1", "orchid2", "orchid3",
             "orchid4", "pale", "goldenrod", "pale", "green", "pale", "turquoise",
             "pale", "violet", "red", "PaleGoldenrod", "PaleGreen", "PaleGreen1",
             "PaleGreen2", "PaleGreen3", "PaleGreen4", "PaleTurquoise", "PaleTurquoise1",
             "PaleTurquoise2", "PaleTurquoise3", "PaleTurquoise4", "PaleVioletRed",
             "PaleVioletRed1", "PaleVioletRed2", "PaleVioletRed3", "PaleVioletRed4",
             "papaya", "whip", "PapayaWhip", "peach", "puff", "PeachPuff",
             "PeachPuff1", "PeachPuff2", "PeachPuff3", "PeachPuff4", "peru",
             "pink", "pink1", "pink2", "pink3", "pink4", "plum", "plum1",
             "plum2", "plum3", "plum4", "powder", "blue", "PowderBlue", "purple",
             "purple1", "purple2", "purple3", "purple4", "red", "red1", "red2",
             "red3", "red4", "rosy", "brown", "RosyBrown", "RosyBrown1", "RosyBrown2",
             "RosyBrown3", "RosyBrown4", "royal", "blue", "RoyalBlue", "RoyalBlue1",
             "RoyalBlue2", "RoyalBlue3", "RoyalBlue4", "saddle", "brown",
             "SaddleBrown", "salmon", "salmon1", "salmon2", "salmon3", "salmon4",
             "sandy", "brown", "SandyBrown", "sea", "green", "SeaGreen", "SeaGreen1",
             "SeaGreen2", "SeaGreen3", "SeaGreen4", "seashell", "seashell1",
             "seashell2", "seashell3", "seashell4", "sienna", "sienna1", "sienna2",
             "sienna3", "sienna4", "sky", "blue", "SkyBlue", "SkyBlue1", "SkyBlue2",
             "SkyBlue3", "SkyBlue4", "slate", "blue", "slate", "gray", "slate",
             "grey", "SlateBlue", "SlateBlue1", "SlateBlue2", "SlateBlue3",
             "SlateBlue4", "SlateGray", "SlateGray1", "SlateGray2", "SlateGray3",
             "SlateGray4", "SlateGrey", "snow", "snow1", "snow2", "snow3",
             "snow4", "spring", "green", "SpringGreen", "SpringGreen1", "SpringGreen2",
             "SpringGreen3", "SpringGreen4", "steel", "blue", "SteelBlue",
             "SteelBlue1", "SteelBlue2", "SteelBlue3", "SteelBlue4", "tan",
             "tan1", "tan2", "tan3", "tan4", "thistle", "thistle1", "thistle2",
             "thistle3", "thistle4", "tomato", "tomato1", "tomato2", "tomato3",
             "tomato4", "turquoise", "turquoise1", "turquoise2", "turquoise3",
             "turquoise4", "violet", "violet", "red", "VioletRed", "VioletRed1",
             "VioletRed2", "VioletRed3", "VioletRed4", "wheat", "wheat1",
             "wheat2", "wheat3", "wheat4", "white", "white", "smoke", "WhiteSmoke",
             "yellow", "yellow", "green", "yellow1", "yellow2", "yellow3",
             "yellow4", "YellowGreen"))
}
