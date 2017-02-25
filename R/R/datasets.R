#' @title Data to re-create Hans Rosling's fameous "Us and Them" animation
#'   
#' @description This data was sourced from \url{https://www.gapminder.org/} and
#'   contains Population, Life Expectancy, Fertility, Income, and
#'   Geographic.Region information between 1962 and 2013 for 198 countries.
#'   
#' @format A data frame with 9855 rows and 8 variables
#' @source \url{http://www.gapminder.org/}
"UsAndThem"


#' @title Fatty Acid Composition of Italian Olive Oils
#'   
#' @description This data set records the percentage composition of 8 fatty 
#'   acids (palmitic, palmitoleic, stearic, oleic, linoleic, linolenic, 
#'   arachidic, eicosenoic) found in the lipid fraction of 572 Italian olive 
#'   oils. The oils are samples taken from three Italian regions varying number 
#'   of areas within each region.  The regions and their areas are recorded as 
#'   shown in the following table:
#'   
#'   \tabular{ll}{ \bold{Region} \tab \bold{Area}\cr North \tab North-Apulia,
#'   South-Apulia, Calabria, Sicily \cr South \tab East-Liguria, West-Liguria,
#'   Umbria \cr Sardinia \tab Coastal-Sardinia, Inland-Sardinia }
#'   
#' @format A data frame containing 572 cases and 10 variates.
#'   
#' @references Forina, M., Armanino, C., Lanteri, S., and Tiscornia, E. (1983)
#'   "Classification of Olive Oils from their Fatty Acid Composition", in Food
#'   Research and Data Analysis (Martens, H., Russwurm, H., eds.), p. 189,
#'   Applied Science Publ., Barking.
#'   
"olive"

#' Fatty Acid Composition of Italian Olive Oils
#' 
#' @description This is the \code{\link{olive}} data set minus the \code{Region}
#'   and \code{Area} variables.
#'   
#' @format A data frame containing 572 cases and 8 variates.
#' 
#' @seealso \code{\link{olive}}
"oliveAcids"

#' @title Canadian Visible Minority Data 2006
#'   
#' @description This data contains information about the visible minority 
#'   populations distributed across major census metropolitan areas of Canada. 
#'   These data are from the 2006 Canadian census, publicly available from 
#'   Statistics Canada Statistics Canada (2006). For each of the 33 Canadian 
#'   census metropolitan areas, we have the total population and the population 
#'   Implemen- tation of all its "visible minorities". These self-declared 
#'   visible minorities are: "Arab", "Black", "Chinese", "Filipino", "Japanese",
#'   "Korean", "Latin American", "Multiple visible minority", "South Asian", 
#'   "Southeast Asian", "Visible minority (not included elsewhere)", and "West 
#'   Asian". For each metropolitan area, we also obtained the approximate 
#'   latitude and longitude coordinates using the Google Maps Geocoding API and 
#'   added them to the data set.
#' 
#' @format A data frame with 33 rows and 18 variates
#' 
#' @source \url{http://www.statcan.gc.ca}
"minority"

