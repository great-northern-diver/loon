#' @title Data to re-create Hans Rosling's famous "Us and Them" animation
#'
#' @description This data was sourced from \url{https://www.gapminder.org/} and
#'   contains Population, Life Expectancy, Fertility, Income, and
#'   Geographic.Region information between 1962 and 2013 for 198 countries.
#'
#' @format A data frame with 9855 rows and 8 variables
#' \describe{
#'  \item{Country}{country name}
#'  \item{Year}{year of recorded measurements}
#'  \item{Population}{country's population}
#'  \item{LifeExpectancy}{average life expectancy in years at birth}
#'  \item{Fertility}{in number of babies per woman}
#'  \item{Income}{Gross domestic product per person adjusted for inflation and purchasing power
#'  (in international dollars)}
#'  \item{Geographic.Region}{one of six large global regions}
#'  \item{Geographic.Region.ID}{two letter identification of country}
#' }
#' @source \url{https://www.gapminder.org/}
"UsAndThem"


#' @title Fatty Acid Composition of Italian Olive Oils
#'
#' @description This data set records the percentage composition of 8 fatty
#'   acids found in the lipid fraction of 572 Italian olive
#'   oils. The oils are samples taken from three Italian regions varying number
#'   of areas within each region.  The regions and their areas are recorded as
#'   shown in the following table:
#'
#'   \tabular{ll}{ \bold{Region} \tab \bold{Area}\cr North \tab North-Apulia,
#'   South-Apulia, Calabria, Sicily \cr South \tab East-Liguria, West-Liguria,
#'   Umbria \cr Sardinia \tab Coastal-Sardinia, Inland-Sardinia }
#'
#' @format A data frame containing 572 cases and 10 variates.
#' \describe{
#'  \item{Region}{Italian olive oil general growing region: North, South, or Sardinia}
#'  \item{Area}{These are "Administrative Regions" of Italy (e.g. Sicily, or Umbria), or parts of such a region like
#'  "Coastal-Sardinia" and "Inland-Sardinia" or "North-Apulia" and "South-Apulia". Administrative regions are larger
#'  than, and contain, Italian provinces.}
#'  \item{palmitic}{Percentage (in hundredths of a percent) of Palmitic acid, or hexadecanoic acid in the olive oil.
#'  It is the most common saturated fatty acid found in animals, plants and micro-organisms.}
#'  \item{palmitoleic}{Percentage (in hundredths of a percent) of Palmitoleic acid, an omega-7 monounsaturated fatty acid.}
#'  \item{stearic}{Percentage (in hundredths of a percent) of Stearic acid, a saturated fatty acid. It is a waxy solid and
#'  its name comes from the Greek word for tallow. Like palmitic acid, it is
#'  one of the most common saturated fatty acids found in nature.}
#'  \item{oleic}{Percentage (in hundredths of a percent) of Oleic acid, the most common fatty acid occurring in nature found in
#'  various animal and vegetable fats and oils.}
#'  \item{linoleic}{Percentage (in hundredths of a percent) of Linoleic acid, a polyunsaturated omega-6 fatty acid.
#'  It is one of two essential fatty acids for humans.}
#'  \item{linolenic}{Percentage (in hundredths of a percent) of Linolenic acid, a type of fatty acid. It can refer to one
#'  of two types of fatty acids or a mixture of both.  One is an omega-3 essential fatty acid; the other an omega-6.}
#'  \item{arachidic}{Percentage (in hundredths of a percent) of Arachidic acid, also known as eicosanoic acid, a saturated fatty acid
#'  that is used for the production of detergents, photographic materials and lubricants.}
#'  \item{eicosenoic}{Percentage (in hundredths of a percent) of Eicosenoic acid, which may refer to one of three closely related
#'  fatty acids: gadoleic acid (omega-11), gondoic acid (omega-9), or paullinic acid (omega-7). }
#' }
#' Note that the percentages (in hundredths of a percent) should sum to approximately 10,000 for each oil (row).
#'
#' @seealso \code{\link{oliveLocations}}
#' @references
#'   Forina, M., Armanino, C., Lanteri, S., and Tiscornia, E. (1983)
#'   "Classification of Olive Oils from their Fatty Acid Composition", in Food
#'   Research and Data Analysis (Martens, H., Russwurm, H., eds.), p. 189,
#'   Applied Science Publ., Barking.
#'
#'
#'
"olive"

#' @title Just the Fatty Acid Composition of Italian Olive Oils
#'
#' @description This is the \code{\link{olive}} data set minus the \code{Region}
#'   and \code{Area} variables.
#'
#' @format A data frame containing 572 cases and 8 variates.
#'
#' @seealso \code{\link{olive}}
"oliveAcids"

#' @title Geographic location of each Italian olive growing area
#' named in the \code{\link{olive}} data.
#'
#' @description A longitude and latitude for each \code{Area}
#'  named in the \code{\link{olive}} data set.
#'
#' @format A data frame containing 9 cases and 3 variates.
#' \describe{
#'  \item{Area}{name of the Italian growing area of the olive oil.}
#'  \item{lat, long}{latitude and longitude in degrees of the approximate
#'  centre of the named growing area}
#' }
#'
#' @seealso \code{\link{olive}}
#'
#' @source \url{https://www.latlong.net}
"oliveLocations"

#' @title Canadian Visible Minority Data 2006
#'
#' @description Population census count of various named
#'   visible minority groups in each of 33 major census metropolitan
#'   areas of Canada in 2006.
#'
#'   These data are from the 2006 Canadian census, publicly available from
#'   Statistics Canada.
#'
#'
#' @format A data frame with 33 rows and 18 variates
#' \describe{
#'  \item{Arab}{Number identifying as `Arab`.}
#'  \item{Black}{Number identifying as `Black`.}
#'  \item{Chinese}{Number identifying as `Chinese`.}
#'  \item{Filipino}{Number identifying as `Filipino`.}
#'  \item{Japanese}{Number identifying as `Japanese`.}
#'  \item{Korean}{Number identifying as `Korean`.}
#'  \item{Latin.American}{Number identifying as `Latin American`.}
#'  \item{Multiple.visible.minority}{Number identifying as being a member
#'  of more than one visible minority.}
#'  \item{South.Asian}{Number identifying as `South Asian`.}
#'  \item{Southeast.Asian}{Number identifying as `Southeast Asian`.}
#'  \item{Total.population}{Total population of the metropolitan census area.}
#'  \item{Visible.minority.not.included.elsewhere}{Number identifying as a
#'  member of a visible minority that was not included elsewhere.}
#'  \item{Visible.minority.population}{Total number identifying as a
#'  member of some visible minority.}
#'  \item{West.Asian}{Number identifying as `West Asian`.}
#'  \item{lat, long}{Latitude and longitude (in degrees) of the metropolitan census area.}
#'  \item{googleLat, googleLong}{Latitude and longitude in degrees determined
#'  using the Google Maps Geocoding API.}
#' }
#' \code{rownames(minority)} are the names of the metropolitan areas or cities.
#'
#'
#' @source \url{https://www.statcan.gc.ca/}
"minority"


