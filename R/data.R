#' Set of sample building polygons
#'
#' Object of the classes 'sf' and 'data.frame' representing a set of sample
#'     building polygons that corresponds with the sample elevation and snow
#'     depth rasters. Contains basic information for three residential buildings
#'     in Fairbanks, Alaska. More details are shown below.
#'
#' @format An 'sf' data frame with 3 rows and 4 variables:
#' \describe{
#'   \item{building_id}{Building ID number}
#'   \item{area}{Area of the building polygon in square meters}
#'   \item{vertices}{Number of vertices in the building polygon}
#'   \item{geometry}{The polygon's spatial geometry}
#' }
"sample_buildings"
