#' Get UK Shape Files
#'
#' \code{get_sf} fetches shape files from ONS Open Geography Portal.
#'
#' This function...
#'
#' @param geog Which geographical
#' @param year Which year
#' @param nations Which UK nations
#' @param type Which boundary type
#' @param crs Any Coordinate Reference System ESPG, recommend 4326 (World Geodetic System) or potentially 27700 (UK OS British National Grid)
#'
#'
#' @return Output is...
#' @examples
#' get_sf("NAT")
#'
#' \dontrun{
#' get_sf("UTLA", nations = c("E","W"))
#' }
#' @import dplyr
#' @import sf
#' @import stringr
#' @importFrom curl has_internet
#' @importFrom utils askYesNo
#' @export

##########################################

get_sf <- function(geog,
                   year = 2019,
                   nations = c("E","S","W","N"),
                   type = "BGC",
                   crs = 4326
                   ){

  # Stop if not internet connection
  if (curl::has_internet() == FALSE) stop("Are you connected to the internet?")

  # Define Coordinate Reference System
  if (crs != 4326 & crs != 27700){
    answer <- askYesNo("Are you sure you want to use a non-recomended CRS?", default = TRUE, prompts = getOption("askYesNo", gettext(c("Yes", "No"))))
    if (answer == FALSE) {
      stop("You choose not to proceed")
    } else{
      warning("Using non-standard coordinate reference system (CRS)")
    }
  }

  crs <- crs

  # Define Year
  if (year < 2018 | year > 2019) stop("'year' must be either 2018 or 2019")
  year <- year

  # Define boundary clipping
  if (type %in% c("BGC", "BFC", "BFE", "BUC")){
    type <- type
  } else{
    stop("'type' must be one of BGC, BFC, BFE or BUC, see help(get_sf) for definitions")
  }

  if (geog == "UTLA") {
    bound <- "Counties_and_Unitary_Authorities"
    tag <- "UK"
  } else if (geog == "LAD"){
    bound <-"Local_Authority_Districts"
    tag <- "UK"
  } else if (geog == "GOR"){
    bound <- "Regions"
    tag <- "EN"
    warning("Regions only exist for England")
  } else if (geog == "NAT"){
    bound <- "Countries"
    tag <- "UK"
  } else{
    stop("Incorrect specification of argument 'geog', 'geog' accepts 'UTLA', 'LAD', 'GOR' or 'NAT'")
  }

  # Construct URL for API call
  url <- paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/",
                bound,
                "_December_",
                year,
                "_Boundaries_",
                tag,
                "_",
                type,
                "/MapServer/0/query?where=1%3D1&outFields=*&outSR=",
                crs,
                "&f=json")

  # Read in shapefile
  suppressWarnings({
    sf <- sf::st_read(url, quiet = TRUE)
  })

  # Renaming
  names(sf) <- c(names(sf)[1:1],
                 stringr::str_sub(names(sf)[2:3], start= -2),
                 names(sf)[4:length(names(sf))])

  # Apply country filtering
  sf <- sf %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::mutate(country = substr(.data$cd, 1, 1)) %>%
        dplyr::filter(.data$country %in% nations) %>%
        dplyr::mutate(country = dplyr::case_when(country == "E" ~ "England",
                                                 country == "S" ~ "Scotland",
                                                 country == "W" ~ "Wales",
                                                 country == "N" ~ "Northern Ireland"))

  if (geog == "UTLA" & year == 2019) {
    data("lea2019lookup", envir=environment())
    sf <- sf %>%
          dplyr::left_join(lea2019lookup, by = c("cd" = "UTLA19CD", "nm" = "UTLA19NM"))
  }

  # Ensure geometry is in the last column
  sf <- sf %>%
        dplyr::select(dplyr::everything(), .data$geometry)

  return(sf)

}
