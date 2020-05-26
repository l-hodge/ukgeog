#' Get UK Administrative Boundaries
#'
#' \code{read_admin} fetches Administrative Boundaries from the ONS Open Geography Portal.
#'
#' This function...
#'
#' @param geog Type of administrative boundaries
#' @param year Year (\code{2018} or \code{2019})
#' @param nations For Which UK nations (\code{c("E","W","S","N")})
#' @param type Boundary Clipping (\code{"BGC", "BFC", "BFE" or "BUC"})
#' @param crs Coordinate Reference System (ESPG). It is recommend to use 4326 (World Geodetic System) or potentially 27700 (UK OS British National Grid)
#'
#'
#' @return Output is...
#' @examples
#' read_admin("NAT")
#'
#' \dontrun{
#' read_admin("UTLA", nations = c("E","W"))
#' }
#' @import lifecycle
#' @import dplyr
#' @import sf
#' @import stringr
#' @importFrom curl has_internet
#' @importFrom utils askYesNo
#' @importFrom utils data
#' @export

##########################################

read_admin <- function(geog,
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
    sf <- sf %>%
      dplyr::left_join(ukgeog::lea2019lookup, by = c("cd" = "UTLA19CD", "nm" = "UTLA19NM"))
  }

  # Ensure geometry is in the last column
  sf <- sf %>%
    dplyr::select(dplyr::everything(), .data$geometry)

  return(sf)

}



#' Get England and Wales Census Boundaries
#'
#' \code{read_census} fetches Census Boundaries from the ONS Open Geography Portal.
#'
#' This function...England and Wales ONLY
#'
#' @param geog Type of census boundaries
#' @param year Year (\code{2001} or \code{2011})
#' @param nations For Which UK nations (\code{c("E","W")})
#' @param type Boundary Clipping
#' @param crs Coordinate Reference System (ESPG). It is recommend to use 4326 (World Geodetic System) or potentially 27700 (UK OS British National Grid)
#'
#'
#' @return Output is...
#' @examples
#' read_census("MSOA")
#'
#' \dontrun{
#' read_census("OA", nations = c("E"))
#' }
#' @import lifecycle
#' @import dplyr
#' @import sf
#' @import stringr
#' @importFrom curl has_internet
#' @importFrom utils askYesNo
#' @importFrom utils data
#' @export

##########################################

read_census <- function(geog,
                        year = 2011,
                        nations = c("E", "W"),
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
  if (year != 2001 & year != 2011) stop("'year' must be either 2001 or 2011")
  year <- year

  if (type == "BFC"){
    type <- 0
  } else if (type == "BFE"){
    type <- 1
  } else if (type == "BGC"){
    type <- 2
  } else{
    stop("'type' must be one of BGC, BFC or BFE, see help(get_census_sf) for definitions")
  }

  if (geog == "OA") {
    bound <- "Output_Area"
  } else if (geog == "LSOA"){
    bound <- "Lower_Super_Output_Areas"
  } else if (geog == "MSOA"){
    bound <- "Middle_Super_Output_Areas"
  } else{
    stop("Incorrect specification of argument 'geog', 'geog' accepts 'OA', 'LSOA' or 'MSOA'")
  }

  # Construct URL for API call
  url <- paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/",
                bound,
                "_December_",
                year,
                "_Boundaries",
                "/MapServer/",
                type,
                "/query?where=1%3D1&outFields=*&outSR=",
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
                                             country == "W" ~ "Wales"))

  # Ensure geometry is in the last column
  sf <- sf %>%
    dplyr::select(dplyr::everything(), .data$geometry)

  return(sf)

}


#' Get UK Electoral Boundaries
#'
#' \code{read_elec} fetches Electoral Boundaries from the ONS Open Geography Portal.
#'
#' This function...
#'
#' @param geog Type of electoral boundaries
#' @param year Year (\code{2018} or \code{2019})
#' @param nations For which UK nations (\code{c("E","W", "S", "N")})
#' @param type Boundary Clipping
#' @param crs Coordinate Reference System (ESPG). It is recommend to use 4326 (World Geodetic System) or potentially 27700 (UK OS British National Grid)
#'
#'
#' @return Output is...
#' @examples
#' read_elec("EU")
#'
#' \dontrun{
#' read_elec("WM")
#' }
#' @import lifecycle
#' @import dplyr
#' @import sf
#' @import stringr
#' @importFrom curl has_internet
#' @importFrom utils askYesNo
#' @importFrom utils data
#' @export

##########################################

read_elec <- function(geog,
                      year = 2018,
                      nations = c("E", "W", "S", "N"),
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
  if (year != 2019 & year != 2018) stop("'year' must be 2018 or 2019")
  year <- year

  # Define boundary clipping
  if (type %in% c("BGC", "BFC", "BFE", "BUC")){
    type <- type
  } else{
    stop("'type' must be one of BGC, BFC, BFE or BUC, see help(get_sf) for definitions")
  }

  if (geog == "WM") {
    bound <- "Westminster_Parliamentary_Constituencies"
    tag <- "UK"
  } else if (geog == "EU"){
    bound <- "European_Electoral_Regions"
    tag <- "UK"
  } else{
    stop("Incorrect specification of argument 'geog', 'geog' accepts 'WM' or 'EU'")
  }

  # Construct URL for API call
  url <- paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Electoral_Boundaries/",
                bound,
                "_December_",
                year,
                "_Boundaries_",
                tag,
                "_",
                type,
                "/MapServer/0",
                "/query?where=1%3D1",
                "&outFields=*",
                "&outSR=", crs,
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

  # Ensure geometry is in the last column
  sf <- sf %>%
    dplyr::select(dplyr::everything(), .data$geometry)

  return(sf)

}


#' Get UK Eurostat NUTS Boundaries
#'
#' \code{read_nuts} fetches Eurostat NUTS Boundaries from the ONS Open Geography Portal.
#'
#' This function...
#'
#' @param geog NUTS 1, 2 or 3
#' @param year Year (\code{2015} or \code{2018})
#' @param nations For Which UK nations (\code{c("E","W","S","N")})
#' @param type Boundary Clipping
#' @param crs Coordinate Reference System (ESPG). It is recommend to use 4326 (World Geodetic System) or potentially 27700 (UK OS British National Grid)
#'
#'
#' @return Output is...
#' @examples
#' read_nuts("NUTS1")
#'
#' \dontrun{
#' read_nuts("NUTS2", nations = c("E"))
#' }
#' @import lifecycle
#' @import dplyr
#' @import sf
#' @import stringr
#' @importFrom curl has_internet
#' @importFrom utils askYesNo
#' @importFrom utils data
#' @export

##########################################

read_nuts <- function(geog,
                      year = 2018,
                      nations = c("E", "W"),
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
  if (year != 2015 & year != 2018) stop("'year' must be either 2015 or 2018")
  year <- year

  if (type == "BFC"){
    type <- 0
  } else if (type == "BFE"){
    type <- 1
  } else if (type == "BGC"){
    type <- 2
  } else{
    stop("'type' must be one of BGC, BFC or BFE, see help(get_census_sf) for definitions")
  }

  if (geog == "NUTS1") {
    bound <- "NUTS_Level_1"
  } else if (geog == "NUTS2"){
    bound <- "NUTS_Level_2"
  } else if (geog == "NUTS3"){
    bound <- "NUTS_Level_3"
  } else{
    stop("Incorrect specification of argument 'geog', 'geog' accepts 'NUTS1', 'NUTS2' or 'NUTS3'")
  }

  # Construct URL for API call
  url <- paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Eurostat_Boundaries/",
                bound,
                "_January_",
                year,
                "_Boundaries",
                "/MapServer/",
                type,
                "/query?where=1%3D1",
                "&outFields=*",
                "&outSR=", crs,
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

  # Ensure geometry is in the last column
  sf <- sf %>%
    dplyr::select(dplyr::everything(), .data$geometry)

  return(sf)

}
