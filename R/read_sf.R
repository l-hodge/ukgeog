#' Get UK boundary shapefiles
#'
#' \code{read_sf} downloads boundaries from the ONS Open Geography Portal.
#'
#' @param geog Type of boundaries
#'
#' Administrative Boundaries available:
#'
#' - Countries/Nations (\code{"NAT"})
#'
#' - Regions (England ONLY) (\code{"GOR"})
#'
#' - Upper Tier Local Authorities (\code{"UTLA"})
#'
#' - Lower Tier Local Authorities (\code{"LAD"})
#'
#' Census Boundaries available:
#'
#' - Output Area (\code{"OA"})
#'
#' - Lower Super Output Area (\code{"LSOA"})
#'
#' - Middle Super Output Area (\code{"MSOA"})
#'
#' Electoral Boundaries available:
#'
#' - Westminster Parliamentary Constituencies (\code{"WM"})
#'
#' - European Union Parliamentary Constituencies (\code{"EU"})
#'
#' - Welsh Assembly Constituencies (\code{"WAC"})
#'
#' - Welsh Assembly Regions (\code{"WAR"})
#'
#' Eurostat NUTS Boundaries available:
#'
#'    - NUTS Level 1 (\code{NUTS1})
#'
#'    - NUTS Level 2 (\code{NUTS2})
#'
#'    - NUTS Level 3 (\code{NUTS3})
#'
#' @param year Year (use \code{available_sf()} to see available years for each set of boundaries)
#' @param nations For which UK nations (\code{c("E","W","S","N")})
#' @param type Boundary clipping
#'
#' - \code{"BFE"}: Full Resolution Extent of the Realm (Low Tide)
#'
#' - \code{"BFC"}: Full Resolution Clipped (High Tide)
#'
#' - \code{"BGC"}: Generalised (20m) Clipped (High Tide)
#'
#' - \code{"BUC"}: Ultra Generalised (500m) Clipped (High Tide)
#'
#' @param crs Coordinate Reference System (ESPG).
#'
#' It is recommend to use \code{4326} (World Geodetic System) or potentially \code{27700} (UK OS British National Grid)
#'
#' @return Output is a simple feature \code{sf} object containing the relevant boundaries.
#'
#' @examples
#' read_sf("NAT")
#' read_sf("MSOA", year = 2011)
#' read_sf("EU", year = 2018, type = "BFC")
#' read_sf("NUTS1", year = 2018, nations = "E")
#'
#' \dontrun{
#' read_sf("UTLA", nations = c("E","W"))
#' read_elec("WM")
#' }
#'
#' @import lifecycle
#' @import dplyr
#' @importFrom tidyselect any_of
#' @importFrom sf st_read st_write
#' @importFrom stringr str_sub
#' @importFrom curl has_internet
#' @importFrom utils askYesNo
#' @importFrom utils data
#'
#' @export

##########################################

read_sf <- function(geog,
                    year = 2021,
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

  # Read in metadata
  n <- ukgeog::metadata[, "geog_short"] == geog

  boundary_type = ukgeog::metadata[n, "boundary_type"]
  geog = ukgeog::metadata[n, "geog"]
  month = ukgeog::metadata[n, "month"]
  tag = ukgeog::metadata[n, "tag"]
  num = 0

  # Check geography is available
  if (sum(n) == 0) stop("Unknown geography, please use one of: '", paste(ukgeog::metadata[, "geog_short"], collapse = "', '"))

  # Check the requested year is available
  yrs <- check_years(boundary_type = boundary_type,
                     year = "",
                     month = month,
                     geog = geog,
                     type = ukgeog::metadata[n, "type"],
                     tag = tag)$year

  if (!(as.character(year) %in% yrs))
    stop(paste("The requested shapefile in only available for", paste(yrs, collapse = ", "), "- check 'available_sf()' to see available shapefiles"))

  # Check boundary clipping
  if (!(type %in% c("BGC", "BFC", "BFE"))) stop("'type' must be one of BGC, BFC or BFE, see help(read_sf) for definitions")

  # Create shapefiles dir if doesn't already exist
  if (!dir.exists("shapefiles")) dir.create("shapefiles")

  # Construct file name
  savename <- paste0("shapefiles/", paste(geog, paste(nations, collapse = ""), year, type, crs, sep = "_"), ".shp")

  # Check file doesn't already exist
  if (file.exists(savename)){
    sf <- st_read(savename, stringsAsFactors = FALSE, quiet = TRUE)
  } else{
    message("Downloading from ONS...")

    if (boundary_type == "Census_Boundaries" | boundary_type == "Eurostat_Boundaries"){
      if (type == "BFC"){type <- 0}
      if (type == "BFE"){type <- 1}
      if (type == "BGC"){type <- 2}
      num = type
      type = ""
    }

    url <- select_url(boundary_type = boundary_type, year = year, month = month, geog = geog, type = type, crs = crs, tag = tag, num = num)

    # Read in shapefile
    suppressWarnings({
      for(i in 1:length(url)){
        if (exists("sf") != TRUE){
          try(
            sf <- st_read(url[i], quiet = TRUE) %>%
              select(-tidyselect::any_of(c("objectd"))),
            silent = TRUE
          )
        }
      }
      if (exists("sf") != TRUE){
        stop("The shapefile you have requested doesn't seem to exist, sorry!")
      }
    })

    # Renaming
    names(sf) <- c(str_sub(names(sf)[1:2], start= -2),
                   names(sf)[3:length(names(sf))])

    # Apply country filtering
    if (boundary_type == "Eurostat_Boundaries") {
      sf <- sf %>%
        mutate_if(is.factor, as.character) %>%
        mutate(country = substr(.data$cd, 3, 3)) %>%
        mutate(country = ifelse(.data$country == "M", "S", # Scotland
                                ifelse(.data$country == "L", "W", # Wales
                                       ifelse(.data$country == "N", "N", # NI
                                              "E")))) %>% # Rest are England
        filter(.data$country %in% nations)
    } else {
      sf <- sf %>%
        mutate_if(is.factor, as.character) %>%
        mutate(country = substr(.data$cd, 1, 1)) %>%
        filter(.data$country %in% nations)
    }

    # Full names of countries
    sf <- sf %>%
      mutate(country = case_when(country == "E" ~ "England",
                                 country == "S" ~ "Scotland",
                                 country == "W" ~ "Wales",
                                 country == "N" ~ "Northern Ireland"))

    # Ensure geometry is in the last column
    sf <- sf %>%
          select(everything(), .data$geometry)

    # Save sf
    suppressWarnings({
      sf::st_write(sf, savename, quiet = TRUE)
    })
  }

  return(sf)

}
