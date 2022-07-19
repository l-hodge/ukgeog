#' Get UK Administrative Boundaries
#'
#' \code{read_admin} downloads Administrative Boundaries from the ONS Open Geography Portal.
#'
#' Boundaries included:
#'
#' - Countries/Nations (\code{"NAT"})
#'
#' - Regions (England ONLY) (\code{"GOR"})
#'
#' - Upper Tier Local Authorities (\code{"UTLA"})
#'
#' - Lower Tier Local Authorities (\code{"LAD"})
#'
#' @param geog Type of administrative boundaries (\code{"NAT", "GOR", "UTLA" or "LAD"})
#' @param year Year (\code{2018} or \code{2019})
#' @param nations For Which UK nations (\code{c("E","W","S","N")})
#' @param type Boundary Clipping
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
#' @return Output is a simple feature object containing the relevant boundaries.
#'
#' @examples
#' read_admin("NAT")
#'
#' \dontrun{
#' read_admin("UTLA", nations = c("E","W"))
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

read_admin <- function(geog,
                       year = 2021,
                       nations = c("E","S","W","N"),
                       type = "BGC",
                       crs = 4326
){

  # Stop if not internet connection
  if (has_internet() == FALSE) stop("Are you connected to the internet?")

  # Define Coordinate Reference System
  if (crs != 4326 & crs != 27700){
    answer <- askYesNo("Are you sure you want to use a non-recomended CRS?", default = TRUE, prompts = getOption("askYesNo", gettext(c("Yes", "No"))))
    if (answer == FALSE) {
      stop("You choose not to proceed")
    } else{
      warning("Using non-standard coordinate reference system (CRS)")
    }
  }

  # Define Year
  if (year < 2018 | year > 2021) stop("'year' must be either 2018, 2019, 2020 or 2021")

  # Define boundary clipping
  if (!(type %in% c("BGC", "BFC", "BFE", "BUC"))) stop("'type' must be one of BGC, BFC, BFE or BUC, see help(read_elec) for definitions")

  # Create shapefiles dir if doesn't already exist
  if (!dir.exists("shapefiles")) dir.create("shapefiles")

  # Construct file name
  savename <- paste0("shapefiles/", paste(geog, paste(nations, collapse = ""), year, type, crs, sep = "_"), ".shp")

  # Check file doesn't already exist
  if (file.exists(savename)){
    sf <- st_read(savename, stringsAsFactors = FALSE, quiet = TRUE)
  } else{
    message("Downloading from ONS...")

    boundary_type = "Administrative_Boundaries"

    if (geog == "UTLA") {
      url <- select_url(boundary_type = boundary_type, year = year, month = "December", geog = "Counties_and_Unitary_Authorities", type = type, crs = crs, tag = "UK", num = 0)
    } else if (geog == "LAD"){
      url <- select_url(boundary_type = boundary_type, year = year, month = "December", geog = "District", type = type, crs = crs, tag = "UK", num = 0)
    } else if (geog == "GOR"){
      url <- select_url(boundary_type = boundary_type, year = year, month = "December", geog = "Regions", type = type, crs = crs, tag = "EN", num = 0)
      message("Note: Regions only exist for England")
    } else if (geog == "NAT"){
      url <- select_url(boundary_type = boundary_type, year = year, month = "December", geog = "Countries", type = type, crs = crs, tag = "UK", num = 0)
    } else{
      stop("Incorrect specification of argument 'geog', 'geog' accepts 'UTLA', 'LAD', 'GOR' or 'NAT'")
    }

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
        stop("The shapefile you have selected doesn't seem to exist, sorry!")
      }
    })

    # Renaming
    names(sf) <- c(str_sub(names(sf)[1:2], start= -2),
                   names(sf)[3:length(names(sf))])

    # Apply country filtering
    sf <- sf %>%
          mutate_if(is.factor, as.character) %>%
          mutate(country = substr(.data$cd, 1, 1)) %>%
          filter(.data$country %in% nations) %>%
          mutate(country = case_when(country == "E" ~ "England",
                                     country == "S" ~ "Scotland",
                                     country == "W" ~ "Wales",
                                     country == "N" ~ "Northern Ireland"))

    # if (geog == "UTLA" & year == 2019) {
    #   sf <- sf %>%
    #         left_join(ukgeog::lea2019lookup, by = c("cd" = "UTLA19CD", "nm" = "UTLA19NM"))
    # }

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

#' Get England and Wales Census Boundaries
#'
#' \code{read_census} downloads Census Boundaries from the ONS Open Geography Portal.
#'
#' Note: This function is _only_ relevant to England and Wales.
#'
#' Boundaries included:
#'
#' - Output Area (\code{"OA"})
#'
#' - Lower Super Output Area (\code{"LSOA"})
#'
#' - Middle Super Output Area (\code{"MSOA"})
#'
#' @param geog Type of census boundaries (\code{"OA", "LSOA", "MSOA"})
#' @param year Year (\code{2001} or \code{2011})
#' @param nations For Which UK nations (\code{c("E","W")})
#' @param type Boundary Clipping
#'
#' - \code{"BFE"}: Full Resolution Extent of the Realm (Low Tide)
#'
#' - \code{"BFC"}: Full Resolution Clipped (High Tide)
#'
#' - \code{"BGC"}: Generalised (20m) Clipped (High Tide)
#'
#' @param crs Coordinate Reference System (ESPG).
#'
#' It is recommend to use \code{4326} (World Geodetic System) or potentially \code{27700} (UK OS British National Grid)
#'
#' @return Output is a simple feature object containing the relevant boundaries.
#'
#' @examples
#' read_census("MSOA")
#'
#' \dontrun{
#' read_census("OA", nations = c("E"))
#' }
#'
#' @import lifecycle
#' @import dplyr
#' @importFrom sf st_read
#' @importFrom stringr str_sub
#' @importFrom curl has_internet
#' @importFrom utils askYesNo
#'
#' @export

##########################################

read_census <- function(geog,
                        year = 2011,
                        nations = c("E", "W"),
                        type = "BGC",
                        crs = 4326
){

  # Stop if not internet connection
  if (has_internet() == FALSE) stop("Are you connected to the internet?")

  # Define Coordinate Reference System
  if (crs != 4326 & crs != 27700){
    answer <- askYesNo("Are you sure you want to use a non-recomended CRS?", default = TRUE, prompts = getOption("askYesNo", gettext(c("Yes", "No"))))
    if (answer == FALSE) {
      stop("You choose not to proceed")
    } else{
      warning("Using non-standard coordinate reference system (CRS)")
    }
  }

  # Define Year
  if (year != 2001 & year != 2011) stop("'year' must be either 2001 or 2011")

  if (type == "BFC"){
    type <- 0
  } else if (type == "BFE"){
    type <- 1
  } else if (type == "BGC"){
    type <- 2
  } else{
    stop("'type' must be one of BGC, BFC or BFE, see help(get_census_sf) for definitions")
  }

  # Create shapefiles dir if doesn't already exist
  if (!dir.exists("shapefiles")) dir.create("shapefiles")

  # Construct file name
  savename <- paste0("shapefiles/", paste(geog, paste(nations, collapse = ""), year, type, crs, sep = "_"), ".shp")

  # Check file doesn't already exist
  if (file.exists(savename)){
    sf <- st_read(savename, stringsAsFactors = FALSE, quiet = TRUE)
  } else{
    message("Downloading from ONS...")

    boundary_type = "Census_Boundaries"

    if (geog == "OA") {
      url <- select_url(boundary_type = boundary_type, year = year, month = "December", geog = "/Output_Area", type = "", crs = crs, tag = "", num = type)
    } else if (geog == "LSOA"){
      url <- select_url(boundary_type = boundary_type, year = year, month = "December", geog = "/Lower_Super_Output_Areas", type = "", crs = crs, tag = "", num = type)
    } else if (geog == "MSOA"){
      url <- select_url(boundary_type = boundary_type, year = year, month = "December", geog = "/Middle_Super_Output_Areas", type = "", crs = crs, tag = "", num = type)
    } else{
      stop("Incorrect specification of argument 'geog', 'geog' accepts 'OA', 'LSOA' or 'MSOA'")
    }

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
        stop("The shapefile you have selected doesn't seem to exist, sorry!")
      }
    })

    # Renaming
    names(sf) <- c(str_sub(names(sf)[1:2], start= -2),
                   names(sf)[3:length(names(sf))])

    # Apply country filtering
    sf <- sf %>%
          mutate_if(is.factor, as.character) %>%
          mutate(country = substr(.data$cd, 1, 1)) %>%
          filter(.data$country %in% nations) %>%
          mutate(country = case_when(country == "E" ~ "England",
                                     country == "W" ~ "Wales"))

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

#' Get UK Electoral Boundaries
#'
#' \code{read_elec} downloads Electoral Boundaries from the ONS Open Geography Portal.
#'
#' Boundaries included:
#'
#' - Westminster Parliamentary Constituencies (\code{"WM"})
#'
#' - European Union Parliamentary Constituencies (\code{"EU"})
#'
#' - Welsh Assembly Constituencies (\code{"WAC"})
#'
#' - Welsh Assembly Regions (\code{"WAR"})
#'
#' @param geog Type of electoral boundaries (\code{"WM", "EU", "WPC" or "WPR"})
#' @param year Year (\code{2018} ONLY)
#' @param nations For which UK nations (\code{c("E", "W", "S", "N")})
#' @param type Boundary Clipping
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
#' @return Output is a simple feature object containing the relevant boundaries.
#'
#' @examples
#' read_elec("EU")
#'
#' \dontrun{
#' read_elec("WM")
#' }
#'
#' @import lifecycle
#' @import dplyr
#' @importFrom sf st_read
#' @importFrom stringr str_sub
#' @importFrom curl has_internet
#' @importFrom utils askYesNo
#'
#' @export

##########################################

read_elec <- function(geog,
                      year = 2018,
                      nations = c("E", "W", "S", "N"),
                      type = "BGC",
                      crs = 4326
){

  # Stop if not internet connection
  if (has_internet() == FALSE) stop("Are you connected to the internet?")

  # Define Coordinate Reference System
  if (crs != 4326 & crs != 27700){
    answer <- askYesNo("Are you sure you want to use a non-recomended CRS?", default = TRUE, prompts = getOption("askYesNo", gettext(c("Yes", "No"))))
    if (answer == FALSE) {
      stop("You choose not to proceed")
    } else{
      warning("Using non-standard coordinate reference system (CRS)")
    }
  }

  if (geog == "WM" & (year < 2018 | year > 2021)) stop("'year' must be 2018, 2019, 2020 or 2021")
  if (geog != "WM" & (year != 2018)) stop("'year' must be 2018")

  # Define boundary clipping
  if (!(type %in% c("BGC", "BFC", "BFE", "BUC"))) stop("'type' must be one of BGC, BFC, BFE or BUC, see help(read_elec) for definitions")

  # Create shapefiles dir if doesn't already exist
  if (!dir.exists("shapefiles")) dir.create("shapefiles")

  # Construct file name
  savename <- paste0("shapefiles/", paste(geog, paste(nations, collapse = ""), year, type, crs, sep = "_"), ".shp")

  # Check file doesn't already exist
  if (file.exists(savename)){
    sf <- st_read(savename, stringsAsFactors = FALSE, quiet = TRUE)
  } else{
    message("Downloading from ONS...")

    boundary_type <- "Electoral_Boundaries"

    if (geog == "WM") {
      url <- select_url(boundary_type = boundary_type, year = year, month = "D", geog = "Westminster_Parliamentary_Constituencies", type = type, crs = crs, tag = "UK", num = 0)
    } else if (geog == "EU"){
      url <- select_url(boundary_type = boundary_type, year = year, month = "December", geog = "European_Electoral_Regions", type = type, crs = crs, tag = "UK", num = 0)
    } else if (geog == "WAC"){
      url <- select_url(boundary_type = boundary_type, year = year, month = "December", geog = "National_Assembly_for_Wales_Constituencies", type = type, crs = crs, tag = "WA", num = 0)
    } else if (geog == "WAR"){
      url <- select_url(boundary_type = boundary_type, year = year, month = "December", geog = "National_Assembly_for_Wales_Electoral_Regions", type = type, crs = crs, tag = "WA", num = 0)
    } else{
      stop("Incorrect specification of argument 'geog', 'geog' only accepts 'WM', 'EU', 'WAC' or 'WAR'")
    }

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
        stop("The shapefile you have selected doesn't seem to exist, sorry!")
      }
    })

    # Renaming
    names(sf) <- c(str_sub(names(sf)[1:2], start= -2),
                   names(sf)[3:length(names(sf))])

    # Apply country filtering
    sf <- sf %>%
      mutate_if(is.factor, as.character) %>%
      mutate(country = substr(.data$cd, 1, 1)) %>%
      filter(.data$country %in% nations) %>%
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

#' Get UK Eurostat NUTS Boundaries
#'
#' \code{read_nuts} downloads Eurostat NUTS Boundaries from the ONS Open Geography Portal.
#'
#' Boundaries include:
#'
#' - NUTS Level 1 (\code{NUTS1})
#'
#' - NUTS Level 2 (\code{NUTS2})
#'
#' - NUTS Level 3 (\code{NUTS3})
#'
#' @param geog NUTS Level 1, 2 or 3
#' @param year Year (\code{2015} or \code{2018})
#' @param nations For which UK nations (\code{c("E","W","S","N")})
#' @param type Boundary Clipping
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
#' @return Output is a simple feature object containing the relevant boundaries.
#'
#' @examples
#' read_nuts("NUTS1")
#'
#' \dontrun{
#' read_nuts("NUTS2", nations = c("E"))
#' }
#'
#' @import lifecycle
#' @import dplyr
#' @importFrom sf st_read
#' @importFrom stringr str_sub
#' @importFrom curl has_internet
#' @importFrom utils askYesNo
#'
#' @export

##########################################

read_nuts <- function(geog,
                      year = 2018,
                      nations = c("E", "W", "S", "N"),
                      type = "BGC",
                      crs = 4326
){

  # Stop if not internet connection
  if (has_internet() == FALSE) stop("Are you connected to the internet?")

  # Define Coordinate Reference System
  if (crs != 4326 & crs != 27700){
    answer <- askYesNo("Are you sure you want to use a non-recomended CRS?", default = TRUE, prompts = getOption("askYesNo", gettext(c("Yes", "No"))))
    if (answer == FALSE) {
      stop("You choose not to proceed")
    } else{
      warning("Using non-standard coordinate reference system (CRS)")
    }
  }

  # Define Year
  if (year != 2015 & year != 2018) stop("'year' must be either 2015 or 2018")

  if (type == "BFC"){
    type <- 0
  } else if (type == "BFE"){
    type <- 1
  } else if (type == "BGC"){
    type <- 2
  } else{
    stop("'type' must be one of BGC, BFC or BFE, see help(get_census_sf) for definitions")
  }

  # Create shapefiles dir if doesn't already exist
  if (!dir.exists("shapefiles")) dir.create("shapefiles")

  # Construct file name
  savename <- paste0("shapefiles/", paste(geog, paste(nations, collapse = ""), year, type, crs, sep = "_"), ".shp")

  # Check file doesn't already exist
  if (file.exists(savename)){
    sf <- st_read(savename, stringsAsFactors = FALSE, quiet = TRUE)
  } else{
    message("Downloading from ONS...")

    boundary_type = "Eurostat_Boundaries"

    if (geog == "NUTS1") {
      url <- select_url(boundary_type = boundary_type, year = year, month = "January", geog = "NUTS_Level_1", type = "", crs = 4326, tag = "", num = type)
    } else if (geog == "NUTS2"){
      url <- select_url(boundary_type = boundary_type, year = year, month = "January", geog = "NUTS_Level_2", type = "", crs = 4326, tag = "", num = type)
    } else if (geog == "NUTS3"){
      url <- select_url(boundary_type = boundary_type, year = year, month = "January", geog = "NUTS_Level_3", type = "", crs = 4326, tag = "", num = type)
    } else{
      stop("Incorrect specification of argument 'geog', 'geog' accepts 'NUTS1', 'NUTS2' or 'NUTS3'")
    }

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
        stop("The shapefile you have selected doesn't seem to exist, sorry!")
      }
    })

    # Renaming
    names(sf) <- c(str_sub(names(sf)[1:2], start= -2),
                   names(sf)[3:length(names(sf))])

    # Apply country filtering
    sf <- sf %>%
      mutate_if(is.factor, as.character) %>%
      mutate(country = substr(.data$cd, 3, 4)) %>%
      mutate(country = ifelse(.data$country == "M", "S", # Scotland
                              ifelse(.data$country == "L", "W", # Wales
                                     ifelse(.data$country == "N", "N", # NI
                                            "E")))) %>% # Rest are England
      filter(.data$country %in% nations) %>%
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
