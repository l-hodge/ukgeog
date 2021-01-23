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
#' @importFrom sf st_read st_write
#' @importFrom stringr str_sub
#' @importFrom curl has_internet
#' @importFrom utils askYesNo
#' @importFrom utils data
#'
#' @export

##########################################

read_admin <- function(geog,
                       year = 2019,
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

  crs <- crs

  # Define Year
  if (geog != "LAD" & (year < 2018 | year > 2019)) stop("'year' must be either 2018 or 2019")
  if (geog == "LAD" & (year < 2018 | year > 2020)) stop("'year' must be either 2018, 2019 or 2020")
  year <- year

  # Define boundary clipping
  if (type %in% c("BGC", "BFC", "BFE", "BUC")){
    type <- type
  } else{
    stop("'type' must be one of BGC, BFC, BFE or BUC, see help(get_sf) for definitions")
  }

  month <- "December"

  if (geog == "UTLA") {
    bound <- "Counties_and_Unitary_Authorities"
    tag <- "UK"
  } else if (geog == "LAD"){
    bound <-"Local_Authority_Districts"
    tag <- "UK"
    if (year == 2020){
      month <- "May"
    }
  } else if (geog == "GOR"){
    bound <- "Regions"
    tag <- "EN"
    message("Note: Regions only exist for England")
  } else if (geog == "NAT"){
    bound <- "Countries"
    tag <- "UK"
  } else{
    stop("Incorrect specification of argument 'geog', 'geog' accepts 'UTLA', 'LAD', 'GOR' or 'NAT'")
  }

  # Create shapefiles dir if doesn't already exist
  if (!dir.exists("shapefiles")) dir.create("shapefiles")

  # Construct file name
  savename <- paste0("shapefiles/", paste(bound, paste(nations, collapse = ""), year, tag, type, crs, sep = "_"), ".shp")

  # Check file doesn't already exist
  if (file.exists(savename)){
    sf <- st_read(savename, stringsAsFactors = FALSE, quiet = TRUE)
  } else{
    message("Downloading from ONS...")

    # Construct URL for API call
    url <- paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/",
                  bound,
                  "_",
                  month,
                  "_",
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
      sf <- st_read(url, quiet = TRUE)
    })

    # Renaming
    names(sf) <- c(names(sf)[1:1],
                   str_sub(names(sf)[2:3], start= -2),
                   names(sf)[4:length(names(sf))])

    # Apply country filtering
    sf <- sf %>%
          mutate_if(is.factor, as.character) %>%
          mutate(country = substr(.data$cd, 1, 1)) %>%
          filter(.data$country %in% nations) %>%
          mutate(country = case_when(country == "E" ~ "England",
                                     country == "S" ~ "Scotland",
                                     country == "W" ~ "Wales",
                                     country == "N" ~ "Northern Ireland"))

    if (geog == "UTLA" & year == 2019) {
      sf <- sf %>%
            left_join(ukgeog::lea2019lookup, by = c("cd" = "UTLA19CD", "nm" = "UTLA19NM"))
    }

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

  # Create shapefiles dir if doesn't already exist
  if (!dir.exists("shapefiles")) dir.create("shapefiles")

  # Construct file name
  savename <- paste0("shapefiles/", paste(bound, paste(nations, collapse = ""), year, "EW", type, crs, sep = "_"), ".shp")

  # Check file doesn't already exist
  if (file.exists(savename)){
    sf <- st_read(savename, stringsAsFactors = FALSE, quiet = TRUE)
  } else{
    message("Downloading from ONS...")

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
      sf <- st_read(url, quiet = TRUE)
    })

    # Renaming
    names(sf) <- c(names(sf)[1:1],
                   str_sub(names(sf)[2:3], start= -2),
                   names(sf)[4:length(names(sf))])

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

  crs <- crs

  # Define boundary clipping
  if (type %in% c("BGC", "BFC", "BFE", "BUC")){
    type <- type
  } else{
    stop("'type' must be one of BGC, BFC, BFE or BUC, see help(read_elec) for definitions")
  }

  if (geog == "WM") {
    bound <- "Westminster_Parliamentary_Constituencies"
    tag <- "UK"
    # Define Year
    if (year != 2018 & year != 2019) stop("'year' must be 2018 or 2019")
    year <- year
    if (year == 2018){boundary <- "_"}else{boundary <- "_Boundaries_"}
  } else if (geog == "EU"){
    bound <- "European_Electoral_Regions"
    tag <- "UK"
    boundary <- "_Boundaries_"
    if (year != 2018) stop("Only available for 2018")
  } else if (geog == "WAC"){
    bound <- "National_Assembly_for_Wales_Constituencies"
    tag <-  "WA"
    boundary <- "_"
    if (year != 2018) stop("Only available for 2018")
  } else if (geog == "WAR"){
    bound <- "National_Assembly_for_Wales_Electoral_Regions"
    boundary <- "_Boundaries_"
    tag <-  "WA"
    if (year != 2018) stop("Only available for 2018")
  } else{
    stop("Incorrect specification of argument 'geog', 'geog' only accepts 'WM', 'EU', 'WAC' or 'WAR'")
  }

  # Construct URL for API call
  url <- paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Electoral_Boundaries/",
                bound,
                "_December_",
                year,
                boundary,
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
    sf <- st_read(url, quiet = TRUE) %>% select(.data$objectid, everything())
  })

  # Renaming
  names(sf) <- c(names(sf)[1:1],
                 str_sub(names(sf)[2:3], start= -2),
                 names(sf)[4:length(names(sf))])

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
    sf <- st_read(url, quiet = TRUE)
  })

  # Renaming
  names(sf) <- c(names(sf)[1:1],
                 str_sub(names(sf)[2:3], start= -2),
                 names(sf)[4:length(names(sf))])

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

  return(sf)

}
