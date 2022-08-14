#' Extract link texts and urls from a web page
#'
#' Simple function based on [Gist by Paul Rougieux](https://gist.github.com/paulrougieux/e1ee769577b40cd9ed9db7f75e9a2cc2)
#'
#' @param url character an url
#'
#' @return a data frame of link text and urls
#'
#' @importFrom dplyr "%>%"
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text

scraplinks <- function(url) {

  # Create an html document from the url
  webpage <- xml2::read_html(url)

  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()

  return(data.frame(link = link_, url = url_))
}

#' Select the url to download from
#'
#' @param year Year
#' @param month Month
#' @param geog Geographical
#' @param type Type
#' @param crs CRS
#' @param boundary_type Boundary Type
#' @param tag Tag
#' @param num Number
#'
#' @importFrom dplyr "%>%" select
#' @importFrom rlang .data

select_url <- function(boundary_type, geog, year, month, type, crs, tag, num) {
  text <- filter_links(boundary_type, geog, year, month, type, tag) %>%
    dplyr::select(.data$url)

  url <- paste0(
    "https://ons-inspire.esriuk.com",
    text$url,
    "/",
    num,
    "/query?where=1%3D1&outFields=*&outSR=",
    crs,
    "&f=json"
  )

  return(url)
}

#' Wrapper for scraplinks to scrap ONS Open Geography
#'
#' @param year Year
#' @param month Month
#' @param geog Geographical
#' @param type Type
#' @param boundary_type Boundary Type
#' @param tag Tag
#'
#' @importFrom dplyr "%>%" filter
#' @importFrom rlang .data

filter_links <- function(boundary_type, geog, year, month, type, tag) {
  df <- scraplinks(
    paste0(
      "https://ons-inspire.esriuk.com/arcgis/rest/services/",
      boundary_type
    )
  ) %>%
    dplyr::filter(
      grepl(year, .data$link),
      grepl(geog, .data$link),
      grepl(month, .data$link),
      grepl(type, .data$link),
      grepl(tag, .data$link),
      grepl("MapServer", .data$url)
    )
  return(df)
}

#' Checks which years are available for a given geography
#'
#' @param year Year
#' @param month Month
#' @param geog Geographical
#' @param type Type
#' @param boundary_type Boundary Type
#' @param tag Tag
#'
#' @importFrom dplyr "%>%" select
#' @importFrom stringr str_extract
#' @importFrom rlang .data

check_years <- function(boundary_type, geog, year, month, type, tag) {

  text <- filter_links(boundary_type, geog, year, month, type, tag) %>%
    dplyr::select(.data$link)

  x <- as.numeric(stringr::str_extract(text$link, "20[0-9]+"))

  return(
    list(
      year = unique(substr(x, 1, 4)),
      geog = geog
    )
  )

}

#' See available shapefiles
#'
#' Wrapper for check_years
#'
#' @return A dataframe of geographies and years available
#'
#' @export

available_sf <- function(){

  df <- c()

  for (i in 1:nrow(ukgeog::metadata)) {
    x <- as.data.frame(check_years(boundary_type = ukgeog::metadata[i, "boundary_type"],
                                   year = "",
                                   month = ukgeog::metadata[i, "month"],
                                   geog = ukgeog::metadata[i, "geog"],
                                   type = ukgeog::metadata[i, "type"],
                                   tag = ukgeog::metadata[i, "tag"])) %>%
      dplyr::mutate(geog_short = ukgeog::metadata[i, "geog_short"])
    df <- rbind(df, x)
  }

  return(df)
}

# Interactively select a shapefile
#
# @importFrom utils menu
#
# @export

# interactive_select <- function(){
#
#   x <- menu(ukgeog::metadata[, "geog"],
#             title="Which geography?")
#
#   yr <- check_years(boundary_type = ukgeog::metadata[x, "boundary_type"],
#                     year = "",
#                     month = ukgeog::metadata[x, "month"],
#                     geog = ukgeog::metadata[x, "geog"],
#                     type = ukgeog::metadata[x, "type"],
#                     tag = ukgeog::metadata[x, "tag"])$year
#
#   y <- menu(yr,
#             title="Which year?")
#
#   c <- c("BGC (recommended)", "BFC", "BFE", "BUC")
#
#   z <- menu(c,
#             title="Which clippling?")
#
#   if (ukgeog::metadata[x, "boundary_type"] == "Administrative_Boundaries") {
#     sf <- read_admin(geog = ukgeog::metadata[x, "geog"],
#                      year = yr[y],
#                      type = substr(c[z], 1, 3))
#   }
#
#   return(sf)
#
#   # return(list(ukgeog::metadata[x, "geog"],
#   #             ukgeog::metadata[x, "boundary_type"],
#   #             yr[y],
#   #             substr(c[z], 1, 3)))
# }

