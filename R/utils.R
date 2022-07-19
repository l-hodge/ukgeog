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

scraplinks <- function(url){

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
#' @param year Year
#' @param month Month
#' @param geog Geographical
#' @param type Type
#' @param crs CRS
#' @param boundary_type Boundary Type
#' @param tag Tag
#' @param num Number
#'
#' @importFrom dplyr filter select slice
#' @importFrom rlang .data

select_url <- function(boundary_type, geog, year, month, type, crs, tag, num){

  text <- scraplinks(
            paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/",
                   boundary_type)
          ) %>%
          filter(grepl(year, .data$link),
                 grepl(geog, .data$link),
                 grepl(month, .data$link),
                 grepl(type, .data$link),
                 grepl(tag, .data$link),
                 grepl("MapServer", .data$url)) %>%
          select(.data$url)

  url <- paste0("https://ons-inspire.esriuk.com",
                text$url,
                "/",
                num,
                "/query?where=1%3D1&outFields=*&outSR=",
                crs,
                "&f=json")

  return(url)

}
