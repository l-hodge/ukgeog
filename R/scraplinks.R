#' Extract link texts and urls from a web page
#'
#' Simple function based on [Gist by Paul Rougieux](https://gist.github.com/paulrougieux/e1ee769577b40cd9ed9db7f75e9a2cc2)
#'
#' @param url character an url
#' @return a data frame of link text and urls
#' @import dplyr "%>%"
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text
#' @export

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
#' @param geog Geographical
#' @param type Type
#' @param crs CRS
#'
#' @export

select_url <- function(year, geog, type, crs, tag){

  scraplinks("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/") %>%
    filter(grepl(year,link),
           grepl(geog,link),
           grepl("December", link),
           grepl(type, link),
           grepl(tag, link),
           grepl("MapServer", url)) %>%
    select(url) %>%
    slice(1) %>%
    as.character() %>%
    paste0("https://ons-inspire.esriuk.com",.,
           "/0/query?where=1%3D1&outFields=*&outSR=",
           crs,
           "&f=json")

}
