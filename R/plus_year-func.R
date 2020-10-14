#' Move to the next year
#'
#' @param df A data.frame
#' @param geog Geographic location identifier, can be mutiple
#' @param year Year
#'
#' @import dplyr
#' @importFrom rlang :=
#'
#' @export

plus_year <- function(df, geog, year){

  # Select only relevant geographies
  df <- df %>%
        dplyr::select(contains(geog))

  # For each relevant geography
  for(var in geog){

    cd0 <- paste0(var, substr(year-1, 3, 4), "CD")
    cd1 <- paste0(var, substr(year, 3, 4), "CD")
    nm0 <- paste0(var, substr(year-1, 3, 4), "NM")
    nm1 <- paste0(var, substr(year, 3, 4), "NM")

    lookup <- paste0(var, "_", substr(year-1, 3, 4), substr(year, 3, 4))

    if(exists(lookup)){
      message(paste0("Implementing changes to ", var, "'s between ", year-1, " and ", year))

      lk <- get(lookup)

      df <- dplyr::left_join(df, lk, by = c(paste0(cd0), paste0(nm0))) %>%
            dplyr::mutate({{ cd1 }} := ifelse(is.na(get(cd1)), get(cd0), get(cd1)),
                          {{ nm1 }} := ifelse(is.na(get(nm1)), get(nm0), get(nm1)))
    } else {
      message(paste0("No Changes to ", var, "'s between ", year-1, " and ", year))

      df <- df %>%
            dplyr::mutate({{ cd1 }} := get(cd0),
                          {{ nm1 }} := get(nm0))
    }

  }
  return(df)
}

# ----

# # If there isn't a lookup
# df_2011 <- data.frame(LAD11CD = c(1:5), LAD11NM = c("a", "b", "c", "d", "e"),
#                       UTLA11CD = rep(1,5), UTLA11NM = c("z"),
#                       stringsAsFactors = FALSE)
# plus_year(df_2011, "LAD", 2012)
#
# # If there is a lookup
# LAD_1112 <- data.frame(LAD11CD = c(3,5), LAD11NM = c("c", "e"), LAD12CD = c(6,7), LAD12NM = c("f", "g"), stringsAsFactors = FALSE)
# LAD_1213 <- data.frame(LAD12CD = c(1), LAD12NM = c("a"), LAD13CD = c(8), LAD13NM = c("h"), stringsAsFactors = FALSE)
# LAD_1314 <- data.frame(LAD13CD = c(6), LAD13NM = c("f"), LAD14CD = c(9), LAD14NM = c("i"), stringsAsFactors = FALSE)
#
# plus_year(df_2011, "LAD", 2012)


#' Create a lookup across years
#' Wrapper for `plus_year`
#'
#' @param year1 Start year
#' @param year2 End year
#' @param geog Geographic location identifier, can be mutiple
#' @param full If `TRUE` (default) then includes years between `year1` and `year2`, else if `FALSE` then just keep `year1` and `year2`
#'
#' @import dplyr
#'
#' @export

create_lookup <- function(year1, year2, geog, full = TRUE){

  # Start with 2011 data
  df <- ukgeog::BASE_2011

  # Loop until end year
  for(yr in 2012:year2){
    df <- suppressMessages(ukgeog::plus_year(df = df, geog = geog, year = yr))
  }

  # Whether to retain in-between years or not
  if(full != TRUE){
    df <- df %>%
          select(contains(substr(c(year1, year2), 3, 4)))
  } else {
    df <- df %>%
          select(contains(substr(year1:year2, 3, 4)))
  }

  # Remove any full row duplicates
  df <- df %>%
        distinct()

  return(df)
}

#' Create a within year lookup
#' Simple wrapper for `create_lookup`
#'
#' @param year Year
#'
#' @export

within_yr_lookup <- function(year){

  return(ukgeog::create_lookup(year1 = year, year2 = year, geog = c("LAD", "UTLA")))

}
