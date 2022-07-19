#' Move to the next year
#'
#' @param df A data.frame
#' @param year Year
#'
#' @import dplyr
#' @importFrom rlang :=
#'
#' @export

plus_year <- function(df, year){

  # Start by assuming there are no changes
    df2 <- df %>%
      select(contains(substr(year-1, 3, 4)))
    names(df2) <- sub(substr(year-1, 3, 4), substr(year, 3, 4),names(df2))
    df <- cbind(df,df2)

  # Define relevant lookup name
    lookup <- paste0("lookup_", substr(year-1, 3, 4), substr(year, 3, 4))

  # See if lookup exists
    if(exists(lookup)){
      message(paste0("Implementing changes between ", year-1, " and ", year))

      lk <- get(lookup)
      # Remove columns that will change
      df <- df %>%
        select(-contains(names(lk %>% select(contains(substr(year, 3, 4))))))

      # Join in new columns
      df <- dplyr::left_join(df,
                             lk,
                             by = names(lk %>% select(contains(substr(year-1, 3, 4)))))

    } else { # Do nothing
      message(paste0("No changes between ", year-1, " and ", year))
    }

  # For each geography fill in the unchanged rows
    for(var in c("LAD", "UTLA", "LEA")){

      cd0 <- paste0(var, substr(year-1, 3, 4), "CD")
      cd1 <- paste0(var, substr(year, 3, 4), "CD")
      nm0 <- paste0(var, substr(year-1, 3, 4), "NM")
      nm1 <- paste0(var, substr(year, 3, 4), "NM")

      df <- df %>%
        dplyr::mutate({{ cd1 }} := ifelse(is.na(get(cd1)), get(cd0), get(cd1)),
                      {{ nm1 }} := ifelse(is.na(get(nm1)), get(nm0), get(nm1)))
    }

  return(df)
}

#' Create a lookup across years
#' Wrapper for `plus_year`
#'
#' @param year1 Start year
#' @param year2 End year
#' @param geog A geographic location identifier
#' @param between If `TRUE` (default) then includes years between `year1` and `year2`, else if `FALSE` then just keep `year1` and `year2`
#' @param changes_only Just keep changes
#'
#' @import dplyr
#'
#' @export

across_yr_lookup <- function(year1, year2, geog = c("LAD", "UTLA", "LEA"), between = TRUE, changes_only = FALSE){

  # Start with 2011 data
  df <- ukgeog::BASE_2011

  # Loop until end year
  for(yr in 2012:year2){
    df <- suppressMessages(ukgeog::plus_year(df = df, year = yr))
  }

  # Whether to retain in-between years or not
  if(between != TRUE){
    df <- df %>%
          select(contains(substr(c(year1, year2), 3, 4)))
  } else {
    df <- df %>%
          select(contains(substr(year1:year2, 3, 4)))
  }

  # Only keep rows where changes in codes or names have occurred
  if(changes_only == TRUE){
    keep <- apply(df %>% select(ends_with("CD") & starts_with(geog)), 1, function(x) length(unique(x[!is.na(x)])) != 1)
    code_changes <- df[keep,]

    keep <- apply(df %>% select(ends_with("NM") & starts_with(geog)), 1, function(x) length(unique(x[!is.na(x)])) != 1)
    name_changes <- df[keep,]

    df <- full_join(code_changes, name_changes, by = names(df))
  }

  # Finally, select only relevant geographies defined by `geog`
  df <- df %>%
    dplyr::select(contains(geog))

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

  return(ukgeog::across_yr_lookup(year1 = year, year2 = year, geog = c("LAD", "UTLA", "LEA")))

}
