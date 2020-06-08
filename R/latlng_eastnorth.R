#' Function to convert Eastings / Northings to Lat / Long
#'
#' \code{convert_lnglat} converts columns containing Eastings and Northings in a dataframe to columns containing Longitude and Latitude
#'
#' This function takes a dataframe with columns containing Eastings and Northings and adds columns containing Longitudes and Latitudes.
#'
#' @param df A dataframe with at least 2 columns
#' @param easting Column containing Easting
#' @param northing Column containing Northing
#'
#' @return Output is a dataframe identical to the input dataframe with two additional columns, 'Long' and 'Lat'.
#'
#' @examples
#'
#' \dontrun{
#' convert_lnglat(df, "eastings", "northings")
#' }
#'
#' @import dplyr
#' @importFrom sp SpatialPointsDataFrame spTransform CRS
#'
#' @export

convert_lnglat <- function(df, # Dataframe
                           easting, # Column containing Easting
                           northing # Column containing Northing
){

  # Create a SpatialPointsDataFrame
  spdf <- SpatialPointsDataFrame(df %>% select(!! easting, !! northing),
                                 data = df,
                                 proj4string = CRS("+init=epsg:27700"))

  # Convert to lat/lng
  conv <- spTransform(spdf, CRS("+init=epsg:4326"))

  # Back to dataframe
  new_df <- as.data.frame(conv)

  colnames(new_df)[ncol(new_df)-1] <- "Long"
  colnames(new_df)[ncol(new_df)] <- "Lat"

  return(new_df)

}
