#' Function to convert Eastings / Northings to Lat / Long
#'
#' \code{convert_lnglat} converts columns containing Eastings and Northings in a dataframe to columns containing Longitude and Latitude
#'
#' This function...
#'
#' @param df A dataframe
#' @param easting Column containing Easting
#' @param northing Column containing Northing
#'
#'
#' @return Output is...
#' @examples
#'
#' \dontrun{
#' convert_lnglat(df, "eastings", "northings")
#' }
#' @import lifecycle
#' @import dplyr
#' @import sp
#' @import rgdal
#' @import stringr
#' @export

convert_lnglat <- function(df, # Dataframe
                           easting, # Column containing Easting
                           northing # Column containing Northing
){

  # Set CRS'
  ukgrid <- "+init=epsg:27700"
  latlong <- "+init=epsg:4326"

  # Create a SpatialPointsDataFrame
  spdf <- sp::SpatialPointsDataFrame(df %>% select(!! easting, !! northing),
                                     data = df,
                                     proj4string = CRS(ukgrid))

  # Convert to lat/lng
  conv <- sp::spTransform(spdf, CRS(latlong))

  # Back to dataframe
  new_df <- as.data.frame(conv)

  colnames(new_df)[ncol(new_df)-1] <- "Long"
  colnames(new_df)[ncol(new_df)] <- "Lat"

  return(new_df)

}
