
#' This function writes a properly formatted Streetlight forecast to the FPUSTATS Database STREETLIGHT_FORECAST table
#'
#' @param year Year of the weather data of interest
#' @param month Month of the weather data of interest
#' @param read_all T if you want to pull down all weather data
#' @param connect True or false if you want the function to handle the FPUSTATS connect
#' @return Writes forecast to FPUSTATS STREETLIGHT_FORECAST table and returns print statement "Successfully uploaded forecast vintage", unique(sl_forecast$FORECAST_VINTAGE)
#' @export
fpustats_read_weather_data <- function(start_date = '1980-01-01',
                                       end_date = '1980-01-15',
                                       read_all = F,
                                       connect = T){

  `%>%` <- dplyr::`%>%`

  if(connect){
    scl_connect('EPMMART_RW')
  }

  if(read_all){

    query <- "Select* from FPUSTATS.WEATHER_DATA"

  } else {

    query <- paste0("Select*
                    FROM FPUSTATS.WEATHER_DATA
                    WHERE TRUNC(DATETIME_UTC) >= to_date('", start_date,"', 'YYYY-MM-DD')",
                    " AND TRUNC(DATETIME_UTC) < to_date('", end_date,"', 'YYYY-MM-DD')")

  }

  weather_data <- RJDBC::dbGetQuery(con, query)

  if(connect){
    RJDBC::dbDisconnect(con)
  }

  return(weather_data)
}


