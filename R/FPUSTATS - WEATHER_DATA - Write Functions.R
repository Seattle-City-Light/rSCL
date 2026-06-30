
#' This function writes a properly formatted Streetlight forecast to the FPUSTATS Database STREETLIGHT_FORECAST table
#'
#' @param year Year of the weather data of interest
#' @param month Month of the weather data of interest
#' @param user Keyring Manager username
#' @param database Keyring Manager database name
#' @param connect True or false if you want the function to handle the FPUSTATS connect
#' @return Writes forecast to FPUSTATS STREETLIGHT_FORECAST table and returns print statement "Successfully uploaded forecast vintage", unique(sl_forecast$FORECAST_VINTAGE)
#' @export
fpustats_update_weather_data <- function(year = 2026,
                                         month = 1,
                                         user = 'MATTHEW',
                                         database = 'EPMMART_RW',
                                         connect = T){

  `%>%` <- dplyr::`%>%`

  if(connect){
    scl_connect(user, database)
  }

  num_days <- days_in_month(ymd(paste(year,month,1)))

  path <- paste0('https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?network=WA_ASOS&station=SEA&data=tmpf&data=dwpf&data=drct&data=sped&data=mslp&data=p01m&data=skyc1&year1=',
                 year,
                 '&month1=',
                 month,
                 '&day1=1&year2=',
                 year,
                 '&month2=',
                 month,
                 '&day2=',
                 num_days,
                 '&tz=Etc%2FUTC&format=onlycomma&latlon=no&elev=no&missing=M&trace=T&direct=no&report_type=3&report_type=4')


  data <- curl::curl_fetch_memory(path)

  parsed_data <- read.csv(text = rawToChar(data$content))

  processed_data <- parsed_data %>%
    mutate(DATETIME = ymd_hm(valid),
           DATE = date(DATETIME),
           HOUR = hour(DATETIME)) %>%
    group_by(DATE, HOUR) %>%
    slice(1) %>%
    mutate(DATETIME = ymd_h(paste(DATE,HOUR)))

  processed_data <- processed_data %>%
    ungroup() %>%
    select(DATETIME,tmpf:skyc1,station)

  colnames(processed_data) <- c('DATETIME','TEMPERATURE','DEW_POINT_TEMPERATURE',
                                'WIND_DIRECTION','WIND_SPEED','SEA_LEVEL_PRESSURE',
                                'PRECIPITATION_DEPTH_1HR','CLOUD_COVERAGE','STATION_ID')

  options(warn = -1)
  processed_data$WIND_SPEED <- as.numeric(processed_data$WIND_SPEED)
  processed_data$WIND_DIRECTION <- as.numeric(processed_data$WIND_DIRECTION)
  processed_data$SEA_LEVEL_PRESSURE <- as.numeric(processed_data$SEA_LEVEL_PRESSURE)
  processed_data$PRECIPITATION_DEPTH_1HR <- as.numeric(processed_data$PRECIPITATION_DEPTH_1HR)
  options(warn = 0)

  setDT(processed_data)
  processed_data[CLOUD_COVERAGE == 'CLR', CLOUD_COVERAGE := 1]
  processed_data[CLOUD_COVERAGE == 'FEW', CLOUD_COVERAGE := 2]
  processed_data[CLOUD_COVERAGE == 'SCT', CLOUD_COVERAGE := 3]
  processed_data[CLOUD_COVERAGE == 'BKN', CLOUD_COVERAGE := 4]
  processed_data[CLOUD_COVERAGE == 'OVC', CLOUD_COVERAGE := 5]
  processed_data[CLOUD_COVERAGE == 'VV ', CLOUD_COVERAGE := NA]

  processed_data$PRECIPITATION_DEPTH_6HR <- NA


  processed_data$DATETIME_UTC <- with_tz(processed_data$DATETIME,'UTC')

  processed_data$DATETIME <- NULL

  processed_data <- processed_data %>%
    select(DATETIME_UTC,TEMPERATURE,DEW_POINT_TEMPERATURE, SEA_LEVEL_PRESSURE,
           WIND_DIRECTION,WIND_SPEED,CLOUD_COVERAGE,PRECIPITATION_DEPTH_1HR,
           PRECIPITATION_DEPTH_6HR, STATION_ID)

  processed_data$DATETIME_UTC <- as.character(processed_data$DATETIME_UTC)

  for(i in seq(1,nrow(processed_data))){

    temp <- processed_data[i,]

    query <- paste0("INSERT INTO FPUSTATS.WEATHER_DATA VALUES(",paste(temp, collapse = ', '),")")

    RJDBC::dbSendUpdate(con, query)

  }

  RJDBC::dbDisconnect(con)

  return(paste("Successfully uploaded Weather Data for ", year,'-',month))
}









#' This function deletes streetlight forecasts from the FPUSTATS Database
#'
#' @param forecast_vintage Year of the vintage of interest
#' @param eff_dt upload date of the specific forecast you want to delete
#' @param connect T/F if you want the function to do the connect or not (typically false if looping over multiple reports)
#' @param user Keyring Manager username
#' @param database Keyring Manager database name
#' @return Deletes specific forecasts from database and returns the print statement "OUL forecast for vintage", forecast_vintage, "Uploaded on:", eff_dt,"has been deleted."
#' @export
fpustats_delete_streetlight_forecast <- function(forecast_vintage = 2026,
                                                 eff_dt = '2026-02-10',
                                                 connect = T,
                                                 user = 'MATTHEW',
                                                 database = 'EPMMART_RW'){
  if(connect){
    scl_connect(user, database)
  }


  query <- paste0("DELETE FROM FPUSTATS.STREETLIGHT_FORECAST WHERE FORECAST_VINTAGE = ", forecast_vintage,
                  "AND EFF_DT = TO_DATE('",eff_dt,"','YYYY-MM-DD')")

  RJDBC::dbSendUpdate(con, query)

  if(connect){
    RJDBC::dbDisconnect(con)
  }

  return(paste("Streetlight forecast for vintage", forecast_vintage, "Uploaded on:", eff_dt,"has been deleted."))

}






