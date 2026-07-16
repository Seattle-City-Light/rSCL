
#' This function writes a properly formatted Streetlight forecast to the FPUSTATS Database STREETLIGHT_FORECAST table
#'
#' @param year Year of the weather data of interest
#' @param month Month of the weather data of interest
#' @param connect True or false if you want the function to handle the FPUSTATS connect
#' @return Writes forecast to FPUSTATS STREETLIGHT_FORECAST table and returns print statement "Successfully uploaded forecast vintage", unique(sl_forecast$FORECAST_VINTAGE)
#' @export
fpustats_write_weather_data <- function(year = 2026,
                                        month = 1,
                                        connect = T){

  `%>%` <- dplyr::`%>%`

  if(connect){
    scl_connect('EPMMART_RW')
  }

  num_days <- lubridate::days_in_month(lubridate::ymd(paste(year,month,1)))

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
    dplyr::mutate(DATETIME = lubridate::ymd_hm(valid),
                  DATE = lubridate::date(DATETIME),
                  HOUR = lubridate::hour(DATETIME),

                  tmpf = ifelse(tmpf=='T' | tmpf=='M', NA, tmpf),
                  dwpf = ifelse(dwpf=='T' | dwpf=='M', NA, dwpf),
                  tmpf = as.numeric(tmpf),
                  dwpf = as.numeric(dwpf),

                  drct = ifelse(drct=='T' | drct=='M', NA, drct),
                  sped = ifelse(sped=='T' | sped=='M', NA, sped),
                  drct = as.numeric(drct),
                  sped = as.numeric(sped),
                  drct = ifelse(drct == 0 & sped == 0, NA, drct),
                  sped = ifelse(drct == 0 & sped == 0, NA, sped),
                  mslp = ifelse(mslp=='T' | mslp=='M', NA, mslp),
                  mslp = as.numeric(mslp),
                  p01m = ifelse(p01m=='T' | p01m=='M', NA, p01m),
                  p01m = as.numeric(p01m)) %>%
    dplyr::group_by(DATE, HOUR) %>%
    dplyr::summarise(tmpf = mean(tmpf,na.rm=T),
                     dwpf = mean(dwpf,na.rm=T),
                     drct = mean(drct,na.rm=T),
                     sped = mean(sped,na.rm=T),
                     mslp = mean(mslp,na.rm=T),
                     p01m = mean(p01m,na.rm=T),
                     skyc1 = dplyr::first(skyc1),
                     station = dplyr::first(station)) %>%
    dplyr::mutate(DATETIME = lubridate::ymd_h(paste(DATE,HOUR)))

  data.table::setDT(processed_data)
  processed_data <- processed_data[, lapply(.SD, function(x) replace(x, is.nan(x), NA))]


  processed_data <- processed_data %>%
    dplyr::ungroup() %>%
    dplyr::select(DATETIME,tmpf:skyc1,station)

  colnames(processed_data) <- c('DATETIME','TEMPERATURE','DEW_POINT_TEMPERATURE',
                                'WIND_DIRECTION','WIND_SPEED','SEA_LEVEL_PRESSURE',
                                'PRECIPITATION_DEPTH_1HR','CLOUD_COVERAGE','STATION_ID')

  data.table::setDT(processed_data)
  processed_data[CLOUD_COVERAGE == 'CLR', CLOUD_COVERAGE := 1]
  processed_data[CLOUD_COVERAGE == 'FEW', CLOUD_COVERAGE := 2]
  processed_data[CLOUD_COVERAGE == 'SCT', CLOUD_COVERAGE := 3]
  processed_data[CLOUD_COVERAGE == 'BKN', CLOUD_COVERAGE := 4]
  processed_data[CLOUD_COVERAGE == 'OVC', CLOUD_COVERAGE := 5]
  processed_data[CLOUD_COVERAGE == 'VV ', CLOUD_COVERAGE := NA]

  processed_data$PRECIPITATION_DEPTH_6HR <- NA


  processed_data$DATETIME_UTC <-lubridate::with_tz(processed_data$DATETIME,'UTC')

  processed_data$DATETIME <- NULL

  processed_data <- processed_data %>%
    dplyr::select(DATETIME_UTC,TEMPERATURE,DEW_POINT_TEMPERATURE, SEA_LEVEL_PRESSURE,
                  WIND_DIRECTION,WIND_SPEED,CLOUD_COVERAGE,PRECIPITATION_DEPTH_1HR,
                  PRECIPITATION_DEPTH_6HR, STATION_ID)

  processed_data$DATETIME_UTC <- paste0("TO_DATE('",as.character(processed_data$DATETIME_UTC),"', 'YYYY-MM-DD HH24:MI:SS')")
  processed_data$STATION_ID <- paste0("'",as.character(processed_data$STATION_ID),"'")
  processed_data <- processed_data %>%
    dplyr::mutate(dplyr::across(everything(), ~ tidyr::replace_na(as.character(.x), "NULL")))

  for(i in seq(1,nrow(processed_data))){

    temp <- processed_data[i,]

    query <- paste0("INSERT INTO FPUSTATS.WEATHER_DATA VALUES(",paste(temp, collapse = ', '),")")

    RJDBC::dbSendUpdate(con, query)

  }

  RJDBC::dbDisconnect(con)

  return(paste("Successfully uploaded Weather Data for ", year,'-',month))
}





#' This function deletes weather data from the FPUSTATS Database
#'
#' @param year Year of the report of interest
#' @param month Month of the report of interest
#' @param connect T/F if you want the function to do the connect or not (typically false if looping over multiple reports)
#' @return Deletes specific year/month from database and returns the print statement "Weather Data for", year,'-',month," has been deleted."
#' @export
fpustats_delete_weather_data <- function(year = 1980,
                                         month = 1,
                                         connect = T){
  if(connect){
    scl_connect('EPMMART_RW')
  }


  query <- paste0("DELETE FROM FPUSTATS.WEATHER_DATA WHERE EXTRACT(YEAR FROM DATETIME_UTC) = ", year,
                  " AND EXTRACT(MONTH FROM DATETIME_UTC) = ", month)

  RJDBC::dbSendUpdate(con, query)

  if(connect){
    RJDBC::dbDisconnect(con)
  }

  return(paste("Weather Data for", year,'-',month," has been deleted."))

}

