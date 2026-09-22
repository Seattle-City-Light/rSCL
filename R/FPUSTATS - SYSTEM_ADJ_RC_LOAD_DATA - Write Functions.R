

#' This function writes properly formatted hourly rate class load to the fpustats database
#'
#' @param hourly_rate_class_data cleaned and adjusted hourly rate class load
#' @return Writes hourly data to FPUSTATS SYSTEM_ADJ_RC_LOAD_DATA table and returns print statement "Successfully uploaded data for ", year,'-',month"
#' @export
fpustats_write_system_adj_rate_class_load <- function(hourly_rate_class_data = data.frame()){

  `%>%` <- dplyr::`%>%`

  #hourly_rate_class_data <- readRDS("I:/FINANCE/FPU/Matthew/Export Dumps/clean_hourly_data.RDS")

  hourly_rate_class_data <- hourly_rate_class_data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(FINAL_MWH = FINAL_KWH/1000) %>%
    dplyr::select(DATETIME,RATE_CLASS,SERVICE_AREA, RATE_TYPE,FINAL_MWH) %>%
    dplyr::mutate(EFF_DT = toupper(as.character(format(Sys.Date(),"%d-%b-%y")))) %>%
    dplyr::rename(DATETIME_PT = DATETIME)

  scl_connect('EPMMART_RW')

  for(i in seq(1,nrow(hourly_rate_class_data))){

    temp <- hourly_rate_class_data[i,]
    temp$DATETIME_PT <- paste0("TO_DATE('",as.character(temp$DATETIME_PT),"', 'YYYY-MM-DD HH24:MI:SS')")
    temp$RATE_CLASS <- paste0("'",temp$RATE_CLASS,"'")
    temp$SERVICE_AREA <- paste0("'",temp$SERVICE_AREA,"'")
    temp$RATE_TYPE <- paste0("'",temp$RATE_TYPE,"'")
    temp$EFF_DT <- paste0("'",temp$EFF_DT,"'")

    query <- paste0("INSERT INTO FPUSTATS.SYSTEM_ADJ_RC_LOAD_DATA VALUES(",paste(temp, collapse = ', '),")")

    RJDBC::dbSendUpdate(con, query)

  }

  RJDBC::dbDisconnect(con)

  return("Successfully uploaded hourly rate class load data.")
}







#' This function deletes weather data from the FPUSTATS Database
#'
#' @param year Year of the report of interest
#' @param month Month of the report of interest
#' @param connect T/F if you want the function to do the connect or not (typically false if looping over multiple reports)
#' @return Deletes specific year/month from database and returns the print statement "Weather Data for", year,'-',month," has been deleted."
#' @export
fpustats_delete_rate_class_load_data <- function(year = 2024,
                                                 month = 3,
                                                 connect = T){
  if(connect){
    scl_connect('EPMMART_RW')
  }


  query <- paste0("DELETE FROM FPUSTATS.SYSTEM_ADJ_RC_LOAD_DATA WHERE EXTRACT(YEAR FROM DATETIME_PT) = ", year,
                  " AND EXTRACT(MONTH FROM DATETIME_PT) = ", month)

  RJDBC::dbSendUpdate(con, query)

  if(connect){
    RJDBC::dbDisconnect(con)
  }

  return(paste("Rate class load data for", year,'-',month," has been deleted."))

}
