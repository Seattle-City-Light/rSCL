
#' This function writes a properly formatted average rates forecast to the FPUSTATS Database AVERAGE_RATES_FORECAST table
#'
#' @param forecast_path Directory of the formatted vintage forecast path
#' @param user Keyring Manager username
#' @param database Keyring Manager database name
#' @return Writes forecast to FPUSTATS AVERAGE_RATES_FORECAST table and returns print statement "Successfully uploaded forecast vintage", unique(avg_rates$FORECAST_VINTAGE)
#' @export
fpustats_write_average_rates_forecast <- function(forecast_path = "I:/FINANCE/FPU/Sales and Revenue Actuals Model/Update Database Scripts/Average Rates/2026 Average Rates Forecast.xlsx",
                                                  user = 'MATTHEW',
                                                  database = 'EPMMART_RW'){
  `%>%` <- dplyr::`%>%`

  avg_rates <- readxl::read_xlsx(forecast_path)

  data.table::setDT(avg_rates)

  avg_rates[is.na(avg_rates)] <- 0

  avg_rates <- avg_rates %>%
    dplyr::select(FORECAST_VINTAGE, YEAR, MONTH, RATE_CLASS, SERVICE_AREA, RATE_TYPE, AVG_RATE) %>%
    dplyr::mutate(EFF_DT = toupper(as.character(format(Sys.Date(),"%d-%b-%y"))))

  scl_connect(user, database)

  for(i in seq(1,nrow(avg_rates))){

    temp <- avg_rates[i,]
    temp$EFF_DT <- paste0("'",temp$EFF_DT,"'")
    temp$RATE_CLASS <- paste0("'",temp$RATE_CLASS,"'")
    temp$SERVICE_AREA <- paste0("'",temp$SERVICE_AREA,"'")
    temp$RATE_TYPE <- paste0("'",temp$RATE_TYPE,"'")


    query <- paste0("INSERT INTO FPUSTATS.AVERAGE_RATES_FORECAST VALUES(",paste(temp, collapse = ', '),")")

    RJDBC::dbSendUpdate(con, query)

  }

  RJDBC::dbDisconnect(con)

  return(paste("Successfully uploaded forecast vintage", unique(avg_rates$FORECAST_VINTAGE)))
}









#' This function deletes average rates forecasts from the FPUSTATS Database
#'
#' @param forecast_vintage Year of the vintage of interest
#' @param eff_dt upload date of the specific forecast you want to delete
#' @param connect T/F if you want the function to do the connect or not (typically false if looping over multiple reports)
#' @param user Keyring Manager username
#' @param database Keyring Manager database name
#' @return Deletes specific forecasts from database and returns the print statement "Average rates forecast for vintage", forecast_vintage, "Uploaded on:", eff_dt,"has been deleted."
#' @export
fpustats_delete_average_rates_forecast <- function(forecast_vintage = 2026,
                                                   eff_dt = '2026-02-12',
                                                   connect = T,
                                                   user = 'MATTHEW',
                                                   database = 'EPMMART_RW'){
  if(connect){
    scl_connect(user, database)
  }


  query <- paste0("DELETE FROM FPUSTATS.AVERAGE_RATES_FORECAST WHERE FORECAST_VINTAGE = ", forecast_vintage,
                  "AND EFF_DATE = TO_DATE('",eff_dt,"','YYYY-MM-DD')")

  RJDBC::dbSendUpdate(con, query)

  if(connect){
    RJDBC::dbDisconnect(con)
  }

  return(paste("Average rates forecast for vintage", forecast_vintage, "Uploaded on:", eff_dt,"has been deleted."))

}






