
#' This function writes a properly formatted Streetlight forecast to the FPUSTATS Database STREETLIGHT_FORECAST table
#'
#' @param forecast_path Directory of the formatted vintage forecast path
#' @param user Keyring Manager username
#' @param database Keyring Manager database name
#' @return Writes forecast to FPUSTATS STREETLIGHT_FORECAST table and returns print statement "Successfully uploaded forecast vintage", unique(sl_forecast$FORECAST_VINTAGE)
#' @export
fpustats_write_streetlight_forecast <- function(forecast_path = "I:/FINANCE/FPU/Sales and Revenue Actuals Model/Update Database Scripts/Streetlights/2026 Streetlight Forecast.xlsx",
                                                user = 'MATTHEW',
                                                database = 'EPMMART_RW'){
  `%>%` <- dplyr::`%>%`

  sl_forecast <- readxl::read_xlsx(forecast_path)

  data.table::setDT(sl_forecast)

  sl_forecast[is.na(sl_forecast)] <- 0

  sl_forecast <- sl_forecast %>%
    dplyr::select(FORECAST_VINTAGE, YEAR, MONTH, MWH) %>%
    dplyr::mutate(EFF_DT = toupper(as.character(format(Sys.Date(),"%d-%b-%y"))))

  scl_connect(user, database)

  for(i in seq(1,nrow(sl_forecast))){

    temp <- sl_forecast[i,]
    temp$EFF_DT <- paste0("'",temp$EFF_DT,"'")

    query <- paste0("INSERT INTO FPUSTATS.STREETLIGHT_FORECAST VALUES(",paste(temp, collapse = ', '),")")

    RJDBC::dbSendUpdate(con, query)

  }

  RJDBC::dbDisconnect(con)

  return(paste("Successfully uploaded forecast vintage", unique(sl_forecast$FORECAST_VINTAGE)))
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






