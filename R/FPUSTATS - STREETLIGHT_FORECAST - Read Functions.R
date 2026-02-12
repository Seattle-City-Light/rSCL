#' This function reads streetlight forecasts from the FPUSTATS Database
#'
#' @param forecast_vintage Which year's forecast are you interested in looking at
#' @param year Year within the forecast of interest
#' @param most_eff_dt T/F Do you want the most recently uploaded version of the forecast(s)
#' @param read_all T/F Do you want all forecasts or not
#' @param user Keyring Manager username
#' @param database Keyring Manager database name
#' @return Returns all streetlight records with the given parameter arguments
#' @export
fpustats_read_streetlight_forecast <- function(forecast_vintage = 2026,
                                               year = 2024,
                                               most_eff_dt = T,
                                               read_all = F,
                                               user = 'MATTHEW',
                                               database = 'EPMMART_RW'){

  `%>%` <- dplyr::`%>%`

  scl_connect(user, database)

  query <- "Select* From FPUSTATS.STREETLIGHT_FORECAST"

  sl_forecast <- RJDBC::dbGetQuery(con, query)

  RJDBC::dbDisconnect(con)

  if(!read_all){

    sl_forecast <- sl_forecast %>%
      dplyr::filter(FORECAST_VINTAGE==forecast_vintage) %>%
      dplyr::filter(YEAR==year)

  }

  if(most_eff_dt){

    sl_forecast <- sl_forecast %>%
      dplyr::arrange(desc(EFF_DT)) %>%
      dplyr::group_by(FORECAST_VINTAGE,YEAR, MONTH) %>%
      dplyr::slice(1)

  }

  return(sl_forecast)

}
