

#' This function writes properly formatted hourly rate class load to the fpustats database
#'
#' @param hourly_rate_class_data cleaned and adjusted hourly rate class load
#' @return Writes hourly data to FPUSTATS SYSTEM_ADJ_RC_LOAD_DATA table and returns print statement "Successfully uploaded data for ", year,'-',month"
#' @export
fpustats_write_system_adj_rate_class_load <- function(hourly_rate_class_data = data.frame()){

  `%>%` <- dplyr::`%>%`

  rate_codes <- scl_rate_code_descr(T)

  rate_codes <- trimws_custom(rate_codes)

  hourly_rate_class_data <- dplyr::left_join(hourly_rate_class_data,rate_codes)

  hourly_rate_class_data <- hourly_rate_class_data %>%
    dplyr::ungroup() %>%
    dplyr::select(DATETIME,RATE_CLASS,SERVICE_AREA, RATE_TYPE,FINAL_KWH) %>%
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
