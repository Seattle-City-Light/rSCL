
#' This function writes a properly formatted oprv report to the FPUSTATS Database OPRV REPORT table
#'
#' @param report_path Directory of the formatted oprv report
#' @param user Keyring Manager username
#' @param database Keyring Manager database name
#' @return Writes report to FPUSTATS OPRV_REPORT table and returns print statement "Successfully uploaded report for", year,'-',month"
#' @export
fpustats_write_oprv_report <- function(report_path = 'I:/FINANCE/FPU/Sales and Revenue Actuals Model/Update Database Scripts/OPRV/OPRV Files/2025-05 OPRV.xlsx',
                                       user = 'MATTHEW',
                                       database = 'EPMMART_RW'){
  `%>%` <- dplyr::`%>%`

  oprv_report <- readxl::read_xlsx(report_path)

  data.table::setDT(oprv_report)

  oprv_report[is.na(oprv_report)] <- 0

  year <- as.numeric(substr(report_path, nchar(report_path) - 16, nchar(report_path) - 13))
  month <- as.numeric(substr(report_path, nchar(report_path) - 11, nchar(report_path) - 10))

  oprv_report$YEAR <- year
  oprv_report$MONTH <- month

  oprv_report <- oprv_report %>%
    dplyr::select(YEAR,MONTH,CLASS, RATE_CODE_GRP,QTY_TYPE,QTY_VALUE) %>%
    dplyr::mutate(EFF_DT = toupper(as.character(format(Sys.Date(),"%d-%b-%y"))))

  scl_connect(user, database)

  for(i in seq(1,nrow(oprv_report))){

    temp <- oprv_report[i,]
    temp$RATE_CODE_GRP <- paste0("'",temp$RATE_CODE_GRP,"'")
    temp$CLASS <- paste0("'",temp$CLASS,"'")
    temp$QTY_TYPE <- paste0("'",temp$QTY_TYPE,"'")
    temp$EFF_DT <- paste0("'",temp$EFF_DT,"'")

    query <- paste0("INSERT INTO FPUSTATS.OPRV_REPORT VALUES(",paste(temp, collapse = ', '),")")

    RJDBC::dbSendUpdate(con, query)

  }

  RJDBC::dbDisconnect(con)

  return(paste("Successfully uploaded report for", year,'-',month))
}









#' This function deletes oprv reports from the FPUSTATS Database
#'
#' @param year Year of the report of interest
#' @param month Month of the report of interest
#' @param eff_dt upload date of the specific report you want to delete
#' @param connect T/F if you want the function to do the connect or not (typically false if looping over multiple reports)
#' @param user Keyring Manager username
#' @param database Keyring Manager database name
#' @return Deletes specific record from database and returns the print statement "OPRV report for", year,'-',month,"Uploaded on:", eff_dt,"has been deleted."
#' @export
fpustats_delete_oprv_report <- function(year = 2025,
                                        month = 5,
                                        eff_dt = '2026-02-03',
                                        connect = T,
                                        user = 'MATTHEW',
                                        database = 'EPMMART_RW'){
  if(connect){
    scl_connect(user, database)
  }


  query <- paste0("DELETE FROM FPUSTATS.OPRV_REPORT WHERE YEAR = ", year,
                  "AND MONTH = ", month,
                  "AND EFF_DT = TO_DATE('",eff_dt,"','YYYY-MM-DD')")

  RJDBC::dbSendUpdate(con, query)

  if(connect){
    RJDBC::dbDisconnect(con)
  }

  return(paste("OPRV report for", year,'-',month,"Uploaded on:", eff_dt,"has been deleted."))

}






