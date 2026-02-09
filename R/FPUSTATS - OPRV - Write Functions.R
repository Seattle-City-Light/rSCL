

fpustats_write_oprv_report <- function(report_path = 'I:/FINANCE/FPU/Sales and Revenue Actuals Model/Update Database Scripts/OPRV/OPRV Files/2025-05 OPRV.xlsx',
                                      scl_connect_user = 'MATTHEW',
                                      scl_connect_db = 'EPMMART_RW'){
  `%>%` <- dplyr::`%>%`
  
  oprv_report <- readxl::read_xlsx(report_path)
  
  data.table::setDT(oprv_report)
  
  oprv_report[is.na(oprv_report)] <- 0
  
  oprv_report$YEAR <- as.numeric(substr(report_path, nchar(report_path) - 16, nchar(report_path) - 13))
  oprv_report$MONTH <- as.numeric(substr(report_path, nchar(report_path) - 11, nchar(report_path) - 10))
  
  oprv_report <- oprv_report %>%
    dplyr::select(YEAR,MONTH,CLASS, RATE_CODE_GRP,QTY_TYPE,QTY_VALUE) %>%
    dplyr::mutate(EFF_DT = toupper(as.character(format(Sys.Date(),"%d-%b-%y"))))
  
  scl_connect(scl_connect_user, scl_connect_db)
  
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
  
}










fpustats_delete_oprv_report <- function(year = 2025, 
                                        month = 5, 
                                        eff_dt = '2026-02-03',
                                        connect = T,
                                        scl_connect_user = 'MATTHEW',
                                        scl_connect_db = 'EPMMART_RW'){
  if(connect){
    scl_connect(scl_connect_user, scl_connect_db)
  }
  

  query <- paste0("DELETE FROM FPUSTATS.OPRV_REPORT WHERE YEAR = ", year,
                  "AND MONTH = ", month,
                  "AND EFF_DT = TO_DATE('",eff_dt,"','YYYY-MM-DD')")
  
  RJDBC::dbSendUpdate(con, query)
  
  if(connect){
    RJDBC::dbDisconnect(con)
  }
  
}






