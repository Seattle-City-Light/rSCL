

#' This function writes properly formatted hourly rate class load to the fpustats database
#'
#' @param start_date start date of interest
#' @param end_date end date of interest
#' @param read_all True if you want all available data
#' @param connect True if you want the function to handle the connection
#' @return returns hourly rate class load data
#' @export
fpustats_read_system_adj_rate_class_load <- function(start_date = '2024-01-01',
                                                     end_date = '2024-01-15',
                                                     read_all = F,
                                                     connect = T){


  if(connect){
    scl_connect('EPMMART_RW')
  }

  if(read_all){

    query <- "Select* from FPUSTATS.SYSTEM_ADJ_RC_LOAD_DATA"

  } else {

    query <- paste0("Select*
                    FROM FPUSTATS.SYSTEM_ADJ_RC_LOAD_DATA
                    WHERE TRUNC(DATETIME_PT) >= to_date('", start_date,"', 'YYYY-MM-DD')",
                    " AND TRUNC(DATETIME_PT) < to_date('", end_date,"', 'YYYY-MM-DD')")

  }

  rate_class_load <- RJDBC::dbGetQuery(con, query)

  if(connect){
    RJDBC::dbDisconnect(con)
  }

  return(rate_class_load)
}
