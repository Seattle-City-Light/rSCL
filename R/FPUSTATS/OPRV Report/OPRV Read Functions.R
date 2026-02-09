
#' This function reads oprv reports from the FPUSTATS Database
#'
#' @param year Year of the report of interest
#' @param month Month of the report of interest
#' @param most_eff_dt T/F Do you want the most recently uploaded version of the report(s)
#' @param read_all T/F Do you want all oprv reports or not
#' @param user Keyring Manager username
#' @param database Keyring Manager database name
#' @return Returns all oprv records with the given parameter arguments
#' @export
fpustats_read_oprv <- function(year = 2024,
                               month = 1,
                               most_eff_dt = T,
                               read_all = F,
                               user = 'MATTHEW',
                               database = 'EPMMART_RW'){

  `%>%` <- dplyr::`%>%`

  scl_connect(user, database)

  query <- "Select* From FPUSTATS.OPRV_REPORT"

  oprv_reports <- RJDBC::dbGetQuery(con, query)

  RJDBC::dbDisconnect(con)

  if(!read_all){

    oprv_reports <- oprv_reports %>%
      filter(YEAR==year) %>%
      filter(MONTH==month)

  }

  if(most_eff_dt){

    oprv_reports <- oprv_reports %>%
      arrange(desc(EFF_DT)) %>%
      group_by(YEAR, MONTH, CLASS, RATE_CODE_GRP,QTY_TYPE) %>%
      slice(1)

  }

  return(oprv_reports)

}




