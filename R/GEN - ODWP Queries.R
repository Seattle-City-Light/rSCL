

#' ODWP Query that pulls system load between two dates
#'
#' @param start_date the start date of interest
#' @param end_date the end date of interest
#' @return Data frame of system load between the two dates after correcting for the hour ending issue
#' @export
scl_pull_system_load <- function(start_date = '2022-11-01', end_date = '2022-11-30'){

  `%>%` <- dplyr::`%>%`

  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)

  scl_connect('ODWP')

  load_dat <- DBI::dbGetQuery(con,
                             paste0("SELECT ACCU.B1, ACCU.B2, ACCU.B3, ACCU.ELEM, ACCU.INFO, ACCU.VALUE, ACCU.ARCHTIME
                                   FROM SCLBA.ACCU ACCU
                                   WHERE (ACCU.ARCHTIME BETWEEN TO_DATE('", start_date ,"', 'yyyy/mm/dd') AND TO_DATE('", end_date +lubridate::days(1) ,"', 'yyyy/mm/dd'))
                                   AND ACCU.B1='SCL' AND ACCU.B2='LOAD' AND ACCU.ELEM='AreaSum' AND ACCU.INFO='eacycle'"))

  load_dat$ARCHTIME <- load_dat$ARCHTIME - lubridate::hours(1)

  load_dat <- load_dat %>%
    dplyr::select(ARCHTIME,VALUE) %>%
    #mutate(DATETIME = ymd_hms(ARCHTIME)) %>% View(.)
    dplyr::rename(DATETIME = ARCHTIME,
           MWH = VALUE)

  load_dat <- load_dat %>%
    dplyr::filter(lubridate::date(DATETIME) >= start_date)

  return(load_dat)
}




