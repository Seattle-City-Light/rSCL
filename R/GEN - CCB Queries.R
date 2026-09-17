


#' CCB Query that maps most of a customers relevant data and identifiers
#'
#' @param ids list of customer id's
#' @param id_type Name of the ID variable
#' @return Data frame of all identifiers mapped together
#' @export
scl_pull_customer_meter_meta <- function(ids = c('0614615463'),
                                         id_type = 'PREM_ID'){

  scl_connect('CCB')

  id_type <- toupper(id_type)

  ccb_check_valid_id_type(id_type)

  list_of_ids <- create_list_of_vectors_of_specific_size(ids,1000)

  meta_list <- list()
  i <- 1
  print(paste0("Pulling Meta Data for ",length(list_of_ids),' bundle(s) of IDs.'))

  for(id_bundle in list_of_ids){

    print(i)

    q1 <- "SELECT mtr.BADGE_NBR, hst.REMOVAL_DTTM, mtr.RETIRE_DT,
                 sp.SP_ID, sp.PREM_ID, sp.ABOLISH_DT,
                 sasp.STOP_DTTM,
                 sa.SA_ID, sa.SA_STATUS_FLG, sa.ACCT_ID,
                 rs.RS_CD, rs.EFFDT
           FROM CISADM.CI_MTR mtr,
                CISADM.CI_MTR_CONFIG cfg,
                CISADM.CI_SP_MTR_HIST hst,
                CISADM.CI_SP sp,
                CISADM.CI_SA_SP sasp,
                CISADM.CI_SA sa,
                CISADM.CI_SA_RS_HIST rs"


    if(id_type=='SA_ID'){
      q2 <- " Where sa.SA_ID in "
    } else if(id_type=='PREM_ID'){
      q2 <- " Where sp.PREM_ID in "
    } else if(id_type=='ACCT_ID'){
      q2 <- " Where sa.ACCT_ID in "
    } else if(id_type=='SP_ID'){
      q2 <- " Where sp.SP_ID in "
    } else if(id_type=='BADGE_NBR'){
      q2 <- " Where mtr.BADGE_NBR in "
    }

    ids <- paste0('(',paste0("'", as.vector(id_bundle), "'", collapse=", "),')')

    q2 <- paste0(q2, ids)

    q3 <- " and mtr.MTR_ID = cfg.MTR_ID
           and cfg.MTR_CONFIG_ID = hst.MTR_CONFIG_ID
           and hst.SP_ID = sp.SP_ID
           and hst.SP_ID = sasp.SP_ID
           and sasp.SA_ID = sa.SA_ID
           and sa.SA_ID = rs.SA_ID
           and rs.RS_CD like 'E%'
          and rs.EFFDT = (select MAX(EFFDT) from CISADM.CI_SA_RS_HIST rs2 where rs.SA_ID = rs2.SA_ID)"

    query <- paste0(q1,q2,q3)

    meta_list[[i]] <- DBI::dbGetQuery(con, query)

    i <- i + 1

  }

  meta <- data.table::rbindlist(meta_list, fill = T)

  meta <- trimws_custom(meta)

  return(meta)

}





ccb_check_valid_id_type <- function(id_type = 'PREM_ID'){

  valid_id_types <- c('SA_ID',
                      'PREM_ID',
                      'ACCT_ID',
                      'SP_ID',
                      "BADGE_NBR")

  if(!id_type %in% valid_id_types){

    print('Please provide a valid id_type from this list:')
    print(valid_id_types)

    return()

  }

}








#' CCB Query that maps rate codes to their full description
#'
#' @param connect True if you want the function to make the connection to CCB
#' @return Data frame with rate codes mapped to their full description
#' @export
scl_rate_code_descr <- function(connect = T){


  if(connect){
    scl_connect('CCB')
  }

  rs_l <- DBI::dbGetQuery(con,"Select RS_CD, DESCR
                          From CISADM.CI_RS_L
                          Where RS_CD like 'E%'")

  rs_l$DESCR <- toupper(rs_l$DESCR)
  rs_l$DESCR2 <- rs_l$DESCR

  data.table::setDT(rs_l)
  rs_l[,DESCR := gsub('SCL ','', DESCR)]
  rs_l[,DESCR := gsub('ELECTRIC ','', DESCR)]
  rs_l[grepl('RESIDENTIAL', DESCR) ,CLASS := 'RESIDENTIAL'][is.na(CLASS),CLASS := 'COMMERCIAL'][,DESCR := gsub('RESIDENTIAL: ','', DESCR)]
  rs_l[CLASS == 'RESIDENTIAL', RATE_CLASS := 'RESIDENTIAL'][grepl('SMALL', DESCR), RATE_CLASS := 'SMALL'][grepl('MEDIUM', DESCR), RATE_CLASS := 'MEDIUM'][grepl('LARGE', DESCR), RATE_CLASS := 'LARGE'][grepl('HIGH DEMAND', DESCR), RATE_CLASS := 'HIGH DEMAND']

  rs_l[,DESCR := gsub('GENERAL SERVICE: ','', DESCR)]
  rs_l[,DESCR := gsub('MEDIUM ','', DESCR)]
  rs_l[,DESCR := gsub('SMALL ','', DESCR)]
  rs_l[,DESCR := gsub('LARGE ','', DESCR)]
  rs_l[,DESCR := gsub('HIGH DEMAND ','', DESCR)]

  rs_l[grepl('STREETLIGHTS', DESCR), RATE_CLASS := 'STREETLIGHTS']
  rs_l[grepl('FLAT', DESCR), RATE_CLASS := 'FLAT']
  rs_l[grepl('PRODUCTION', DESCR), RATE_CLASS := 'PRODUCTION']
  rs_l[grepl('SELF CONSUMING', DESCR), RATE_CLASS := 'SELF CONSUMING']

  rs_l[,DESCR := gsub('STREETLIGHTS ','', DESCR)][,DESCR := gsub('FLAT ','', DESCR)][,DESCR := gsub('PRODUCTION ','', DESCR)][,DESCR := gsub('SELF CONSUMING ','', DESCR)]

  rs_l[,DESCR := gsub('STANDARD GENERAL SERVICE ','', DESCR)][,DESCR := gsub('STANDARD ','', DESCR)]

  rs_l[grepl('RECEIVED', DESCR), SALE_TYPE := 'RECEIVED'][is.na(SALE_TYPE), SALE_TYPE := 'DELIVERED']

  rs_l[,DESCR := gsub('(RECEIVED): ','', DESCR)][,DESCR := gsub('(RECEIVED):','', DESCR)]

  rs_l[grepl('NON-RESIDENTIAL', DESCR), CLASS := 'COMMERCIAL'][grepl('GREEN UP', DESCR), RATE_CLASS := 'GREEN UP']

  rs_l[,DESCR := gsub('NON-RESIDENTIAL ','', DESCR)][,DESCR := gsub('RESIDENTIAL ','', DESCR)][,DESCR := gsub('- GREEN UP - ','', DESCR)]

  rs_l[RATE_CLASS =='PRODUCTION', SALE_TYPE := 'DELIVERED/RECEIVED']

  rs_l[grepl('BIMONTHLY|BI-MONTHLY', DESCR), BILL_FREQ := 'BIMONTHLY'][is.na(BILL_FREQ), BILL_FREQ:='MONTHLY']

  rs_l[,DESCR := gsub('BIMONTHLY','', DESCR)][,DESCR := gsub('BI-MONTHLY','', DESCR)][,DESCR := gsub('MONTHLY','', DESCR)]

  rs_l[grepl('PILOT', DESCR), PROGRAM := 'PILOT'][grepl('COMMERCIAL CHARGING', DESCR), PROGRAM := 'COMMERCIAL CHARGING'][is.na(PROGRAM), PROGRAM:='STANDARD']

  rs_l[,DESCR := gsub('INTERVAL','', DESCR)][,DESCR := gsub('INTEVAL','', DESCR)][,DESCR := gsub('PILOT','', DESCR)][,DESCR := gsub('COMMERCIAL CHARGING','', DESCR)]

  rs_l[grepl('TOU', DESCR), RATE_TYPE := 'TOU'][is.na(RATE_TYPE), RATE_TYPE:='STANDARD']

  rs_l[,DESCR := gsub('TOU','', DESCR)]

  rs_l[grepl('BURIEN', DESCR), SERVICE_AREA := 'BURIEN']
  rs_l[grepl('CITY', DESCR), SERVICE_AREA := 'CITY']
  rs_l[grepl('KING COUNTY', DESCR), SERVICE_AREA := 'KING COUNTY']
  rs_l[grepl('LAKE FOREST PARK|LFP', DESCR), SERVICE_AREA := 'LAKE FOREST PARK']
  rs_l[grepl('NETWORK', DESCR), SERVICE_AREA := 'NETWORK']
  rs_l[grepl('NORMANDY PARK', DESCR), SERVICE_AREA := 'NORMANDY PARK']
  rs_l[grepl('SEATAC', DESCR), SERVICE_AREA := 'SEATAC']
  rs_l[grepl('SHORELINE', DESCR), SERVICE_AREA := 'SHORELINE']
  rs_l[grepl('SUBURBAN|SUBRBAN', DESCR), SERVICE_AREA := 'SUBURBAN']
  rs_l[grepl('TUKWILA', DESCR), SERVICE_AREA := 'TUKWILA']

  rs_l[,DESCR := gsub('BURIEN','', DESCR)]
  rs_l[,DESCR := gsub('CITY','', DESCR)]
  rs_l[,DESCR := gsub('KING COUNTY','', DESCR)]
  rs_l[,DESCR := gsub('LAKE FOREST PARK','', DESCR)]
  rs_l[,DESCR := gsub('LFP','', DESCR)]
  rs_l[,DESCR := gsub('NETWORK','', DESCR)]
  rs_l[,DESCR := gsub('NORMANDY PARK','', DESCR)]
  rs_l[,DESCR := gsub('SEATAC','', DESCR)]
  rs_l[,DESCR := gsub('SHORELINE','', DESCR)]
  rs_l[,DESCR := gsub('SUBURBAN','', DESCR)]
  rs_l[,DESCR := gsub('SUBRBAN','', DESCR)]
  rs_l[,DESCR := gsub('TUKWILA','', DESCR)]

  rs_l[RS_CD =='EFLTRB  ', BILL_FREQ := 'BIMONTHLY']
  rs_l[RS_CD =='ERLIB   ', BILL_FREQ := 'BIMONTHLY']

  rs_l[RATE_CLASS == 'GREEN UP', PROGRAM := 'GREEN UP'][RATE_CLASS == 'GREEN UP', SERVICE_AREA := 'GREEN UP']
  rs_l[RATE_CLASS == 'STREETLIGHTS', SERVICE_AREA := 'STREETLIGHTS'][RATE_CLASS == 'FLAT', SERVICE_AREA := 'FLAT'][RATE_CLASS == 'PRODUCTION', SERVICE_AREA := 'PRODUCTION'][RATE_CLASS == 'SELF CONSUMING', SERVICE_AREA := 'SELF CONSUMING']

  rs_l$DESCR <- rs_l$DESCR2
  rs_l$DESCR2 <- NULL

  `%>%` <- dplyr::`%>%`

  rs_l <- rs_l %>%
    dplyr::select(RS_CD, DESCR, CLASS, RATE_CLASS, RATE_TYPE, SERVICE_AREA, BILL_FREQ, PROGRAM, SALE_TYPE) %>%
    dplyr::rename(RS_DESCR = DESCR)

  trimws_custom(rs_l)

  return(rs_l)

}
