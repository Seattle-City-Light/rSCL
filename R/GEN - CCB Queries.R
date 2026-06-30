


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

  check_valid_id_type(id_type)

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
          and rs.EFFDT = (select MAX(EFFDT) from CISADM.CI_SA_RS_HIST rs2 where rs.SA_ID = rs2.SA_ID)"

    query <- paste0(q1,q2,q3)

    meta_list[[i]] <- DBI::dbGetQuery(con, query)

    i <- i + 1

  }

  meta <- rbindlist(meta_list, fill = T)

  return(meta)

}





check_valid_id_type <- function(id_type = 'PREM_ID'){

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




