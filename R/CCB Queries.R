


#' CCB Query that maps most of a customers relevant data and identifiers
#'
#' After establishing a ccb connection using the scl_connect() function, this
#' function given badge numbers, service agreements or account ids will map these ids
#' each other and attach additional ids for service point, service agreement status
#' and rate code
#'
#' @param id list of customer id's
#' @param is_badge_num logical: True if the ids provided are badge numbers
#' @param is_sa logical: True if the ids provided are service agreements
#' @param is_acct_id logical: True if the ids provided are account numbers
#' @param most_recent_rs logical: True if only interested in the most recent rate code for the customers
#' @return Data frame of all identifiers mapped together
#' @export
ccb_customer_mapping <- function(id = c(),
                                 is_badge_num = F,
                                 is_sa = F,
                                 is_acct_id= F,
                                 most_recent_rs = T){

  id_list <- create_list_of_vectors_of_specific_size(id, 1000)
  list <- list()
  i <- 1
  print(paste0("Query done when counter gets to: ", length(id_list)))

  for(ids in id_list){

    print(i)

    if(is_badge_num ){

      list[[i]] <- dbGetQuery(con, paste0("SELECT mtr.BADGE_NBR,
                                                  hst.REMOVAL_DTTM,
                                                  mtr.RETIRE_DT,
                                                  mtr.SERIAL_NBR,
                                                  hst.SP_ID,
                                                  sa.SA_ID,
                                                  sa.SA_STATUS_FLG,
                                                  sa.ACCT_ID,
                                                  rs.RS_CD,
                                                  rs.EFFDT
                                     FROM CISADM.CI_MTR mtr,
                                          CISADM.CI_MTR_CONFIG cfg,
                                          CISADM.CI_SP_MTR_HIST hst,
                                          CISADM.CI_SA_SP sasp,
                                          CISADM.CI_SA sa,
                                          CISADM.CI_SA_RS_HIST rs
                                     where mtr.BADGE_NBR in (", to_sql_list(ids),")
                                     and mtr.MTR_ID = cfg.MTR_ID
                                     and cfg.MTR_CONFIG_ID = hst.MTR_CONFIG_ID
                                     and hst.SP_ID = sasp.SP_ID
                                     and sasp.SA_ID = sa.SA_ID
                                     and sa.SA_ID = rs.SA_ID ",
                                         if (most_recent_rs) {
                                           "and rs.EFFDT = (select MAX(EFFDT)
                                                            from CISADM.CI_SA_RS_HIST rs2
                                                            where rs.SA_ID = rs2.SA_ID)"
                                         }
      ))
    }
    else if(is_sa){

      list[[i]] <- dbGetQuery(con, paste0("SELECT mtr.BADGE_NBR,
                                                  hst.REMOVAL_DTTM,
                                                  mtr.RETIRE_DT,
                                                  mtr.SERIAL_NBR,
                                                  hst.SP_ID,
                                                  sa.SA_ID,
                                                  sa.SA_STATUS_FLG,
                                                  sa.ACCT_ID,
                                                  rs.RS_CD,
                                                  rs.EFFDT
                                     FROM CISADM.CI_MTR mtr,
                                          CISADM.CI_MTR_CONFIG cfg,
                                          CISADM.CI_SP_MTR_HIST hst,
                                          CISADM.CI_SA_SP sasp,
                                          CISADM.CI_SA sa,
                                          CISADM.CI_SA_RS_HIST rs
                                     where mtr.MTR_ID = cfg.MTR_ID
                                     and cfg.MTR_CONFIG_ID = hst.MTR_CONFIG_ID
                                     and hst.SP_ID = sasp.SP_ID
                                     and sasp.SA_ID = sa.SA_ID
                                     and sa.SA_ID in (", to_sql_list(ids),")
                                     and sa.SA_ID = rs.SA_ID ",
                                         if (most_recent_rs) {
                                           "and rs.EFFDT = (select MAX(EFFDT)
                                                            from CISADM.CI_SA_RS_HIST rs2
                                                            where rs.SA_ID = rs2.SA_ID)"
                                         }
      ))
    }
    else if(is_acct_id){

      list[[i]] <- dbGetQuery(con, paste0("SELECT mtr.BADGE_NBR,
                                                  hst.REMOVAL_DTTM,
                                                  mtr.RETIRE_DT,
                                                  mtr.SERIAL_NBR,
                                                  hst.SP_ID,
                                                  sa.SA_ID,
                                                  sa.SA_STATUS_FLG,
                                                  sa.ACCT_ID,
                                                  rs.RS_CD,
                                                  rs.EFFDT
                                     FROM CISADM.CI_MTR mtr,
                                          CISADM.CI_MTR_CONFIG cfg,
                                          CISADM.CI_SP_MTR_HIST hst,
                                          CISADM.CI_SA_SP sasp,
                                          CISADM.CI_SA sa,
                                          CISADM.CI_SA_RS_HIST rs
                                     where mtr.MTR_ID = cfg.MTR_ID
                                     and cfg.MTR_CONFIG_ID = hst.MTR_CONFIG_ID
                                     and hst.SP_ID = sasp.SP_ID
                                     and sasp.SA_ID = sa.SA_ID
                                     and sa.ACCT_ID in (", to_sql_list(ids),")
                                     and sa.SA_ID = rs.SA_ID ",
                                         if (most_recent_rs) {
                                           "and rs.EFFDT = (select MAX(EFFDT)
                                                            from CISADM.CI_SA_RS_HIST rs2
                                                            where rs.SA_ID = rs2.SA_ID)"
                                         })
      )
    }

    customer_map <- rbindlist(list)
    customer_map <- trimws_custom(customer_map)

    return(customer_map)

  }

}
