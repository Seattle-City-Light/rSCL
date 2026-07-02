



#' MSCS Query that maps badge number in CCB to d1_sp_id in MSCS and vice versa
#'
#' @param ids list of id's
#' @param id_type Name of the ID variable (BADGE_NBR or D1_SP_ID)
#' @return Data frame of all identifiers mapped together
#' @export
scl_pull_badge_to_d1sp_map <- function(ids = c('890903'),
                                   id_type = 'BADGE_NBR'){
  `%>%` <- dplyr::`%>%`

  mscs_check_valid_id_type(id_type)

  param <- scl_connect('MSCS')

  list_of_ids <- create_list_of_vectors_of_specific_size(ids,1000)

  map_list <- list()
  i <- 1
  print(paste0("Pulling ID map for ",length(list_of_ids),' bundle(s) of IDs.'))

  for(id_bundle in list_of_ids){

    if(id_type == 'BADGE_NBR'){

      query <-paste0("SELECT SP.D1_SP_ID,
                           DI.ID_VALUE as BADGE_NBR,
                          IE.D1_INSTALL_DTTM,
                          IE.D1_REMOVAL_DTTM
                    FROM  D1_SP SP,
                          D1_INSTALL_EVT IE,
                          D1_DVC_CFG DCFG,
                          D1_DVC D,
                          D1_DVC_IDENTIFIER DI
                    WHERE SP.D1_SP_ID=IE.D1_SP_ID
                    AND   IE.DEVICE_CONFIG_ID=DCFG.DEVICE_CONFIG_ID
                    AND   DCFG.D1_DEVICE_ID=D.D1_DEVICE_ID
                    AND   D.D1_DEVICE_ID=DI.D1_DEVICE_ID

                    AND   DI.DVC_ID_TYPE_FLG = 'D1BN'
                    AND   DI.ID_VALUE in (", to_sql_list(id_bundle),");")

      query <- stringr::str_squish(gsub('\n','', query))

      offset <- 0

      url_prod <- param$SERVER

      username <- param$KEYRING_USERNAME

      temp <- httr::POST(url_prod,
                         httr::authenticate(user=username,
                                      password=param$PASSWORD),
                         body = list(statementText = query, offset = offset) %>% jsonlite::toJSON(auto_unbox = T),
                         httr::content_type("application/json"))

      temp_dat <- jsonlite::fromJSON(rawToChar(temp$content))

      result_dat <- temp_dat$items$resultSet$items[[1]]

    }else if(id_type == 'D1_SP_ID'){

      query <-paste0("SELECT SP.D1_SP_ID,
                           DI.ID_VALUE as BADGE_NBR,
                          IE.D1_INSTALL_DTTM,
                          IE.D1_REMOVAL_DTTM
                    FROM  D1_SP SP,
                          D1_INSTALL_EVT IE,
                          D1_DVC_CFG DCFG,
                          D1_DVC D,
                          D1_DVC_IDENTIFIER DI
                    WHERE SP.D1_SP_ID=IE.D1_SP_ID
                    AND   IE.DEVICE_CONFIG_ID=DCFG.DEVICE_CONFIG_ID
                    AND   DCFG.D1_DEVICE_ID=D.D1_DEVICE_ID
                    AND   D.D1_DEVICE_ID=DI.D1_DEVICE_ID

                    AND   IE.D1_REMOVAL_DTTM IS NULL
                    AND   DI.DVC_ID_TYPE_FLG = 'D1BN'
                    AND   SP.D1_SP_ID in (", to_sql_list(id_bundle),");")

      query <- stringr::str_squish(gsub('\n','', query))

      offset <- 0

      url_prod <- param$SERVER

      username <- param$KEYRING_USERNAME

      temp <- httr::POST(url_prod,
                         httr::authenticate(user=username,
                                            password=param$PASSWORD),
                         body = list(statementText = query, offset = offset) %>% jsonlite::toJSON(auto_unbox = T),
                         httr::content_type("application/json"))

      temp_dat <- jsonlite::fromJSON(rawToChar(temp$content))

      result_dat <- temp_dat$items$resultSet$items[[1]]
    }

    map_list[[i]] <- result_dat

    print(i)

    i <- i + 1

  }

  id_map <- data.table::rbindlist(map_list, fill=T)

  colnames(id_map) <- toupper(colnames(id_map))

  return(id_map)
}





mscs_check_valid_id_type <- function(id_type = 'PREM_ID'){

  valid_id_types <- c('D1_SP_ID',
                      "BADGE_NBR")

  if(!id_type %in% valid_id_types){

    print('Please provide a valid id_type from this list:')
    print(valid_id_types)

    return()

  }

}





#' MSCS Query that maps badge number in CCB to d1_sp_id in MSCS and vice versa
#'
#' @param d1_sp_ids list of d1_sp_id's
#' @param start_date start date of period for hourly data
#' @param end_date end date of period for hourly data
#' @return Data frame of service point hourly data
#' @export
scl_pull_mscs_hourly_load <- function(d1_sp_ids = c('746791443786','950920972116'),
                                      start_date = '2025-01-01',
                                      end_date = '2025-01-08'){
  `%>%` <- dplyr::`%>%`

  param <- scl_connect('MSCS')

  list_of_ids <- create_list_of_vectors_of_specific_size(d1_sp_ids,1000)

  data_list <- list()
  i <- 1
  print(paste0("Pulling meter data for ",length(list_of_ids),' bundle(s) of IDs.'))

  for(id_bundle in list_of_ids){

    query <-paste0("select ie.D1_SP_ID,
            msrmt.measr_comp_id,
            msrcomp.measr_comp_type_cd,
            msrmt.msrmt_cond_flg,
            msrmt.msrmt_dttm,
            msrmt.msrmt_local_dttm,
            msrmt.msrmt_val
            from cisadm.D1_INSTALL_EVT ie,
            cisadm.D1_MEASR_COMP msrcomp,
            cisadm.D1_MSRMT msrmt
            where ie.D1_SP_ID in (", to_sql_list(id_bundle) ,")
            and ie.device_config_id = msrcomp.device_config_id
            and msrcomp.measr_comp_id = msrmt.measr_comp_id
            and msrcomp.measr_comp_type_cd like '%KWH%5%'
            and msrmt.msrmt_dttm BETWEEN TO_DATE('",start_date,"', 'yyyy/mm/dd') AND TO_DATE('",end_date,"', 'yyyy/mm/dd');")

    query <- stringr::str_squish(gsub('\n','', query))

    offset <- 0

    url_prod <- param$SERVER

    username <- param$KEYRING_USERNAME

    temp <- httr::POST(url_prod,
                       httr::authenticate(user=username,
                                          password=param$PASSWORD),
                       body = list(statementText = query, offset = offset) %>% jsonlite::toJSON(auto_unbox = T),
                       httr::content_type("application/json"))

    temp_dat <- jsonlite::fromJSON(rawToChar(temp$content))

    result_dat <- temp_dat$items$resultSet$items[[1]]

    has_more <- temp_dat$items$resultSet$hasMore

    if (has_more) {

      result_list <- list()
      counter <- 1
      offset <- offset + 10000

      result_list[[counter]] <- result_dat

      while (has_more) {

        temp <- httr::POST(url_prod,
                           httr::authenticate(user=username,
                                              password=param$PASSWORD),
                           body = list(statementText = query, offset = offset) %>% jsonlite::toJSON(auto_unbox = T),
                           httr::content_type("application/json"))

        temp_dat <- jsonlite::fromJSON(rawToChar(temp$content))

        result_dat2 <- temp_dat$items$resultSet$items[[1]]
        has_more <- temp_dat$items$resultSet$hasMore

        counter <- counter + 1
        offset <- offset + 10000

        result_list[[counter]] <- result_dat2

      }

      result_dat <- data.table::rbindlist(result_list, fill = T)
    }

    data_list[[i]] <- result_dat
    print(i)
    i <- i + 1
  }

  data <- data.table::rbindlist(data_list, fill = T)

  colnames(data) <- toupper(colnames(data))

  data <- data %>%
    dplyr::rename(DATETIME_PST = MSRMT_DTTM,
                  DATETIME_PT = MSRMT_LOCAL_DTTM,
                  KWH = MSRMT_VAL)

  return(data)
}
