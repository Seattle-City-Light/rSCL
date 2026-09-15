# ------------------------------------------------------------------------------
# This file contains useful functions that have general utility
# and not related to a specific purpose
# ------------------------------------------------------------------------------
.datatable.aware <- TRUE





#' @export
create_list_of_vectors_of_specific_size <- function(v = c(1,2,3,4,5,6,7), size = 2){

  list_of_vectors <- list()
  num_items <- ceiling(length(v)/size)

  for(i in seq(1:num_items)){

    if(i != num_items){
      list_of_vectors[[i]]= v[seq(size*(i-1)+1, size*i,1)]

    }else{
      list_of_vectors[[i]]=v[seq(size*(i-1)+1,length(v),1)]
    }
  }

  return(list_of_vectors)
}





#' @export
trimws_custom <- function(df) {

  df <- data.table::as.data.table(df)

  cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]

  if (length(cols_to_be_rectified) > 0) {
    # Call the explicit S3 data.table bracket function
    df <- data.table:::`[.data.table`(
      df,
      ,
      (cols_to_be_rectified) := lapply(.SD, trimws),
      .SDcols = cols_to_be_rectified
    )
  }

  return(df)
}





#' @export
to_sql_list <- function(x){

  paste0("'", as.vector(x), "'", collapse=", ")

}





#' @export
create_prem_type_cd_cat <- function(x){

  if(!'PREM_TYPE_CD' %in% colnames(x)){

    print('Please provide a data.frame with a colname of PREM_TYPE_CD')
    return()

  }

  sf <- c('SINFAM',
          'MOBILE',
          'HSEBT',
          'ADU',
          'DADU',
          'TWNHS',
          'TRIPLX',
          'DUPLEX',
          'QUADPX')

  mf <- c('APT',
          'CONDO',
          'BDGHSE',
          'COMBO')

  com <- c('PORT',
           'LOCMUN',
           'MUNI',
           'CHURCH',
           'PUBPLC',
           'SCHOOL',
           'FEDFAC',
           'UNIV',
           'HOTEL',
           'STATE',
           'COUNTY',
           'BUILPREM',
           'INDUST',
           'MEDICL',
           'METFAC',
           'BDPUBPLC',
           'COMMER',
           'SHA')

  x <- trimws_custom(x)

  data.table::setDT(x)
  x[PREM_TYPE_CD %in% sf,PREM_TYPE := 'SINGLE FAMILY']
  x[PREM_TYPE_CD %in% mf,PREM_TYPE := 'MULTI FAMILY']
  x[PREM_TYPE_CD %in% com,PREM_TYPE := 'COMMERCIAL']

  if(any(!x$PREM_TYPE_CD %in% c(mf,sf,com))){

    print(paste0("These prem types weren't categorized: ",
                 x[!unique(x$PREM_TYPE_CD) %in% c(mf,sf,com)]$PREM_TYPE_CD))

  }

  return(x)

}
