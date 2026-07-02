# ------------------------------------------------------------------------------
# This file contains useful functions that have general utility
# and not related to a specific purpose
# ------------------------------------------------------------------------------




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

  `:=` <- data.table::`:=`

  df <- data.table::as.data.table(df)
  cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
  if (length(cols_to_be_rectified) > 0) {
    # Call the data.table bracket function explicitly
    df <- data.table:::`[`(
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
