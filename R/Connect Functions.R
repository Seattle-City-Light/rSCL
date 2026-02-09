# -----------------------------------------------------------------------------
# These functions provide the general connections for common scl databases
# -----------------------------------------------------------------------------






#' Generalized scl database connect function
#'
#' This function establishes R session connection to databases like
#' CCB, MSCS, ACCELA, and EPMMART
#'
#' @param user Name of user trying to connect to database
#' @param database Name of database user is trying to connect to
#' @return Prints if connection was successful or not also will return parameters for db like MSCS
#' @export
scl_connect <- function(user = 'MATTHEW', database = 'CCB') {

  if(database %in% c('ACCELA')){

    connect_accela()

  } else if(database %in% c('MSCS')){

    return(connect_mscs(user))

  } else {

    # reading in users keyring credentials
    creds <- get_connect_creds(user,database)

    # specify Oracle driver and establish connection -- need to use super assignment to global env
    drv <<- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver", classPath=creds$JDBC_DRIVER_PATH)
    con <<- DBI::dbConnect(drv,
                      creds$SERVER,
                      creds$KEYRING_USERNAME,
                      password= keyring::key_get(creds$KEYRING_SERVICE, creds$KEYRING_USERNAME, creds$KEYRING_NAME))

    # lock keyring
    keyring::keyring_lock(keyring=creds$KEYRING_NAME)

    return("Connect Successful")

  }

  return("No known connection with provided user and database.")

}





# General connection to the Accela database. Doesn't require a username or password.
connect_accela <- function(){

  con <<- DBI::dbConnect(odbc::odbc(),
                    Driver = "SQL Server",
                    Server = "ITDWAEPDW100", # prod
                    Database = "SAAS_Reports",
                    UID = "readonly",
                    PWD = "readonly",
                    Port = 1433)

  return("Connect Successful")

}





# gets the credentials for the mscs connect since this connection has to be done through a rest API
connect_mscs <- function(user = 'MATTHEW'){

  creds <- get_connect_creds(user,'MSCS')

  creds$PASSWORD <- keyring::key_get(creds$KEYRING_SERVICE, creds$KEYRING_USERNAME, creds$KEYRING_NAME)

  keyring::keyring_lock(keyring=creds$KEYRING_NAME)

  print("Connect Successful")

  return(creds)

}
