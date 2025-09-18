# -----------------------------------------------------------------------------------
# These functions provide the general connects in R sessions for common scl databases
# -----------------------------------------------------------------------------------





# defining location of user credential database
get_cred_path <- function(){

  user_cred_path <- "I:/FINANCE/FPU/RSCL/rscl/Connect User Credentials.csv"

  return(user_cred_path)

}






#' Print known rscl connect users
#'
#' @export
print_connect_users <- function(){

  creds <- read.csv(get_cred_path())

  user_list <- unique(creds$USER)

  print(user_list)

}






#' print known rscl databases
#'
#' @export
print_connect_databases <- function(){

  creds <- read.csv(get_cred_path())

  databases <- unique(creds$DATABASE)

  print(databases)

}





# get specific credentials for connection
get_cred <- function(user = 'MATTHEW', database = 'CCB'){

  creds <- read.csv(get_cred_path())

  creds <- creds %>%
    filter(USER==user) %>%
    filter(DATABASE== database)

  return(creds)

}





#' Generalized scl database connect function
#'
#' This function establishes R session connection to databases like
#' CCB, MSCS, ACCELA, and EPMMART
#'
#' @param user Name of user trying to connect to database
#' @param database Name of database user is trying to connect to
#' @return 0 if connecting to database directly or full credentials, including password for db like MSCS
#' @export
scl_connect <- function(user = 'MATTHEW', database = 'CCB') {

  if(database %in% c('ACCELA')){

    # no specific credentials needed for read only
    con <<- dbConnect(odbc(),
                      Driver = "SQL Server",
                      Server = "ITDWAEPDW100", # prod
                      Database = "SAAS_Reports",
                      UID = "readonly",
                      PWD = "readonly",
                      Port = 1433)

    return(0)

  } else if(database %in% c('MSCS')){

    creds <- get_cred(user,database)
    creds$PASSWORD <- key_get(creds$KEYRING_SERVICE, creds$KEYRING_USERNAME, creds$KEYRING_NAME)
    keyring_lock(keyring=creds$KEYRING_NAME)
    return(creds)

  } else {

    # reading in users keyring credentials
    creds <- get_cred(user,database)

    # specify Oracle driver and establish connection -- need to use super assignment to global env
    drv <<- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="N:/APPS/Oracle11g64/jdbc/lib/ojdbc6.jar")
    con <<- dbConnect(drv,
                      creds$SERVER,
                      creds$KEYRING_USERNAME,
                      password=key_get(creds$KEYRING_SERVICE, creds$KEYRING_USERNAME, creds$KEYRING_NAME))

    # lock keyring
    keyring_lock(keyring=creds$KEYRING_NAME)

    return(0)

  }

}





# remove credentials for a specific user and database
delete_user_cred <- function(user = 'MATTHEW', database = 'CCB'){

  creds <- read_csv(get_cred_path())

  creds <- creds %>%
    filter(!(USER==user & DATABASE==database))

  write_csv(creds, get_cred_path())

}





# add a new user credential for a specific database
add_user_cred <- function(user = 'MATTHEW',
                          database = 'CCB',
                          server = 'jdbc:oracle:thin:@//sclzcisdbprod100.light.ci.seattle.wa.us:1557/RPTPRD',
                          keyring_name = 'MH',
                          keyring_username = 'HAMLINM_RO',
                          keyring_service = 'CCB'){

  creds <- read_csv(get_cred_path())

  temp_data <- data.frame(USER = user,
                          DATABASE = database,
                          SERVER = server,
                          KEYRING_NAME = keyring_name,
                          KEYRING_USERNAME = keyring_username,
                          KEYRING_SERVICE = keyring_service)

  creds <- bind_rows(creds, temp_data) %>%
    arrange(USER)

  write_csv(creds, get_cred_path())

}





# update keyring password for a database
update_keyring <- function(keyring_name = "MH",
                           keyring_service = "CCB",
                           keyring_username = "HAMLINM_RO",
                           keyring_password = ""){


  key_set_with_value(service=kr_service, username=kr_username, password=kr_password, keyring=kr_name)

  keyring_lock(keyring_name)

}
