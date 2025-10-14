# ---------------------------------------------------------------------------
# These functions are designed to manage keyring parameters for the various ways
# we connect to different databases
# ---------------------------------------------------------------------------






#' Print known key ring connections
#'
#' @export
print_keyring_connections <- function(){

  creds <- read.csv(system.file("extdata", "Connect User Credentials.csv", package = "rscl"))

  print(creds)

}






#' remove connection for a specific user and database
#'
#' @export
delete_keyring_connection <- function(user = 'MATTHEW', database = 'CCB'){

  #creds <- read.csv(get_cred_path())
  creds <- read.csv(system.file("extdata", "Connect User Credentials.csv", package = "rscl"))

  creds <- creds %>%
    filter(!(USER==user & DATABASE==database))

  write_csv(creds, system.file("extdata", "Connect User Credentials.csv", package = "rscl"))

}





#' add a new user credential for a specific database
#'
#' @export
add_keyring_connection <- function(user = 'MATTHEW',
                          database = 'CCB',
                          server = 'jdbc:oracle:thin:@//sclzcisdbprod100.light.ci.seattle.wa.us:1557/RPTPRD',
                          keyring_name = 'MH',
                          keyring_username = 'HAMLINM_RO',
                          keyring_service = 'CCB',
                          keyring_password = ""){

  creds <- read.csv(system.file("extdata", "Connect User Credentials.csv", package = "rscl"))

  temp_data <- data.frame(USER = user,
                          DATABASE = database,
                          SERVER = server,
                          KEYRING_NAME = keyring_name,
                          KEYRING_USERNAME = keyring_username,
                          KEYRING_SERVICE = keyring_service)

  creds <- bind_rows(creds, temp_data) %>%
    arrange(USER)

  write_csv(creds, system.file("extdata", "Connect User Credentials.csv", package = "rscl"))

  keyring::key_set_with_value(service=keyring_service,
                              username=keyring_username,
                              password=keyring_password,
                              keyring=keyring_name)

  keyring::keyring_lock(keyring_name)

}





#' update keyring password for a database
#'
#' @export
update_keyring_password <- function(user = 'MATTHEW', database = 'CCB', password = ''){

  creds <- read.csv(system.file("extdata", "Connect User Credentials.csv", package = "rscl"))

  creds <- creds %>%
    filter(USER==user & DATABASE == database)

  keyring::key_set_with_value(service=creds$KEYRING_SERVICE,
                     username=creds$KEYRING_USERNAME,
                     keyring=creds$KEYRING_NAME,
                     password=password)

  keyring::keyring_lock(keyring_name)

}





#' get specific credentials for connection
#'
#' @export
get_connect_creds <- function(user = 'MATTHEW', database = 'CCB'){

  #creds <- read.csv(get_cred_path())
  creds <- read.csv(system.file("extdata", "Connect User Credentials.csv", package = "rscl"))

  creds <- creds %>%
    filter(USER == user) %>%
    filter(DATABASE == database)

  return(creds)

}
