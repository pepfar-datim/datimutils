#' @title LoadConfig(config_path)
#'
#' @description Loads a JSON configuration file to access a DHIS2 instance
#' @param config_path Path to the DHIS2 credentials file
#' @return A parsed list of the configuration file. 
#'
loadConfigFile <- function(config_path = NA) {
  #Load from a file
  if (!is.na(config_path)) {
    if (file.access(config_path, mode = 4) == -1) {
      stop(paste("Cannot read configuration located at",config_path))
    }
    
    dhis_config <- jsonlite::fromJSON(config_path)
    options("baseurl" = dhis_config$dhis$baseurl)
    options("config" = config_path)
    return(dhis_config)
  } else {
    stop("You must specify a credentials file!") }
}

#' @title makeKeyring(ring ="DatimLogin", service = getOption("baseurl"), username)
#'
#' @description makes a new keyRing with default name and default service
#' @param ring ring name
#' @param service baselink
#' @param username username 
#' @return none
#'
makeKeyring <- function (ring ="DatimLogin", service = getOption("baseurl"), username) 
{
keyring::keyring_create(ring)
keyring::keyring_unlock(ring)
keyring::key_set(service, username, keyring = ring)
}

#' @title getCredentialsFromKeyring(ring, service, username)
#'
#' @description retrieves username, service, and password from keyring
#' @param ring ring name
#' @param service baselink
#' @param username username 
#' @return a list containing entries called password, service, and username
#'
getCredentialsFromKeyring <- function(ring, service, username) 
{

  if (keyring::keyring_is_locked(ring)) {
    keyring::keyring_unlock(ring)
  }
  
  credentials = c( "password" = keyring::key_get(service,username),
                   keyring::key_list(service = service, keyring = ring))
  keyring::keyring_lock(ring)
  return(credentials)
}

#' @title apiLogin(keyring_username = NULL,config_path=NULL, 
#'config_path_default = "dhis", base_url = getOption("baseurl"), 
#'ring ="datimKeyring" )
#'
#' @description logins into a datim or dhis2 api using either a keyring or a config file
#' @param keyring_username the username of the keyring that will be created
#' @param config_path path to a dhis config file, keyring_username will bypass this option if not null
#' @param config_path_level if there a multiple json entries in the config file, it will default to dhis
#' @param base_url the service or base url to apend api calls
#' @param ring the name of the keyring to be created, default is datimKeyRing
#' @return a list containing entries called password, service, and username
#'
apiLogin<-function(keyring_username = NULL,config_path=NULL, 
                    config_path_level = "dhis", base_url = getOption("baseurl"), 
                    ring ="datimKeyring" ) {
  require(keyring)
  if(!is.null(config_path) & is.null(keyring_username) ){
    credentials = loadConfigFile(config_path = config_path)
    credentials = credentials[[config_path_level]]
  }else{
    result <- try(key_list(keyring = ring),silent = T)
    if("try-error" %in% class(result)){
      error_type <- attr(result,"condition")
      if(grepl("The specified keychain could not be found",error_type$message)){
        result <- try(makeKeyring(ring =ring, service = base_url, username = keyring_username), silent = T)
        if("try-error" %in% class(result)){
          error_type <- attr(result,"condition")
          if(grepl("The specified item could not be found in the keychain",error_type$message)){
            keyring::keyring_delete(keyring = ring)
            makeKeyring(ring =ring, service = base_url, username = keyring_username)
          }
        }
      }
    }
      credentials = getCredentialsFromKeyring(ring = ring, service = base_url, 
                                          username = keyring_username)
      if(nchar(credentials[["username"]] ) == 0 )
      {
        print("username empty for newkeyring, please type yes to delete and try again")
        keyring::keyring_delete(ring)
        stop("Must provide username for new Keyring, deleting bad keyring")
      }

  } 
  
  url <- URLencode(URL = paste0(base_url, "api","/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url ,
                 httr::authenticate(credentials[["username"]], credentials[["password"]]),
                 httr::timeout(60))
  rm(credentials)
  if(r$status != 200L){
    stop("Could not authenticate you with the server!")
  } else {
    me <- jsonlite::fromJSON(httr::content(r,as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return("Successfully logged into DATIM")
  }
}
