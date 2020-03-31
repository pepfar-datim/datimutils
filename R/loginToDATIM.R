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

makeKeyring <- function (ring ="DatimLogin", service = getOption("baseurl"), username) 
{
keyring::keyring_create(ring)
keyring::keyring_unlock(ring)
keyring::key_set(service, username, keyring = ring)
}

getCredentialsFromKeyring <- function (ring, service, username) 
{

  if (keyring::keyring_is_locked(ring)) {
    keyring::keyring_unlock(ring)
  }
  
  credentials = c( "password" = keyring::key_get(service,username),
                   keyring::key_list(service = service, keyring = ring))
  keyring::keyring_lock(ring)
  return(credentials)
}

apiLogin<-function(keyring_username = NULL,config_path=NULL, 
                    config_path_default = "dhis", base_url = getOption("baseurl"), 
                    ring ="datimKeyring" ) {
  
  if(!is.null(config_path) & is.null(keyring_username) ){
    credentials = loadConfigFile(config_path = config_path)
    credentials = credentials[[config_path_default]]
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
  } 
  
  url <- URLencode(URL = paste0(getOption("baseurl"), "api","/me"))
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

apiLogin(keyring_username = NULL,config_path=NULL, 
                    config_path_default = "dhis", base_url = "https://www.datim.org/", 
                    ring ="datimKey8" )
#make documentation and check if it is compatible with cran

