#' @export
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

#' @export
#' @title makeKeyring(ring ="DatimLogin", service = getOption("baseurl"), username)
#'
#' @description makes a new keyRing
#' @param ring keyring name
#' @param service baseurl
#' @param username username 
#' @return none
#' @details ENTER FIRST KEYCHAIN PASSWORD THEN SECRET
#'
makeKeyring <- function (ring ="DatimLogin", service = getOption("baseurl"), username) 
{

result <- try(keyring::key_list(keyring = ring),silent = T)
  if("try-error" %in% class(result)){
    error_type <- attr(result,"condition")
    if(grepl("The specified keychain could not be found",error_type$message)){
        print("enter KEYCHAIN password, then enter SECRET")
        keyring::keyring_create(ring)
        keyring::key_set(service, username, keyring = ring)
        keyring::keyring_lock(ring)
      }}else{print("keychain already exists")}  
}

#' @export
#' @title getCredentialsFromKeyring(ring, service, username)
#'
#' @description retrieves username, service, and password from keyring
#' @param ring keyring name
#' @return a list containing entries called password, baseurl, and username
#'
getCredentialsFromKeyring <- function(ring) 
{
  try <- as.list(keyring::key_list(keyring = ring))
  credentials = c( "password" = keyring::key_get(try[["service"]]), try)
  names(credentials) <- c("password", "baseurl", "username")
  keyring::keyring_lock(ring)
  return(credentials)
}

#' @export
#' @title loginToDATIMfunction(ring =NULL, config_path=NULL, config_path_level = "dhis" )
#' @description logins into a datim or dhis2 api using either a keyring or a config file
#' @param config_path path to a dhis config file, ring will bypass this option if not null
#' @param config_path_level if there a multiple json entries in the config file, it will default to dhis
#' @param ring the name of the keyring to be used
#'
loginToDATIM<-function(ring =NULL, config_path=NULL, config_path_level = "dhis" ) {
  
  if(!is.null(config_path) & is.null(ring) ){
    credentials = loadConfigFile(config_path = config_path)
    credentials = credentials[[config_path_level]]
  }else{credentials = getCredentialsFromKeyring(ring = ring)}

  url <- utils::URLencode(URL = paste0(credentials[["baseurl"]], "api","/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url ,
                 httr::authenticate(credentials[["username"]], credentials[["password"]]),
                 httr::timeout(60))
  if(r$status != 200L){
    stop("Could not authenticate you with the server!")
  } else {
    me <- jsonlite::fromJSON(httr::content(r,as = "text"))
    print(paste0( me$name, " is logged in"))
    options("organisationUnit" = me$organisationUnits$id)
  }
}
