#' @title LoadConfig(config_path)
#'
#' @description Loads a JSON configuration file to access a DHIS2 instance
#' @param config_path Path to the DHIS2 credentials file
#' @return A parsed list of the configuration file.
#'
loadConfigFile <- function(config_path = NA) {
  # Load from a file
  if (!is.na(config_path)) {
    if (file.access(config_path, mode = 4) == -1) {
      stop(paste("Cannot read configuration located at", config_path))
    }
    dhis_config <- jsonlite::fromJSON(config_path)
    return(dhis_config)
  } else {
    stop("You must specify a credentials file!")
  }
}

#' @title makeKeyring(ring ="DatimLogin",
#' service = getOption("baseurl"), username)
#'
#' @description makes a new keyRing
#' @param username username
#' @param ring keyring name
#' @param service baseurl
#' @return none
#' @details ENTER FIRST KEYCHAIN PASSWORD THEN SECRET
#'
makeKeyring <- function(username,
                        ring = "DatimLogin",
                        service = getOption("baseurl")) {
  # checks if keyring exists and if it doesnt, it makes one and then locks it
  result <- try(keyring::key_list(keyring = ring), silent = T)
  if ("try-error" %in% class(result)) {
    error_type <- attr(result, "condition")
    if (grepl(
      "The specified keychain could not be found",
      error_type$message
    )) {
      print("enter KEYCHAIN password, then enter SECRET")
      keyring::keyring_create(ring)
      keyring::key_set(service, username, keyring = ring)
      keyring::keyring_lock(ring)
    }
  } else {
    print("keychain already exists")
  }
}

#' @title getCredentialsFromKeyring(ring, service, username)
#'
#' @description retrieves username, service, and password from keyring
#' @param ring keyring name
#' @return a list containing entries called password, baseurl, and username
#'
getCredentialsFromKeyring <- function(ring) {
  # returns credentials from a keyring
  try <- as.list(keyring::key_list(keyring = ring))
  credentials <- c("password" = keyring::key_get(try[["service"]]), try)
  names(credentials) <- c("password", "baseurl", "username")
  keyring::keyring_lock(ring)
  return(credentials)
}

#' @export
#' @title loginToDATIMfunction(config_path=NULL,
#' config_path_level = "dhis" )
#' @description logins into a datim or dhis2 api using either default keyring and a
#' config file
#' @param config_path path to a dhis config file
#' @param config_path_level if there a multiple json entries in the config
#' file, it will default to dhis
#'
loginToDATIM <- function(config_path = NULL,
                         config_path_level = "dhis",
                         username = NULL,
                         password = NULL) {

  ## TODO error if config path and username and/or password are provided
  ## error if username provided but no password and if password provided with no user
  ## TODO modify to use username and password instead of config file if username and password are provided
  
  
  
  # loads credentials from secret file
  credentials <- loadConfigFile(config_path = config_path)
  credentials <- credentials[[config_path_level]]
  password <- credentials[["password"]]
  if (is.null(password)) {
    password <- ""
  }
  # checks if password in file and if not checks keyring, and if not there prompts to make one
  if (nchar(password) == 0) {
    password <- try(keyring::key_get(
      service = credentials[["baseurl"]],
      username = credentials[["username"]]
    ))
    if ("try-error" %in% class(password)) {
      keyring::key_set(service = credentials[["baseurl"]], username = credentials[["username"]])
      password <- keyring::key_get(
        service = credentials[["baseurl"]],
        username = credentials[["username"]]
      )
    }
  }

  # form url
  url <- utils::URLencode(URL = paste0(credentials[["baseurl"]], "api", "/me"))
  # Logging in here will give us a cookie to reuse
  r <- httr::GET(
    url,
    httr::authenticate(credentials[["username"]], password),
    httr::timeout(60)
  )
  if (r$status != 200L) {
    stop("Could not authenticate you with the server!")
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    print(paste0(me$name, " is logged in"))
    options("organisationUnit" = me$organisationUnits$id)
  }
}
