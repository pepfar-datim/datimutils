d2Session <- R6::R6Class("d2Session",
                         #' @title d2Session
                         public = list(
                           #' @field  config_path Path to a JSON configuration
                           #' file.
                           config_path = NULL,
                           #' @field  base_url The URL of the server,
                           #' e.g. https://www.datim.org/.
                           base_url = NULL,
                           #' @field  username Your user name.
                           username = NULL,
                           #' @field user_orgunit UID of the users assigned
                           #' organisation unit
                           user_orgunit = NULL,
                           #' @field handle An httr handle used to communicate
                           #' with the DHIS2 instance.
                           handle = NULL,
                           #' @field me dhis2 api/me response
                           me  = NULL,
                           #' @field max_cache_age Maximum time responses should be cached
                           max_cache_age  = NULL,
                           #' @field token An httr OAUTH2 token
                           token = NULL,
                           #' @description
                           #' Create a new DHISLogin object
                           #' @param config_path Configuration file path
                           #' @param base_url URL to the server.
                           #' @param handle httr handle to be used for dhis2
                           #' connections
                           #' @param me DHIS2 me response object
                           #' @param token OAUTH2 token
                           initialize = function(config_path = NA_character_,
                                                 base_url,
                                                 handle,
                                                 me,
                                                 token) {
                             self$config_path <- config_path
                             self$me <- me
                             self$user_orgunit <- me$organisationUnits$id
                             self$base_url <- base_url
                             self$username <- me$userCredentials$username
                             self$handle <- handle
                             self$token <- token
                             }
                       )
)

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
  result <- try(keyring::key_list(keyring = ring), silent = TRUE)
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
#' @description logins into a datim or dhis2 api using either default keyring
#' and a config file. This function creates a d2Session login object in the
#' environment calling the login function.
#' E.g. global environment or R-shiny session. Thus you do not need to assign
#' the output of this function to a variable as it creates the variable/object
#' as a side effect.
#' @param config_path path to a dhis config file. If provided, username and
#'  password should not be provided.
#' @param config_path_level if there a multiple json entries in the config
#' file, it will default to dhis
#' @param username DHIS 2 username. If provided must provide password and
#' config_path must be NULL
#' @param password DHIS 2 password for the username. If provided must provide
#' password and config_path must be NULL
#' @param base_url if providing password and username directly this must be
#' non null
#' @param d2_session_name the variable name for the d2Session object.
#' The default name is d2_default_session and will be used by other datimutils
#' functions by default when connecting to datim. Generally a custom name
#' should only be needed if you need to log into two seperate DHIS2 instances
#' at the same time. If you create a d2Session object with a custom name then
#' this object must be passed to other datimutils functions explicitly
#' @param d2_session_envir the environment in which to place the R6 login
#' object, default is the immediate calling environment
loginToDATIM <- function(config_path = NULL,
                         config_path_level = "dhis",
                         username = NULL,
                         password = NULL,
                         base_url = NULL,
                         d2_session_name = "d2_default_session",
                         d2_session_envir = parent.frame()) {

  if ((!(is.null(username)) && is.null(password)) || (is.null(username) && !(is.null(password)))) {
    stop("If directly providing function credentials you must specify both username and password")
  }
  if ((!(is.null(config_path)) && !(is.null(password))) && !(is.null(username))) {
    stop("If using config_path then credentials can not be passed in directly")
  }
  if (!(is.null(password)) && !(is.null(username)) && is.null(base_url)) {
    stop("If directly passing password and username, base_url can't be null")
  }

  ## TODO modify to use username and password instead of config file if username and password are provided
  if (!(is.null(username)) && !(is.null(password))) {
    password <- password
    username <- username
    base_url <- base_url
  } else {

  # loads credentials from secret file
  credentials <- loadConfigFile(config_path = config_path)
  credentials <- credentials[[config_path_level]]
  password <- credentials[["password"]]
  if (is.null(password)) {
    password <- ""
  }
  # checks if password in file and if not checks keyring, and if not there
  # prompts to make one
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

  username <- credentials[["username"]]
    base_url <- credentials[["baseurl"]]
  }

  # form url
  url <- utils::URLencode(URL = paste0(base_url, "api", "/me"))
  handle <- httr::handle(base_url)
  # Logging in here will give us a cookie to reuse
  r <- httr::GET(
    url,
    httr::authenticate(username,
                       password),
    httr::timeout(60),
    handle = handle
  )

  if (r$status == 200L) {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))

    # create the session object in the calling environment of the login function
    assign(d2_session_name,
           d2Session$new(config_path = config_path,
                         base_url = base_url,
                         handle = handle,
                         me = me,
                         token = NULL ),
           envir = d2_session_envir)
  } else if (r$status == 302L) {
    stop("Unable to authenticate due to DATIM currently undergoing maintenance.
         Please try again later!")
  } else if (r$status == 503L) {
    stop("Unable to reach DATIM, the server may be experiencing issues.
         Please try again later!")
  } else if (r$status == 404L) {
    stop("Unable to authenticate due to an invalid URL.Please check the
         'base_url' parameter you provided.")
  } else if (r$status == 401L) {
    stop("Unable to authenticate due to an invalid username or password.
         Please update your credentials and try again.")
  } else {
    stop("An unknowon error has occured during authentication!")
  }
}



#' @title       datimutils::loginToDATIMOAuth(base_url =  Sys.getenv("BASE_URL"),
#' token = token,
#' app = oauth_app,
#' api = oauth_api,
#' redirect_uri= APP_URL,
#' scope = oauth_scope,
#' d2_session_envir = parent.env(environment()))
#'
#' @param base_url URL of the DHIS2 server
#' @param token An OAUTH2.0 token object. Will be created if not supplied.
#' @param redirect_uri The redirect URI which should be used after
#' successful authentication with the server.
#' @param app An httr OAUTH app object.
#' @param api An hjttr OAUTH endpoint.
#' @param scope A character vector of scopes which should be requested.
#' @param d2_session_name the variable name for the d2Session object.
#' The default name is d2_default_session and will be used by other datimutils
#' functions by default when connecting to datim. Generally a custom name
#' should only be needed if you need to log into two seperate DHIS2 instances
#' at the same time. If you create a d2Session object with a custom name then
#' this object must be passed to other datimutils functions explicitly
#' @param d2_session_envir the environment in which to place the R6 login
#' object, default is the immediate calling environment
#'
#' @export
#'

loginToDATIMOAuth <- function(
    base_url = NULL,
    token = NULL,
    redirect_uri = NULL,
    app = NULL,
    api = NULL,
    scope = NULL,
    d2_session_name = "d2_default_session",
    d2_session_envir = parent.frame()) {

  if (is.null(token)) {
    token <- httr::oauth2.0_token(
      app = app,
      endpoint = api,
      scope = scope,
      use_basic_auth = TRUE,
      oob_value = redirect_uri,
      cache = FALSE
    )
  } else {
    token <- token #For Shiny
  }

  # form url
  url <- utils::URLencode(URL = paste0(base_url, "api", "/me"))
  handle <- httr::handle(base_url)
  #Get Request
  r <- httr::GET(
    url,
    httr::config(token = token),
    httr::timeout(60),
    handle = handle
  )

  if (r$status_code != 200L) {
    stop("Could not authenticate you with the server!")
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    # create the session object in the calling environment of the login function
    assign(d2_session_name,
           d2Session$new(base_url = base_url,
                         handle = handle,
                         me = me,
                         token = token),
           envir = d2_session_envir)
  }
}
