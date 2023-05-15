isSimpleString <- function(x) {
  !(is.na(x) || x == "" || is.null(x)) & typeof(x) == "character"
}

.handleDataStoreResponse <- function(path, d2_session, retry, timeout) {
  resp <-
    tryCatch({
      api_get(
        path,
        d2_session = d2_session,
        retry = retry,
        timeout = timeout,
        verbose = TRUE
      )
    },
    error = function(e) {
      warning(paste(path, "could not be retreived from the server"))
      return(NULL)
    })

  if (inherits(resp, "list")) {
    jsonlite::parse_json(httr::content(resp$api_responses, type = "text"), simplifyVector = FALSE)
  } else {
    return(NULL)
  }
}
#' @title getDataStoreKey returns a particular key from the datastore
#' @description The DHIS2 datastore can be used to store arbitrary JSON objects.
#' @param namespace Namespace of the datastore
#' @param key  Key of the datastore
#' @param simplify Should the JSON response be simplified?
#' @param d2_session the d2Session object, default is "d2_default_session",
#' it will be made upon login in to DATIM with loginToDATIM
#' @param retry Number of times to retry the request
#' @param timeout Timeout in number of seconds
#'
#' @return A list if the request is successful. A data frame may be returned if
#' the simiplify paramater is set to TRUE, but it depends on the intrinsic structure
#' of the key itself.
#' @export
#'

getDataStoreKey <- function(namespace, key, simplify = FALSE,
                            d2_session = dynGet("d2_default_session", inherits = TRUE),
                            retry  = 3,
                            timeout = 60) {


if (!isSimpleString(namespace) || !isSimpleString(key)) {
  stop("Please specify the namespace and key!")
}
  path <- paste0("api/dataStore/", namespace, "/", key)

  .handleDataStoreResponse(path, d2_session, retry, timeout)
}



#' Title getDataStoreNamespaceKeys returns all keys for a given name space
#'
#' @param namespace The name of the namespace as a string
#' @param d2_session the d2Session object, default is "d2_default_session",
#' it will be made upon login in to DATIM with loginToDATIM
#' @param retry Number of times to retry the request
#' @param timeout Timeout in number of seconds
#' @return A character vector of strings contained in the namespace.
#'
#' @export
#'
getDataStoreNamespaceKeys <- function(namespace,
                                      d2_session = dynGet("d2_default_session", inherits = TRUE),
                                      retry  = 3,
                                      timeout = 60)  {

  if (!isSimpleString(namespace)) {
    stop("Please specify the namespace as a string!")
  }

  path <- paste0("api/dataStore/", namespace)

  .handleDataStoreResponse(path, d2_session, retry, timeout)
}

#' Title getDataStoreNamespaces returns a list of all namespaces present in the datastore
#'
#' @param d2_session the d2Session object, default is "d2_default_session",
#' it will be made upon login in to DATIM with loginToDATIM
#' @param retry Number of times to retry the request
#' @param timeout Timeout in number of seconds
#' @return A character vector of strings of the namespaces in the dataStore.
#'
#' @export
#'
getDataStoreNamespaces <- function(d2_session = dynGet("d2_default_session", inherits = TRUE),
                                      retry  = 3,
                                      timeout = 60)  {

  path <- paste0("api/dataStore.json")

  .handleDataStoreResponse(path, d2_session, retry, timeout)
}
