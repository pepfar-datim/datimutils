
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

isSimpleString <- function(x) {
  !(is.na(x) || x == "" || is.null(x)) & typeof(x) == "character"
}

if (!isSimpleString(namespace) || !isSimpleString(key)) {
  stop("Please specify the namespace and key!")
}
  url <- paste0(d2_session$base_url, "api/dataStore/", namespace, "/", key)

  # retry api get block, only retries if reponse code not in 400s
  i <- 1
  response_code <- 5

  while (i <= retry && (response_code < 400 || response_code >= 500)) {

    resp <- NULL
    resp <-
      try(#Is we are using an OAUTH token, we need to put the authorization code in the header.
        #Otherwise, just use the cookie.
        if (is.null(d2_session$token)) {
          httr::GET(url, httr::timeout(timeout),
                    handle = d2_session$handle)
        } else {
          httr::GET(
            url,
            httr::timeout(timeout),
            handle = d2_session$handle,
            httr::add_headers(
              Authorization = paste("Bearer", d2_session$token$credentials$access_token, sep = " ")
            )
          )
        })

    response_code <- httr::status_code(resp)
    Sys.sleep(i - 1)
    i <- i + 1
    if (response_code == 200 &&
        resp$url == url &&
        httr::http_type(resp) == "application/json") {
      break
    }
  }

    if (response_code == 404L) {
      warning("The requested datastore object could not be found")
      return(NULL)
    }

    jsonlite::parse_json(httr::content(resp, type = "text"), simplifyVector = simplify)
}
