#' @export
#' @title Execute and return a DATIM API query.
#' @description Gets and flattens DATIM API query as dataframe.
#' @param path Should begin with api/ and contain the query
#' @param base_url the url on which is added the path
#' @param retry number of times to try in case of failure,
#' default will not try again
#' @param timeout how long should a reponse be waited for
#' @param api_version defaults to current but can pass in version number
#' @return Result of DATIM API query returned as named list.
#'
api_get <- function(path, base_url = getOption("baseurl"),
                    retry = 1, timeout = 60,
                    api_version = NULL) {
  #check if the word api in the path and if not add it
  if (!(grepl("api", substr(path, 1, 4)))) {
    path <- paste0("api/", path)
  }
  #if api_version is specified, add it in to the path
    path <- sub("(?<=.{4})", ifelse(is.null(api_version), "",
      paste0(api_version, "/")
    ),
    path,
    perl = TRUE
    )
  url <- paste0(url = base_url, path = path)
  #this if else block will add .json?paging=false where it is needed, depending on the path
  if (grepl("\\/\\?", url)) {
    url <- sub("(.*?)(\\//?)", "\\1.json?paging=false\\2", url)
  } else if (grepl("\\?", url)) {
    url <- sub("(.*?)(\\?)", "\\1.json?paging=false\\2", url)
  } else {
    url <- paste0(url, ".json?paging=false")
  }
  #replaces /// with /
  url <- gsub("///", "/", url)
  #replaces all // with / unless it is the // in http://
  gsub("[^http://]//", "/", url)
  #retry api get block
  i <- 1
  response_code <- 5
  while (i <= retry & response_code != 200) {
    resp <- httr::GET(url, httr::timeout(timeout))
    response_code <- httr::status_code(resp)
    i <- i + 1
  }
  #if the response comes back in html and not json it means you landed on the login page
  if (httr::http_type(resp) != "application/json") {
    stop(
      paste0("API did not return json, are you logged into DATIM?
         If not please use loginToDatim function \n url is "), url,
      "\n", "cookie is", httr::cookies(resp)
    )
  }
  #unknown error catching
  if (httr::status_code(resp) != 200) {
    stop(paste0("api query failed for url ", url))
  }
  resp <- jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = T)
  return(resp)
}
