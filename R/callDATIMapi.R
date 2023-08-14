#' @title Execute and return a DATIM API query.
#' @description Gets and flattens DATIM API query as dataframe.
#' @param path Should begin with api/ and contain the query
#' @param d2_session the d2Session object, default is "d2_default_session",
#' it will be made upon logining in to datim with loginToDATIM
#' @param retry number of times to try in case of failure,
#' default will not try again
#' @param timeout how long should a reponse be waited for
#' @param api_version defaults to current but can pass in version number
#' @param verbose return raw content with data
#' @param quiet Echo the URL which is called to the console.
#' @return Result of DATIM API query returned as named list.
#'
api_get <- function(path,
                    d2_session,
                    retry = 1, timeout = 60,
                    api_version = NULL,
                    verbose = FALSE,
                    quiet = TRUE) {

  base_url <- d2_session$base_url
  handle <- d2_session$handle
  if (is.null(base_url)) {
    stop("You are not logged into DATIM")
  }
  # error if unsported file format desired
  if (grepl("\\.jsonp|\\.html|\\.xml|\\.pdf|\\.xls|\\.csv|\\.html\\+css|\\.adx", path) ||
      grepl("\\.jsonp|\\.html|\\.xml|\\.pdf|\\.xls|\\.csv|\\.html\\+css|\\.adx", base_url)) {
    stop("invalid file extension, either pass in a link with json or a link without a file format")
  }

  # make sure all "?" outside of the .json?paging=false are &'s
  path <- gsub("\\?", "&", path)
  path <- gsub("json&", "json?", path)


  # remove trailing / from path
  if (substr(path, nchar(path), nchar(path)) == "/") {
    path <- substr(path, 1, nchar(path) - 1)
  }

  # check if the word api in the path and if not add it
  if (!(grepl("api", substr(path, 1, 4)))) {
    path <- paste0("api/", path)
  }

  # if api_version is specified, add it in to the path
  path <- sub("(?<=.{4})", ifelse(is.null(api_version), "",
    paste0(api_version, "/")
  ),
  path,
  perl = TRUE
  )

  url <- paste0(url = base_url, path = path)

  # this if else block will add .json?paging=false where it is needed,
  # depending on the path
  if (!(grepl("json", url))) {
    if (grepl("&", url)) {
      url <- sub("(.*?)(&)", "\\1.json?paging=false\\2", url)
    } else {
      url <- paste0(url, ".json?paging=false")
    }
  }

  # this block adds paging=false in the case that only .json was passed in
  if (grepl("json", url) && !(grepl("paging", url))) {
    url <- sub(".json", ".json?paging=false", url)
  }

  # replaces /// with /
  url <- gsub("///", "/", url)

  # replaces all // with / unless it is the // in http://
  url <- gsub("[^http://]//", "/", url)

  # encodes url
  url <- utils::URLencode(url)

  # removes whitespace
  url <- gsub(" ", "", url)
  if (!quiet) {
    print(url)
  }

  # retry api get block, only retries if response code not in 400s
  i <- 1
  response_code <- 5

  while (i <= retry && (response_code < 400 || response_code >= 500)) {
    resp <- NULL
    resp <-
      try(
        #Is we are using an OAUTH token, we need to
        #put the authorization code in the header.
        #Otherwise, just use the cookie.
        if (is.null(d2_session$token)) {
          httr::GET(url, httr::timeout(timeout),
                    handle = handle)
        } else {
          httr::GET(url,
                    httr::timeout(timeout),
                    handle = handle,
                    httr::add_headers(Authorization =
                                        paste("Bearer",
                                              d2_session$token$credentials$access_token, sep = " ")))
        }

      )

    # try is added in order to handle if resp comes back as a "try-error" class
    response_code <- try(httr::status_code(resp), silent = TRUE)

    if (is(response_code, "try-error")) {
      message(
        paste0(
          "Api call to server failed on attempt ",
          i,
          " trying again ..."
        )
        )
    }

    Sys.sleep(i - 1)
    i <- i + 1

    if (response_code == 200L &&
        stringi::stri_replace(resp$url, regex = ".*/api/", replacement = "") ==
        stringi::stri_replace(url, regex = ".*/api/", replacement = "") &&
        httr::http_type(resp) == "application/json") {
      break
    }

  }

  # let user know that by the last attempt the api continues to return an error, this should break before status code
  # as a status code cannot be pulled from a failed api grab
  if (class(resp) == "try-error") {
    stop(
      paste0(
        "Server returned no response even on the last retry,
        this could a malformed link or a server issue, otherwise try again ",
        url
      )
    )
  }

  # unknown error catching which returns message and response code
  if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 500) {
    stop(paste0(
      "client error returned by url, this normally means a malformed link ", url,
      " response code: ", httr::status_code(resp)
    ))
  } else if (httr::status_code(resp) != 200) {
    stop(paste0(
      "api query failed for url ", url,
      " response code: ", httr::status_code(resp)
    ))
  }

  # if the response comes back in html and not json it means you landed on the
  # login page
  if (httr::http_type(resp) != "application/json") {
    stop(
      paste0("API did not return json, are you logged into DATIM?
         If not please use loginToDatim function \n url is "), url,
      "\n", "cookie is", httr::cookies(resp)
    )
  }

  # extract text response from api response
  content <- jsonlite::fromJSON(httr::content(resp, as = "text"),
    simplifyDataFrame = TRUE,
    flatten = TRUE
  )

  if (verbose) {
    return(list("data" = content, "api_responses" = resp))
  } else {
    return(content)
  }


}
