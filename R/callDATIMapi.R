#' @title Execute and return a DATIM API query.
#' @description Gets and flattens DATIM API query as dataframe.
#' @param path Should begin with api/ and contain the query
#' @param base_url the url on which is added the path
#' @param retry number of times to try in case of failure,
#' default will not try again
#' @param timeout how long should a reponse be waited for
#' @param api_version defaults to current but can pass in version number
#' @param wrapper_reduce indicator passed in by wrappers to reduce list to data.frame
#' @param expand dataframe to know how to expand result in case of duplicate filters
#' @return Result of DATIM API query returned as named list.
#'
api_get <- function(path, base_url = getOption("baseurl"),
                    retry = 1, timeout = 60,
                    api_version = NULL, wrapper_reduce = NULL, expand = NULL) {
  #error if unsported file format desired
  if(grepl(".jsonp|.html|.xml|.pdf|.xls|.csv|.html+css|.adx", path )
     |grepl(".jsonp|.html|.xml|.pdf|.xls|.csv|.html+css|.adx", base_url))
  {
    stop("invalid file extension, either pass in a link with json or a link without a file format")
  }
  #make sure all "?" outside of the .json?paging=false are &'s
  path <- gsub("\\?","&", path)
  path <- gsub("json&","json?", path)
  #remove trailing / from path
  if(substr(path, nchar(path), nchar(path)) == "/")
  {
    path <- substr(path, 1, nchar(path)-1)
  }
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
  if(!(grepl("json", url)))
       {
    if (grepl("&", url)) {
    url <- sub("(.*?)(&)", "\\1.json?paging=false\\2", url)
  } else {
    url <- paste0(url, ".json?paging=false")
    }
  }
  #this block adds pagin=false in the case that only .json was passed in
  if(grepl("json", url) & !(grepl("paging", url)))
  {
  url <- sub(".json", ".json?paging=false", url)
  }
  #replaces /// with /
  url <- gsub("///", "/", url)
  #replaces all // with / unless it is the // in http://
  url <- gsub("[^http://]//", "/", url)
  #retry api get block, only retries if reponse code not in 400s
  i <- 1; response_code <- 5
  while (i <= retry & (response_code < 400 | response_code >= 500 )) {
    resp <- httr::GET(url, httr::timeout(timeout))
    response_code <- httr::status_code(resp)
    i <- i + 1
  }

#unknown error catching which returns message and response code
  if (httr::status_code(resp) >= 400 & httr::status_code(resp) <= 500 ) {
    stop(paste0("client error returned by url, this normally means a malformed link", url,
                " response code: ", httr::status_code(resp) ))
  } else if (httr::status_code(resp) != 200) {
    stop(paste0("api query failed for url ", url ,
                " response code: ", httr::status_code(resp)))
  }

#if the response comes back in html and not json it means you landed on the login page
  if (httr::http_type(resp) != "application/json") {
    stop(
      paste0("API did not return json, are you logged into DATIM?
         If not please use loginToDatim function \n url is "), url,
      "\n", "cookie is", httr::cookies(resp)
    )
  }
  resp <- jsonlite::fromJSON(httr::content(resp, as = "text"), flatten = T)

  #if a wrapper is used here it will pass df and not list
  if(!(is.null(wrapper_reduce))){
  resp <- resp[[wrapper_reduce]]}
  print(url)
  
  #this will add the duplicates to the dataframe if duplicates were in the filter
  if(!(is.null(expand))){
  resp <- resp[match(expand$x, resp[,1]), ,drop = F]
  expand <- expand[expand$x %in% resp[,1],]
  bindlist <- list()
  for(i in 1:nrow(expand))
  {
    bindlist[[i]] <- rep(resp[i,],expand[expand$x == resp[i,1], "Freq"]-1)

  }
  bind <- as.data.frame(c(do.call("rbind", bindlist)))
  colnames(bind) <- colnames(resp)
  resp <- rbind(resp,bind)
  }
  
  return(resp)
}




