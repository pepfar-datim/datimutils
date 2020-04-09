#' @export
#' @title Execute and return a DATIM API query.
#' 
#' @description
#' Gets and flattens DATIM API query as dataframe.
#' 
#' @param path Should begin with api/ and contain the query
#' @param baseurl the url on which is added the path
#' @rety number of times to try in case of failure, default will not try again
#' @timeout how long should a reponse be waited for
#' @api_version defaults to current but can pass in version number
#' 
#' @return Result of DATIM API query returned as named list.
#' @usage  api_get(path = "api/me",  baseurl = "https://www.datim.org", retry =2, api_version = 29)
  
api_get <- function(path, baseurl = getOption("baseurl"), retry = 1, timeout = 60,
                    api_version = NULL ) {
  if(!(grepl("api",substr(path,1,4)))){
      path <- paste0("api/",path)
    }
  if(grepl("api",substr(path,1,4))){
  path <- sub( '(?<=.{4})', ifelse(is.null(api_version), "", paste0(api_version, "/")), 
               path, perl=TRUE )}
  url <- paste0(url = baseurl, path = path)
  if(grepl("?",url))
  {
    url <-sub("(.*)(\\?)", "\\1.json?paging=false\\2", url)
  }else{url <- paste0(url,".json?paging=false")}
  url <- gsub("//", "/",url)
  i = 1; response_code = 5
  while(i <= retry & response_code != 200 ){
    resp <- httr::GET(url, httr::timeout(timeout))
    response_code = httr::status_code(resp)
    i = i+1
  }
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json, are you logged into DATIM? If not please use loginToDatim function")}
  if (httr::status_code(resp) != 200) {
    stop(paste0("api query failed for url ", url))}
  resp = jsonlite::fromJSON(httr::content(resp,as = "text"), flatten = T)
  #  do.call(rbind.data.frame, me)
  return(resp)
}


