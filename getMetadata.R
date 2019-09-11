#' @export
#' @title retryAPI(api_url, content_type, max_attempts)
#' 
#' @description Submits specified api request up to specified maximum times
#' stopping when expected content type is returned with 200 response
#' @param api_url string - full url for web request
#' @param content_type string - expected type of content in reposne e.d 'application/json'
#' @param max_attempts integer - maximum number of retries for succesful request
#' @param timeout integer - maximum time to wait for API response
#' @return  full api response
#'
retryAPI <- function(api_url, content_type, max_attempts = 3, timeout = 180){
  for(i in 1:max_attempts){
    try({
      response <- httr::GET(api_url, httr::timeout(timeout))
      if (response$status_code == 200L && 
          response$url == api_url && 
          httr::http_type(response) == content_type){
        return(response)
      }
    })
    Sys.sleep(i/2 + 1)
  }
  # if i am here all my attempts failed
  stop(paste("Failed to obtain valid response in RetryAPI for:", api_url))
}

formatForApi_filters <- functions(metadata_filters){
  assertthat::has_name(metadata_filters, "property")
  assertthat::has_name(metadata_filters, "operator")
  assertthat::has_name(metadata_filters, "value")
  
# if values are a list make csv
# concatenate each filter
# concatenate all filters
  dplyr::mutate(metadata_filters, 
                value = purrr::map_chr(value, paste0, collapse = ",")) %>%
    dplyr::mutate(value = dplyr::if_else(operator %in% c("in", "!in"),
                                         paste0("[", value, "]"), value)) %>% 
    dplyr::transmute(api_filters = paste0("&filter=",
                            property, ":",
                            operator, dplyr::if_else(operator %in% c("null", "!null"), "", ":"),
                            value)) %>% 
    .[["api_filters"]] %>% 
    paste0(collapse = "")
    
  
  purrr::map(metadata_filters, ~ print(.x[["property"]]))
             
             ~ paste0("&filter="
                                       .$property, ":"
                                       .$operator, ";",
                                       .$value)) %>% 
    dplyr::
  
  }

#' @export
#' @title getMetadata
#' 
#' @description General utility to get metadata details from DATIM
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param end_point string - api endpoint for the metadata of interest e.g. dataElements, 
#' organisationUnits
#' @param filters - list of strings - the parameters for  the DHIS2 metadata filter, 
#' e.g. c("id:eq:1234","name:in:Kenya,Rwanda")
#' @param fields - string for the fields to return structured as DHIS 2 expects,
#' e.g. "name,id,items[name,id]"
#' @return list of metadata details
getMetadata <- function(end_point, 
                        filters = NULL, 
                        fields = NULL,
                        verbose = FALSE,
                        base_url = getOption("baseurl")){
  #combine in lists
  
  url_filters <-  ""
  url_fields <-  ""
  
  if (!is.null(filters)) {
    url_filters <- filters %>% paste0("&filter=", ., collapse = "") %>% URLencode()
  }
  
  if (!is.null(fields)) {
    url_fields <- paste0("&fields=", paste(fields,sep="",collapse=",")) %>% URLencode()
  }
  
  web_api_call <- paste0(base_url, "api/", end_point, ".json?paging=false",
                         url_filters,
                         url_fields)
  r <- web_api_call %>% RetryAPI("application/json", 20)
  # httr::GET()
  assertthat::are_equal(r$status_code, 200L)
  #    if (r$status_code == 200L) {
  httr::content(r, "text")   %>%
    jsonlite::fromJSON() %>%
    rlist::list.extract(.,end_point) #} else {
  #  stop("Could not retreive endpoint")
  #}
}

metadata_filters <- tibble::tribble(~property, ~operator, ~value,
                "id", "in", c("a4FRJ2P4cLf","tFBgL95CRtN"),
                              "id", "in", c("a4FRJ2P4cLf","tFBgL95CRtN"),
                "id","null",NULL)

getMetadata("dataSets", filters, fields)