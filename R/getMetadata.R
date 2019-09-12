#' @title retryAPI(api_url, content_type, max_attempts)
#' 
#' @description Submits specified api request up to specified maximum times
#' stopping when expected content type is returned with 200 response
#' @param api_url string - full url for web request
#' @param content_type string - expected type of content in reposne e.d 'application/json'
#' @param max_attempts integer - maximum number of retries for succesful request
#' @param timeout integer - maximum time to wait for API response
#' @return  full api response when succesful
#'
retryAPI <- function(api_url, 
                     content_type, 
                     max_attempts = 3, 
                     timeout = 180){
  for(i in 1:max_attempts){
    try({
      response <- httr::GET(api_url, 
                            httr::timeout(timeout))
      if (response$status_code == 200L && 
          response$url == api_url && 
          httr::http_type(response) == content_type){
        return(response)
      }
      if (response$status_code >= 400 && 
          response$status_code < 500){ #client error
        break
      }
    })
    Sys.sleep(i/2 + 1)
  }
  # if i am here all my attempts failed
  stop(paste("Failed to obtain valid response in RetryAPI for:", api_url))
}

#' @importFrom magrittr %>% 
#' @importFrom rlang .data
#' @title formatForApi_filters
#' 
#' @description converts filters specified in a data frame to the format
#' expected by the api
#' @param metadata_filters dataframe - coulmns for property, operator, and value
#' components of a metadata filter (value can be a string vector for "in" operators)
#' @return filter string ready for API call

formatForApi_filters <- function(metadata_filters){ 
  assertthat::has_name(metadata_filters, "property")
  assertthat::has_name(metadata_filters, "operator")
  assertthat::has_name(metadata_filters, "value")
  
# concatenate sub components of each filter 
# special handling for operator "in"
# special handling for operator null
# concatenate all filters
  metadata_filters$value[is.na(metadata_filters$value)] <- ""
  metadata_filters$value[is.null(metadata_filters$value)] <- ""
  
   
    dplyr::mutate(metadata_filters, 
                  value = purrr::map_chr(.data$value, paste0, collapse = ",")) %>% 
    dplyr::mutate(value = dplyr::if_else(.data$operator %in% c("in", "!in"),
                                         paste0("[", .data$value, "]"),
                                         .data$value)) %>%
    dplyr::transmute(api_filters = paste0("&filter=",
                                          .data$property, ":",
                                          .data$operator,
                                          dplyr::if_else(.data$operator %in% c("null",
                                                                         "!null",
                                                                         "empty"),
                                                         "",
                                                         ":"),
                                          .data$value)) %>%
    .[["api_filters"]] %>%
    paste0(collapse = "")
  }

#' @export
#' @importFrom magrittr %>%
#' @title getMetadata
#' 
#' @description General utility to get metadata details from DATIM. 
#' Note API calls are limited to roughly 3000 characters, requests that generate
#' api calls in excess of these limits will produce an error. This function oes not try to 
#' split up the call. 
#' @param end_point string - api endpoint for the metadata of interest e.g. dataElements, 
#' organisationUnits
#' @param metadata_filters - list of strings - the parameters for  the DHIS2 metadata filter, 
#' e.g. c("id:eq:1234","name:in:Kenya,Rwanda")
#' @param fields - string for the fields to return structured as DHIS 2 expects,
#' e.g. "name,id,items[name,id]"
#' @param verbose returns the raw api response if TRUE
#' @param base_url string - base url for call e.g. "https://www.datim.org/"
#' defaults to the global option baseurl
#' @param api_version string - apit version for call e.g. "30"
#' @param max_attempts int - maximum number of times to retry the call if it fails 
#' @return list of metadata details
#' @examples datimutils::DHISLogin_Play()
#'          base_url = "https://play.dhis2.org/2.30/"
#'          metadata_filters <- tibble::tribble(~property, ~operator, ~value,
#'                                              "id", "in", c("lyLU2wR22tC","VTdjfLXXmoi"))
#'          datimutils::getMetadata("dataSets", 
#'                                  base_url = base_url)
#'          datimutils::getMetadata("dataSets",
#'                                  metadata_filters = metadata_filters, 
#'                                  fields = c("name", "id"), 
#'                                  base_url = base_url)
getMetadata <- function(end_point, 
                        metadata_filters = NULL, 
                        fields = NULL,
                        verbose = FALSE,
                        base_url = getOption("baseurl"),
                        api_version = "30",
                        max_attempts = 3){
  if (is.null(metadata_filters)) {
    api_filters = ""
  } else{
    api_filters = formatForApi_filters(metadata_filters)
  }
  
  if (is.null(fields)) {
    api_fields = ""
  } else{
    api_fields = paste0(fields, collapse = ",") %>% 
      {paste0("&fields=", .)}
  }
  
  
  api_call <- glue::glue("{base_url}api/{api_version}/{end_point}.json?paging=false{api_filters}{api_fields}") %>% 
    utils::URLencode()
  
  r <- api_call %>% retryAPI(content_type =  "application/json", 
                             max_attempts = max_attempts)
  
  if(verbose) return(r)
  
  httr::content(r, "text")   %>%
    jsonlite::fromJSON() #%>%
  #   rlist::list.extract(.,end_point) #} else {
  # #  stop("Could not retreive endpoint")
  # #}
  
}

# metadata_filters <- tibble::tribble(~property, ~operator, ~value,
#                 "id", "in", c("a4FRJ2P4cLf","tFBgL95CRtN"),
#                               "id", "in", c("a4FRJ2P4cLf","tFBgL95CRtN"),
#                 "id","null",NULL)
# 
# getMetadata("dataSets", metadata_filters)

getMetadata_id <- function(ids, 
                           end_point, 
                           fields, 
                           base_url = getOption("baseurl")){
  tibble::tribble()
  getMetadata(end_point = end_point,
              )
  
}

mapMetadata <- function(data, 
                        column,
                        end_point,
                        from, 
                        to,
                        base_url = "baseurl"){
  
}