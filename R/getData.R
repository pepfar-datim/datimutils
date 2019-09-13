#' @export
#' @importFrom magrittr %>% 
#' @title FormatForApi_Dimensions(data, type_col, dim_id_col, item_id_col)
#' 
#' @description Uses specified columns in a data from to produce APIrequest 
#' formated dimensions e.g. &dimension=dim-id:dim-item;dim-item
#' Only includes unique dimension, dim-id, dim-item tupples 
#' @param data dataframe - containing parameters to incorporate into api call  
#' @param type_col string - name of column in data that specifies "dimension"
#' or "filter"
#' @param dim_id_col string - name of column in data that specifies 
#' dimension ids - including dx, ou, etc.
#' @param item_id_col string - name of column in data that specifies 
#' dimension item ids
#' @return  string ready for api call such as
#' "dimension=dim-id:dim-item;dim-item&filter=dim-id:dim-item;dim-item"
#' Note there is no leading "&" in string
#' @examples
#' df = tibble::tribble(~type, ~dim_id, ~item_id, ~other_col,
#' "dimension",    "LFsZ8v5v7rq", "CW81uF03hvV", 
#' "Implementing Partner: AIDSRelief Consortium",
#' "dimension",    "LFsZ8v5v7rq", "C6nZpLKjEJr", 
#' "Implementing Partner: African Medical and Research Foundation",
#' "filter", "dx", "BOSZApCrBni", "ART enrollment stage 1",
#' "filter", "dx", "dGdeotKpRed", "ART enrollment stage 2",
#' "dimension", "ou", "O6uvpzGd5pu", "Bo",
#' "filter", "pe", "THIS_FINANCIAL_YEAR","")
#' FormatForApi_Dimensions(df, "type", "dim_id", "item_id")
#'
FormatForApi_Dimensions <- function(data, type_col, dim_id_col, item_id_col){
  assertthat::assert_that(assertthat::has_name(data, type_col),
                          assertthat::has_name(data, dim_id_col),
                          assertthat::has_name(data, item_id_col))
  data %>% dplyr::mutate(type = data[[type_col]],
                         dim_id = data[[dim_id_col]],
                         item_id = data[[item_id_col]])  %>%
    dplyr::select(.data$type, .data$dim_id, .data$item_id) %>% unique() %>% 
    dplyr::group_by_at(c("type", "dim_id"))  %>%  
    dplyr::summarise(items = paste0(.data$item_id, collapse = ";")) %>% 
    dplyr::ungroup() %>% 
    dplyr::transmute(component = glue::glue("{type}={dim_id}:{items}")) %>% 
    .[[1]] %>% 
    paste0(collapse="&")
}

#' @export
#' @title getData_Analytics <-  function(dimensions, base_url)
#' 
#' @description calls the analytics endpoint using the details in the dimensions parameter
#' dataframe 
#' @param dimensions data frame - must contain columns named "type", "dim_uid", 
#' and "dim_item_uid". Type column contains "filter" or "dimension". dim_uid contains
#' the uid of a dimension or one of the special dimension types e.g. dx, pe, ou, co. 
#' Column dim_item_uid contains the uid of the dimension item to use which can also be
#' a "special" uid such as DE_GROUP-zhdJiWlPvCz  
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return data frame with the rows of the response
#'
#' @examples
#'  dimensions_sample <- tibble::tribble(~type, ~dim_item_uid, ~dim_uid,
#' "filter", "vihpFUg2WTy", "dx", #PMTCT positive test rate indicator
#' "dimension", "ImspTQPwCqd", "ou", # sierra leone
#' "dimension", "LEVEL-2", "ou", 
#' "filter", "LAST_YEAR", "pe",
#' "dimension", "UOqJW6HPvvL", "veGzholzPQm",
#' "dimension", "WAl0OCcIYxr", "veGzholzPQm",
#' "dimension", "uYxK4wmcPqA", "J5jldMd8OHv",
#' "dimension", "EYbopBOJWsW", "J5jldMd8OHv")
#' # veGzholzPQm = HIV age, UOqJW6HPvvL = 15-24y, WAl0OCcIYxr = 25-49y, 
#' # J5jldMd8OHv = Facility Type, uYxK4wmcPqA = CHP, EYbopBOJWsW = MCHP
#'   DHISLogin_Play()
#'   GetData_Analytics(dimensions_sample, "https://play.dhis2.org/2.30/")

GetData_Analytics <-  function(dimensions, 
                               base_url = getOption("baseurl")){
  api_call <- paste0(base_url,  
                     "api/30/analytics.json?",
                     FormatForApi_Dimensions(dimensions, "type", 
                                                              "dim_uid", "dim_item_uid"),
                     "&outputIdScheme=UID&hierarchyMeta=true") # gives us UIDs in response                  
  response <- api_call %>% 
    utils::URLencode()  %>%
    retryAPI("application/json", 20)
  
  content <- response %>% 
    httr::content(., "text") %>% 
    jsonlite::fromJSON()
  
  my_data <- content$rows
  if(length(dim(my_data)) != 2){ # empty table returned
    return(list(results = NULL, 
                api_call = response$url)
    )
  } 
  colnames(my_data) <- content$headers$column
  my_data <- tibble::as_tibble(my_data)
  
  # list column(vector) of the org hiearchy including the org unit itself
  # added to the data in a mutate below
  ou_hierarchy <- purrr::map_chr(my_data[["Organisation unit"]], 
                                 function(x) paste0(content$metaData$ouHierarchy[[x]], "/", x)) %>% 
    stringr::str_split("/")
  
  my_data <-
    dplyr::mutate(my_data, Value = as.numeric(.data$Value), ou_hierarchy = ou_hierarchy)
  return(list(results = my_data, 
              api_call = response$url)
  )
}