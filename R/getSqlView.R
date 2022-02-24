#' @export
#' @title getSqlView
#' @description Runs specified sql view with specified sql view variables and returns
#' a tibble with the results. It is possible to specify the col_types when reading in the data
#' @param ... any filters to be used get put in here
#' @param sql_view_uid chr - the uid of the sql view
#' @param variable_keys character list - list of the variable names for the sql view
#' @param variable_values character list - list of the variable values ordered to correspond with
#' the related variable key
#' @param d2_session the d2Session object, default is "d2_default_session",
#' it will be made upon logging in to datim with loginToDATIM
#' @param retry number of times to retry
#' @param timeout number of seconds to wait during call
#' @return dataframe with the results of the sql view

getSqlView <- function(...,sql_view_uid, variable_keys = NULL, variable_values = NULL,
                       d2_session = dynGet("d2_default_session", inherits = TRUE),
                       retry=1, timeout = 180){

  assertthat::assert_that(length(variable_keys) == length(variable_values))

  variable_k_v_pairs <- NULL

  # format sql variable key value pairs for api call
  if(length(variable_keys) > 0){

     variable_k_v_pairs <- mapply(function(x,y) paste0("var=", x, ":", y),
                                  variable_keys, variable_values)
    variable_k_v_pairs <- paste0( "&",variable_k_v_pairs, collapse = "&")

  }

  if (missing(...)) {
    add <- NULL
  } else {
    # turn filters received as ... to a character vector of individual filters
    filters_chr <- unlist(list(...))
    add <- stringr::str_flatten(filters_chr, "filter=")
    add <- paste0("filter=", add)
  }

  path <-paste0("sqlViews/", sql_view_uid, "/data.json?paging=false",
                     variable_k_v_pairs, add)

   resp <- api_get(
    path = path, 
    d2_session = d2_session,
    retry = retry,
    timeout = timeout
  )

  headers <- resp$listGrid$headers$column

  resp <- as.data.frame(do.call("rbind",resp$listGrid$rows),stringsAsFactors = FALSE)

  colnames(resp) <- headers

  return(resp)
}

#?filter=orgunit_level:eq:2&filter=orgunit_name:ilike:bo
#?criteria=level:4


#' @export
#' @title listSqlViews
#' @param d2_session the d2Session object, default is "d2_default_session",
#' it will be made upon logging in to datim with loginToDATIM
#' @return dataframe with the list of sql views

listSqlViews <- function(d2_session = dynGet("d2_default_session", inherits = TRUE)){
  
   api_get(
    path = "sqlViews/", 
    d2_session = d2_session ) %>% 
    purrr::pluck('sqlViews')

}

