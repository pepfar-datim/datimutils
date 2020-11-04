#' @export
#' @title getSqlView
#' @description Runs specified sql view with specified sql view variables and returns
#' a tibble with the results. It is possible to specify the col_types when reading in the data
#' @param ... any filters to be used get put in here
#' @param sql_view_uid chr - the uid of the sql view
#' @param variable_keys character list - list of the variable names for the sql view
#' @param variable_values character list - list of the variable values ordered to correspond with
#' the related variable key
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param retry number of times to retry
#' @param timeout number of seconds to wait during call
#' @return dataframe with the results of the sql view

getSqlView <- function(...,sql_view_uid, variable_keys = NULL, variable_values = NULL,
                       base_url = getOption("baseurl"), retry=1, timeout = 180){

  assertthat::assert_that(length(variable_keys) == length(variable_values))

  variable_k_v_pairs <- NULL

  # format sql variable key value pairs for api call
  if(length(variable_keys) > 0){

     variable_k_v_pairs <- mapply(function(x,y) paste0("var=", x, ":", y),
                                  variable_keys, variable_values)
    variable_k_v_pairs <- paste0("&", variable_k_v_pairs, collapse = "&")

  }

  if (missing(...)) {
    add <- NULL
  } else {
    # turn filters recieved as ... to a character vector of individual filters
    filters_chr <- unlist(list(...))
    add <- stringr::str_flatten(filters_chr, "&filter=")
    add <- paste0("&filter=", add)
  }

  path <- paste0("sqlViews/", sql_view_uid, "/data.json",
                     variable_k_v_pairs, add)

   resp <- api_get(
    path = path, base_url = base_url, retry = retry,
    timeout = timeout,
  )

  headers <- resp[[1]]$headers$column

  resp <- as.data.frame(do.call("rbind", resp[[1]]$rows), stringsAsFactors = F)

  colnames(resp) <- headers

  return(resp)
}

#?filter=orgunit_level:eq:2&filter=orgunit_name:ilike:bo
#?criteria=level:4


#' @export
#' @title listSqlViews
#' @param base_url base_url
#' @return dataframe with the list of sql views

listSqlViews <- function(base_url = getOption("baseurl")){

  resp <- api_get(
    path = "sqlViews/", base_url = base_url
  )

  return(resp[[1]])

}

