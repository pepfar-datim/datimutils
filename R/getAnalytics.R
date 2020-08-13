#' @export
#' @title getAnalytics
#' @description calls the analytics endpoint u
#' @param ... any options here to tag on the end of the url
#' @param dimensions dimensions
#' @param filters filters
#' @param start_date start_date
#' @param end_date end_date
#' @param order order
#' @param columns columns
#' @param rows rows
#' @param sort_order sort_order
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param retry retry
#' @return data frame with the rows of the response

getAnalytics <-  function(..., dimensions = NULL, filters = NULL, start_date = NULL, end_date = NULL,
                               order = NULL, columns = NULL, rows = NULL, sort_order = NULL,
                               base_url = getOption("baseurl"), retry=1){
  table <- NULL

  if(!(is.null(sort_order)))
    {
    sort_order <- paste0("sortOrder=", sort_order)
  }
  if(!(is.null(order)))
    {
    order <- paste0("order=", sort_order)
  }
  if(!(is.null(start_date)))
    {
    start_date <- paste0("startDate=", start_date)
  }
  if(!(is.null(end_date)))
    {
    end_date <- paste0("endDate=", end_date)
  }
      if(!(is.null(rows)) & !(is.null(columns)))
    {
    table <- paste0("tableLayout=true&columns=",
                    paste0(columns, collapse = ";"),"&rows=",
                    paste0(rows, collapse = ";"))
  }

  end_point <- "analytics?"
  ends <- unlist(list(...))
  ends <- paste0(ends,collapse = "&")

  path <- paste0(end_point, stringr::str_c(dimensions, filters,table, sort_order, order, start_date, end_date,  ends,  sep = "&"))
  if(substr(path,nchar(path),nchar(path)) == "&")
    {
    path <- substr(path,1,nchar(path)-1)
  }
  resp <- api_get(
    path = path, base_url = base_url, retry = retry
  )

  return(resp)
}

#' @export
#' @title dForm
#' @description formats dimensions for getAnalytics calls
#' @param ... dimensions
#' @param id id
#' @return formatted dimensions

dForm <- function(..., id = NULL)
{
  if(is.null(id)){
    stop("must specify at least one dimension id")
  }
  values <- list(...)
  values <- lapply(values, function(x) paste0(x, collapse = ";"))
  values <- mapply(function(x,y) paste0("dimension=", y, ":", x), values, id)
  return(paste0(unlist(values), collapse = "&"))
}

#' @export
#' @title fForm
#' @description formats filters for getAnalytics calls
#' @param ... filters
#' @param id id
#' @return formatted filters

fForm <- function(..., id = NULL)
{
  if(is.null(id)){
    stop("must specify at least one filter id")
  }
  values <- list(...)
  values <- lapply(values, function(x) paste0(x, collapse = ";"))
  values <- mapply(function(x,y) paste0("filter=", y, ":", x), values, id)
  return(paste0(unlist(values), collapse = "&"))
}

