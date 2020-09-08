#' @export
#' @title getAnalytics
#' @description calls the analytics endpoint
#' @param ... any options here to tag on the end of the url
#' @param dx dimensions dx
#' @param dx_f filters dx
#' @param pe dimensions pe
#' @param pe_f filters pe
#' @param ou dimensions ou
#' @param ou_f filters f
#' @param co dimensions co
#' @param co_f filters co
#' @param ao dimensions ao
#' @param ao_f filters ao
#' @param outputIdScheme UID for uids, NAME for names
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param retry retry
#' @return data frame with the rows of the response

getAnalytics <-  function(..., dx = NULL, dx_f = NULL, pe = NULL, pe_f = NULL,
ou = NULL, ou_f = NULL, co = NULL, co_f = NULL,
ao = NULL, ao_f = NULL,
outputIdScheme = "UID",
base_url = getOption("baseurl"), retry = 1){

  #variable set up
  dx <- .dForm(dx, id = "dx");dx_f <- .fForm(dx_f, id = "dx")
  pe <- .dForm(pe, id = "pe");pe_f <- .fForm(pe_f, id = "pe")
  ou <- .dForm(ou, id = "ou");ou_f <- .fForm(ou_f, id = "ou")
  co <- .dForm(co, id = "co");co_f <- .fForm(co_f, id = "co")
  ao <- .dForm(ao, id = "ao");ao_f <- .fForm(ao_f, id = "ao")

  #process ...
  end_point <- "analytics?"
  ends <- unlist(list(...))
  ends <- paste0(ends,collapse = "&")

  #collapse everything and form path
  path <- paste0(end_point, stringr::str_c(dx,pe,ou,co,ao,dx_f,pe_f,ou_f,co_f,ao_f, ends, paste0("outputIdScheme=",outputIdScheme), sep = "&"))

  #call api
  resp <- api_get(
    path = path, base_url = base_url, retry = retry
  )

  #collect data types
  coercions <- resp$headers$valueType
  #collect replacements for uids
  #replacements <- resp$metaData$items
  #collect column names
  col_names <- resp$headers$column
  #get data
  resp <- as.data.frame(resp$rows, stringsAsFactors = F)
  #change column names
  colnames(resp) <- col_names

  ##replace uids with readable names
  #resp <- apply(resp,2, function(x) {ifelse(x %in% names(replacements),
  #       unlist(replacements[names(replacements) %in% x]),
  #       x)})
  #
  #resp <- as.data.frame(resp, stringsAsFactors = F)

  #change data types to numeric where possible
  resp[,coercions == "NUMBER"] <- sapply(resp[,coercions == "NUMBER"], as.numeric)

  return(resp)
}


#' @title .dForm
#' @description formats dimensions for getAnalytics calls
#' @param ... dimensions
#' @param id id
#' @return formatted dimensions

.dForm <- function(..., id = NULL){
  if(missing(...)|is.null(...)){
    return(NULL)
  }
  values <- list(...)
  values <- lapply(values, function(x) paste0(x, collapse = ";"))
  values <- mapply(function(x,y) paste0("dimension=", y, ":", x), values, id)
  return(paste0(unlist(values), collapse = "&"))
}


#' @export
#' @title .fForm
#' @description formats filters for getAnalytics calls
#' @param ... filters
#' @param id id
#' @return formatted filters

.fForm <- function(..., id = NULL){
  if(missing(...)|is.null(...)){
    return(NULL)
  }
  values <- list(...)
  values <- lapply(values, function(x) paste0(x, collapse = ";"))
  values <- mapply(function(x,y) paste0("filter=", y, ":", x), values, id)
  return(paste0(unlist(values), collapse = "&"))
}



#' @title .analyticsFilter(values, property, operator)
#' @description used to format filter strings for metadata calls
#' @param values the values in property=operator:values
#' @param property the property in property=operator:values
#' @param operator the operator in property=operator:values
#' @return property=operator:values
#' @usage
#'
#' operator %.d% values
#'
#' operator %.f% values
#'
#' .analyticsFilter(values, property, operator)
.analyticsFilter <- function(values, property, operator) {

    values <- paste0(values, collapse = ",")

    return(paste0(property, "=", operator, ":", values))
}

#' @export
#' @rdname dot-analyticsFilter
"%.d%" <- function(operator, values) {
  operator <- rlang::ensym(operator)
  .analyticsFilter(property = "dimension", operator = operator, values = values)
}


#' @export
#' @rdname dot-analyticsFilter
"%.f%" <- function(operator, values) {
  operator <- rlang::ensym(operator)
  .analyticsFilter(property = "filter", operator = operator, values = values)
}