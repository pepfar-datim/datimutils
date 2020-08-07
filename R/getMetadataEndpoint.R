#' @title duplicateResponse(resp)
#' @description adds in duplicates to the response if they were in the filter
#' @param resp api response after simplification of data structure
#' @param expand a vector of something
#' @param by which column to use as a reference during duplication
#' @return the same api reponse that entered but with added records
#'
#'
duplicateResponse <- function(resp, expand, by) {
  if (!(is.vector(resp))) {
    resp <- resp[match(expand, resp[, by]), ]
  } else {
    resp <- resp[match(expand, resp)]
  }

  return(resp)
}


#' @title getMetadataEndpoint
#' @description wrapper to getMetadata that retrieves a metadata endpoint
#' @param end_point the endpoint to use
#' @param values - string vector of identifiers that will be used in the filter
#' e.g. vector of uids, names, or codes.
#' @param by - what to filter by, i.e. id or name, default is id
#' @param fields - the fields, which can come in any formt as long as all
#' components are present
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param retry the number of times to try the call
#' @return the metadata response in json format and flattened
#' @usage
#'
#' .getMetadataEndpoint(end_point, values, by, fields, base_url, retry)
#'
#' getCategories(values, by, fields, base_url, retry)
#'
#' getCatCombos(values, by, fields, base_url, retry)
#'
#' getCatOptionCombos(values, by, fields, base_url, retry)
#'
#' getCatOptionGroupSets(values, by, fields, base_url, retry)
#'
#' getCatOptionGroups(values, by, fields, base_url, retry)
#'
#' getCatOptions(values, by, fields, base_url, retry)
#'
#' getDataElementGroupSets(values, by, fields, base_url, retry)
#'
#' getDataElementGroups(values, by, fields, base_url, retry)
#'
#' getDataElements(values, by, fields, base_url, retry)
#'
#' getDataSets(values, by, fields, base_url, retry)
#'
#' getIndicatorGroupSets(values, by, fields, base_url, retry)
#'
#' getIndicatorGroups(values, by, fields, base_url, retry)
#'
#' getIndicators(values, by, fields, base_url, retry)
#'
#' getOptionGroupSets(values, by, fields, base_url, retry)
#'
#' getOptionGroups(values, by, fields, base_url, retry)
#'
#' getOptionSets(values, by, fields, base_url, retry)
#'
#' getOptions(values, by, fields, base_url, retry)
#'
#' getOrgUnitGroupSets(values, by, fields, base_url, retry)
#'
#' getOrgUnitGroups(values, by, fields, base_url, retry)
#'
#' getOrgUnits(values, by, fields, base_url, retry)
#'
.getMetadataEndpoint <- function(end_point, values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1) {

  see <- try(stringr::str_extract_all(fields, "\\[[^()]+\\]")[[1]], silent = T)

  name_reduce <- NULL

  default_fields <- if (is.null(fields)) {
    c(by, "name", "id")
  } else if (!(any(grepl("name", fields)))) {
    c(by, fields, "name")
  } else if (length(see)!=0 & class(see) != "try-error") {
  if (grepl("name",see) & !(grepl("name",gsub( gsub("\\]","\\\\]",gsub("\\[","\\\\[",see)), "", fields)))){
    c(by, fields, "name")
  } }else{
    fields
  }
  default_fields <- stringr::str_remove_all(default_fields, " ")
  default_fields <- unique(default_fields)
  # by parameter restricted to being an identifiable property
  # as defined in DHIS2 docs
  if (!(by %in% c("name", "id", "code", "shortName"))) {
    stop(
      "getOrgUnits expects a by parameter of id (the default), name, code, or ",
      "shortName. Use the ",
      "more general getMetadata function for other scenarios."
    )
  }

  if ((by == "name" & is.null(fields))) {
    name_reduce <- "id"
  } else if (is.null(fields)) {
    name_reduce <- "name"
  } else if (!(is.null(fields))) {
    name_reduce <- gsub("\\[.*?\\]", "", fields)
    if (length(name_reduce == 1)) {
      name_reduce <- gsub(" ", "", unlist(strsplit(name_reduce, ",")))
    }
  }


  filters <- metadataFilter(
    values = unique(values),
    property = by,
    operator = "in"
  )

  # call getMetadata with info above
  data <- getMetadata(
    end_point = !!end_point,
    base_url = base_url,
    filters,
    fields = default_fields, retry = retry
  )

  length_response <- try(length(data[[1]]), silent = T)
  if(length_response == 0)
    {return(NULL)}
  data <- duplicateResponse(resp = data, expand = values, by = by)
  if (!(is.null(name_reduce)) && class(data) %in% "data.frame") {
    potential_data <- try(data[, name_reduce], silent = T)
    if(!(class(potential_data) == "try-error"))
      {
      data <- potential_data
    }
  }


  return(data)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCategories  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('categories', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatCombos  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('categoryCombos', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptionCombos  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('categoryOptionCombos', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptionGroupSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('categoryOptionGroupSets', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptionGroups  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('categoryOptionGroups', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptions  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('categoryOptions', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataElementGroupSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('dataElementGroupSets', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataElementGroups  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('dataElementGroups', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataElements  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('dataElements', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('dataSets', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getIndicatorGroupSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('indicatorGroupSets', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getIndicatorGroups  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('indicatorGroups', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getIndicators  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('indicators', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptionGroupSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('optionGroupSets', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptionGroups  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('optionGroups', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptionSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('optionSets', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptions  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('options', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOrgUnitGroupSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('organisationUnitGroupSets', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOrgUnitGroups  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('organisationUnitGroups', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOrgUnits  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl"), retry = 1){
  .getMetadataEndpoint('organisationUnits', values = values,
                             by = as.character(rlang::ensym(by)),
                             fields = fields,
                             base_url = base_url, retry = retry)
}
