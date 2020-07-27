#' @title duplicateResponse(resp)
#' @description adds in duplicates to the response if they were in the filter
#' @param resp api response after simplification of data structure
#' @param expand a vector of something
#' @param by which column to use as a reference during duplication
#' @return the same api reponse that entered but with added records
#'
#'
duplicateResponse <- function(resp, expand, by) {
  if (!(is.array(resp))) {
    resp <- resp[match(expand, resp[, by]), ]
  } else {
    resp <- resp[match(expand, resp)]
  }

  return(resp)
}


#' @title .getMetadataEndpoint
#' @name dot-getMetadataEndpoint
#' @description wrapper to getMetadata that retrieves a metadata endpoint
#' @param ... the endpoint to use
#' @param values - string vector of identifiers that will be used in the filter
#' e.g. vector of uids, names, or codes.
#' @param by - what to filter by, i.e. id or name, default is id
#' @param fields - the fields, which can come in any formt as long as all
#' components are present
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return the metadata response in json format and flattened
#' @usage
#'
#' .getMetadataEndpoint(..., values, by, fields, base_url)
#'
#' getCategories(values, by, fields, base_url)
#'
#' getCatCombos(values, by, fields, base_url)
#'
#' getCatOptionCombos(values, by, fields, base_url)
#'
#' getCatOptionGroupSets(values, by, fields, base_url)
#'
#' getCatOptionGroups(values, by, fields, base_url)
#'
#' getCatOptions(values, by, fields, base_url)
#'
#' getDataElementGroupSets(values, by, fields, base_url)
#'
#' getDataElementGroups(values, by, fields, base_url)
#'
#' getDataElements(values, by, fields, base_url)
#'
#' getDataSets(values, by, fields, base_url)
#'
#' getIndicatorGroupSets(values, by, fields, base_url)
#'
#' getIndicatorGroups(values, by, fields, base_url)
#'
#' getIndicators(values, by, fields, base_url)
#'
#' getOptionGroupSets(values, by, fields, base_url)
#'
#' getOptionGroups(values, by, fields, base_url)
#'
#' getOptionSets(values, by, fields, base_url)
#'
#' getOptions(values, by, fields, base_url)
#'
#' getOrgUnitGroupSets(values, by, fields, base_url)
#'
#' getOrgUnitGroups(values, by, fields, base_url)
#'
#' getOrgUnits(values, by, fields, base_url)
#'
.getMetadataEndpoint <- function(..., values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")) {


  values <- values; by<- by; fields <- fields; base_url <- base_url

  name_reduce <- NULL

  default_fields <- if (is.null(fields)) {
    c(by, "name", "id")
  } else if (!("name" %in% fields) && !(any(grepl("name", fields)))) {
    c(by, fields, "name")
  } else {
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
    name_reduce <- gsub("\\[[^()]*\\]", "", fields)
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
    end_point = ...,
    base_url = base_url,
    filters,
    fields = default_fields, retry = 1
  )

  length_response <- try(length(data[[1]]), silent = T)
  if(length_response == 0)
    {stop("no data returned by api call")}
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
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(categories, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatCombos  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(categoryCombos, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptionCombos  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(categoryOptionCombos, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptionGroupSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(categoryOptionGroupSets, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptionGroups  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(categoryOptionGroups, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptions  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(categoryOptions, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataElementGroupSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(dataElementGroupSets, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataElementGroups  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(dataElementGroups, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataElements  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(dataElements, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(dataSets, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getIndicatorGroupSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(indicatorGroupSets, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getIndicatorGroups  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(indicatorGroups, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getIndicators  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(indicators, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptionGroupSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(optionGroupSets, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptionGroups  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(optionGroups, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptionSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(optionSets, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptions  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(options, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getOrgUnitGroupSets  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(organisationUnitGroupSets, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getOrgUnitGroups  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")
){
  .getMetadataEndpoint(organisationUnitGroups, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}

#' @export
#' @rdname dot-getMetadataEndpoint
getOrgUnits  <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")){
  .getMetadataEndpoint(organisationUnits, values = values,
                             by = try(as.character(rlang::ensym(by)), silent = TRUE),
                             fields = fields,
                             base_url = base_url)

}
