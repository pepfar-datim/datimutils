#' @title duplicateResponse
#' @description adds in duplicates to the response if they were in the filter
#' @param resp api response after simplification of data structure
#' @param expand a vector of something
#' @param by which column to use as a reference during duplication
#' @return the same api reponse that entered but with added records
#'
#'
duplicateResponse <- function(resp, expand, by) {
  if (!(is.vector(resp))) {
    if(all(is.na(match(expand, resp[, by])))){
      return(resp)
    }
    resp <- resp[match(expand, resp[, by]), ]
  } else {
    resp <- resp[match(expand, resp)]
  }

  return(resp)
}

#' @title .splitUrlComponent
#' @description splits components of a long url into multiple urls
#' @param values a vector of values to split
#' @param limit the limit of length of each split
#' @return a list with the splits
#'

.splitUrlComponent <- function(values, limit) {
    values_list <- list()
    times_to_split <- ceiling(sum(nchar(values)) / limit) + 1
    seq_to_use <- ceiling(seq(1, length(values), length = times_to_split))
    for (i in seq_along(seq_to_use)) {
      if (i == 1) {
        values_list[[i]] <- values[seq_to_use[i]:seq_to_use[i + 1]]
      } else if (i != length(seq_to_use)) {
        values_list[[i]] <- values[(seq_to_use[i] + 1):seq_to_use[i + 1]]
      }
    }

  return(values_list)
}

#' @title getMetadataEndpoint
#' @description wrapper to getMetadata that retrieves a metadata endpoint
#' @param end_point the endpoint to use
#' @param values - string vector of identifiers that will be used in the filter
#' e.g. vector of uids, names, or codes.
#' @param by - what to filter by, i.e. id or name, default is id
#' @param fields - the fields, which can come in any formt as long as all
#' components are present
#' @param modify_fields whether or not to modify fields or use exactly what is passed in
#' @param d2_session the d2Session object, default is "d2_default_session",
#' it will be made upon logining in to datim with loginToDATIM
#' @param retry the number of times to try the call
#' @return the metadata response in json format and flattened
#' @usage
#'
#' .getMetadataEndpoint(end_point, values, by, fields, modify_fields, d2_session, retry)
#'
#' getCategories(values, by, fields, modify_fields, d2_session, retry)
#'
#' getCatCombos(values, by, fields, modify_fields, d2_session, retry)
#'
#' getCatOptionCombos(values, by, fields, modify_fields, d2_session, retry)
#'
#' getCatOptionGroupSets(values, by, fields, modify_fields, d2_session, retry)
#'
#' getCatOptionGroups(values, by, fields, modify_fields, d2_session, retry)
#'
#' getCatOptions(values, by, fields, modify_fields, d2_session, retry)
#'
#' getDataElementGroupSets(values, by, fields, modify_fields, d2_session, retry)
#'
#' getDataElementGroups(values, by, fields, modify_fields, d2_session, retry)
#'
#' getDataElements(values, by, fields, modify_fields, d2_session, retry)
#'
#' getDataSets(values, by, fields, modify_fields, d2_session, retry)
#'
#' getIndicatorGroupSets(values, by, fields, modify_fields, d2_session, retry)
#'
#' getIndicatorGroups(values, by, fields, modify_fields,  d2_session, retry)
#'
#' getIndicators(values, by, fields, modify_fields, d2_session, retry)
#'
#' getOptionGroupSets(values, by, fields, modify_fields, d2_session, retry)
#'
#' getOptionGroups(values, by, fields, modify_fields, d2_session, retry)
#'
#' getOptionSets(values, by, fields, modify_fields, d2_session, retry)
#'
#' getOptions(values, by, fields, modify_fields, d2_session, retry)
#'
#' getOrgUnitGroupSets(values, by, fields, modify_fields, d2_session, retry)
#'
#' getOrgUnitGroups(values, by, fields, modify_fields, d2_session, retry)
#'
#' getOrgUnits(values, by, fields, modify_fields, d2_session, retry)
#'
#' getDimensions(values, by, fields, modify_fields, d2_session, retry)
#'
.getMetadataEndpoint <- function(end_point, values,
                                 by = "id",
                                 fields = NULL,
                                 modify_fields = TRUE,
                                 d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  see <- try(stringi::stri_extract_all_regex(fields, "\\[[^()]+\\]")[[1]], silent = TRUE)

  name_reduce <- NULL

  if(modify_fields){
  default_fields <- if (is.null(fields)) {
    c(by, "name", "id")
  } else if (!(any(grepl("name", fields)))) {
    c(by, fields, "name")
  } else if (length(see) != 0 && class(see) != "try-error") {
    if (grepl("name", see) &&
        !(grepl("name",
                gsub(gsub("\\]", "\\\\]", gsub("\\[", "\\\\[", see)),
                     "",
                     fields)))) {
      c(by, fields, "name")
    } else {
      fields
    }
  } else {
    fields
  }
  default_fields <- stringi::stri_replace_all_fixed(default_fields, " ", "")
  default_fields <- unique(default_fields)
  } else{
    default_fields <- fields
  }

  # by parameter restricted to being an identifiable property
  # as defined in DHIS2 docs
  if (!(by %in% c("name", "id", "code", "shortName"))) {
    stop(
      "getOrgUnits expects a by parameter of id (the default), name, code, or ",
      "shortName. Use the ",
      "more general getMetadata function for other scenarios."
    )
  }
  if(modify_fields){
  if ((by == "name" && is.null(fields))) {
    name_reduce <- "id"
  } else if (is.null(fields)) {
    name_reduce <- "name"
  } else if (!(is.null(fields))) {
    name_reduce <- gsub("\\[.*?\\]", "", fields)
    if (length(name_reduce == 1)) {
      name_reduce <- gsub(" ", "", unlist(strsplit(name_reduce, ",")))
    }
  }}
  unique_values <- unique(values)

  #break up url to multiple calls if needed
  if (sum(nchar(unique_values)) > 2000) {

    values_list <- .splitUrlComponent(unique_values, 2000)

    filters <- lapply(values_list, function(x) {
      metadataFilter(
        values = x,
        property = by,
        operator = "in"
      )
    })

    # call getMetadata multiple times
   data_list <-  lapply(filters, function(x) {
     getMetadata(
       end_point = !!end_point,
       d2_session = d2_session,
       x,
       fields = default_fields, retry = retry
     )})

   # bind the responses
   data <- do.call("rbind", data_list)
  } else {
    #normal route
    filters <- metadataFilter(
      values = unique_values,
      property = by,
      operator = "in"
    )

    # call getMetadataa single time
    data <- getMetadata(
      end_point = !!end_point,
      d2_session = d2_session,
      filters,
      fields = default_fields, retry = retry
    )
  }

  #return NULL if there is nothing to return
  length_response <- try(length(data[[1]]), silent = TRUE)
  if (length_response == 0) {
    return(NULL)
  }

  #add in duplicates
  data <- duplicateResponse(resp = data, expand = values, by = by)

  #reduce fields returned
  if (!(is.null(name_reduce)) && class(data) %in% "data.frame") {
    potential_data <- try(data[, name_reduce], silent = TRUE)
    if (!(class(potential_data) == "try-error")) {
      data <- potential_data
    }
  }

  # when the input value is a singleton unnest the top layer if data is a list
  if (length(values) == 1
      && length(data) == 1
      && is.list(data)) {
    return(data[[1]])
  } else {
    return(data)
  }
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCategories <- function(values,
                          by = "id",
                          fields = NULL,
                          modify_fields = TRUE,
                                 d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("categories",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatCombos <- function(values,
                         by = "id",
                         fields = NULL,
                         modify_fields = TRUE,
                         d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("categoryCombos",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptionCombos <- function(values,
                               by = "id",
                               fields = NULL,
                               modify_fields = TRUE,
                               d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("categoryOptionCombos",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptionGroupSets <- function(values,
                                  by = "id",
                                  fields = NULL,
                                  modify_fields = TRUE,
                                  d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("categoryOptionGroupSets",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptionGroups <- function(values,
                               by = "id",
                               fields = NULL,
                               modify_fields = TRUE,
                               d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("categoryOptionGroups",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getCatOptions <- function(values,
                          by = "id",
                          fields = NULL,
                          modify_fields = TRUE,
                          d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("categoryOptions",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataElementGroupSets <- function(values,
                                    by = "id",
                                    fields = NULL,
                                    modify_fields = TRUE,
                                    d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("dataElementGroupSets",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataElementGroups <- function(values,
                                 by = "id",
                                 fields = NULL,
                                 modify_fields = TRUE,
                                 d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("dataElementGroups",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataElements <- function(values,
                            by = "id",
                            fields = NULL,
                            modify_fields = TRUE,
                            d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("dataElements",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getDataSets <- function(values,
                        by = "id",
                        fields = NULL,
                        modify_fields = TRUE,
                        d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("dataSets",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getUserGroups <- function(values,
                        by = "id",
                        fields = NULL,
                        modify_fields = TRUE,
                        d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("userGroups",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getIndicatorGroupSets <- function(values,
                                  by = "id",
                                  fields = NULL,
                                  modify_fields = TRUE,
                                  d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("indicatorGroupSets",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getIndicatorGroups <- function(values,
                               by = "id",
                               fields = NULL,
                               modify_fields = TRUE,
                               d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("indicatorGroups",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getIndicators <- function(values,
                          by = "id",
                          fields = NULL,
                          modify_fields = TRUE,
                          d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("indicators",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptionGroupSets <- function(values,
                               by = "id",
                               fields = NULL,
                               modify_fields = TRUE,
                               d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("optionGroupSets",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptionGroups <- function(values,
                            by = "id",
                            fields = NULL,
                            modify_fields = TRUE,
                            d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("optionGroups",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptionSets <- function(values,
                          by = "id",
                          fields = NULL,
                          modify_fields = TRUE,
                          d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("optionSets",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOptions <- function(values,
                       by = "id",
                       fields = NULL,
                       modify_fields = TRUE,
                       d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("options",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOrgUnitGroupSets <- function(values,
                                by = "id",
                                fields = NULL,
                                modify_fields = TRUE,
                                d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("organisationUnitGroupSets",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                      modify_fields = modify_fields,
                      d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOrgUnitGroups <- function(values,
                             by = "id",
                             fields = NULL,
                             modify_fields = TRUE,
                             d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("organisationUnitGroups",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getOrgUnits <- function(values,
                        by = "id",
                        fields = NULL,
                        modify_fields = TRUE,
                        d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("organisationUnits",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}

#' @export
#' @rdname dot-getMetadataEndpoint
getDimensions <- function(values,
                          by = "id",
                          fields = NULL,
                          modify_fields = TRUE,
                          d2_session = dynGet("d2_default_session", inherits = TRUE), retry = 1) {
  .getMetadataEndpoint("dimensions",
                       values = values,
                       by = as.character(rlang::ensym(by)),
                       fields = fields,
                       modify_fields = modify_fields,
                       d2_session = d2_session, retry = retry
  )
}
