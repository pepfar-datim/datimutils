#' @title duplicateResponse(resp)
#' @description adds in duplicates to the response if they were in the filter
#' @param resp api response after simplification of data structure
#' @param expand a table with the number of times to duplicate each specific row
#' @return the same api reponse that entered but with added records
#'

duplicateResponse <- function(resp, expand) {

  # every column of resp in expand, whcih one has a true make choose

  fill <- try(as.data.frame(apply(resp, 2, function(y) y %in% expand$x)), silent = T)

  if(class(fill) != "try-error")
  {
  
    if (nrow(fill) > 1) {
    fill <- fill[1, ]
  }

  choose <- resp[, as.logical(fill), drop = T]

  # match rows of resp to rows of expand

  if (!(anyNA(choose))) {
    temp <- match(expand$x, choose)
    if (!(all(is.na(temp)))) {
      resp <- resp[temp, , drop = F]

      bindlist <- list()
      # create the duplicates and store in a list
      if (all(expand$Freq != 1)) {
        expand <- expand[expand$x %in% choose[temp], ]

        for (i in 1:nrow(expand))
        {
          bindlist[[i]] <- rep(i, times = expand$Freq[i])
        }

        # add in the duplicates to the final response
        resp <- resp[c(do.call("rbind", bindlist)), ]
        }
      }
    }
  }
  return(resp)
}

#' @title simplifyStructure(resp)
#' @description takes a api response and simplifies it down to the most basic data structure
#' @param resp raw text response recieved from datim api
#' @return api response reduced to most simple data structure
#'

simplifyStructure <- function(resp) {

  # only enter if class is list and length one, other wise it is already simplified
  if (class(resp) == "list" & length(resp) == 1 & length(resp[[1]]) != 0) {
    possible_resp <- resp
    continue <- T

    # the while bloack reduces the structure till it cant
    while (continue) {
      if (class(possible_resp) == "character") {
        continue <- F
      } else if (class(possible_resp) == "list") {
        possible_resp <- possible_resp[[1]]
      } else if (dim(possible_resp)[1] == 1 & dim(possible_resp)[2] == 1) {
        possible_resp <- possible_resp[[1]]
      } else {
        continue <- F
      }
    }

    # if it is a data frame check if it is nested or standard
    if (class(possible_resp) == "data.frame") {
      if (!(("list" %in% apply(possible_resp, 2, typeof)))) {
        resp <- possible_resp
      } else {
        if (!(length(possible_resp[, sapply(possible_resp, class) == "list"][[1]]) == 0)) {
          resp <- try(tidyr::unnest(possible_resp, cols = colnames(possible_resp)), silent = T)
          if ("try-error" %in% class(resp)) {
            resp <- possible_resp
          }
        }
      }
    } else if (class(possible_resp) == "character") {
      resp <- possible_resp
    }
  }
  return(resp)
}

#' @title processFilters(end_point, filters)
#' @description takes a filter argument and turns it into an api compatible string
#' @param filters wildcard argument that can come in as any format or datatype
#' @param end_point end point
#' @return the processed metadata filter string compatible with DATIM api
#'

processFilters <- function(end_point, filters) {

  # takes filter argument and turns it into a single character string
  ex <- stringr::str_flatten(unlist(sapply(filters, as.character)))

  # removes extraneous info (will be added later anyway for consistency)
  look <- sub("\\?filter=|\\?filter", "", ex)
  look <- sub("\\&filter=|\\&filter", "", look)

  # if the format comes in correct
  if (stringr::str_count(look, pattern = ":") == 2) {
    filter_option <- stringr::str_extract(look, "(?<=:).*(?=:)")
    filter_item <- stringr::str_extract(look, "^[^:]*")
    rest <- stringr::str_extract(look, "[^:]+$")
    end_point <- ifelse(is.na(end_point), "", end_point)
    end_point_tentative <- ""

    # creates a basic filter path
    ex <- paste0(
      filter_item,
      filter_option,
      rest
    )
  } else {

    # extracts end_point and what is not end_point
    end_point_tentative <- stringr::str_extract(look, ".+?(?=id|name)")
    end_point <- ifelse(is.na(end_point_tentative),
      ifelse(is.na(end_point), "", end_point), end_point_tentative
    )
    end_point <- gsub("/", "", end_point)
    look <- sub("(.*)(id|name)", "\\2", look)

    # extracts the original filter
    filter_option <-
      stringr::str_extract(
        substr(sub("name", "", look), 1, 8),
        "!ilike|!like|ilike|like|!in|!eq|in|eq"
      )

    # extracts either id or name from filter
    filter_item <- stringr::str_extract(
      substr(look, 1, 4),
      "name|id"
    )

    ex <- look
  }

  # removes :
  ex <- gsub(":", "", ex)

  # takes first part of filter, i.e. idin
  one <- sub(
    paste0("(", filter_item, filter_option, ")", "(.*)"),
    "\\1", ex
  )
  one.one <- sub(
    paste0("(", filter_item, filter_option, ")", "(.*)"),
    "\\2", ex
  )

  # takes second part of filter, i.e. ["abc"] and adds commas if filter = in
  if (grepl("name", one)) {
    filters <- gsub("\\?", "\\\\?", filters)
    filters <- gsub("\\[", "\\\\[", filters)
    filters <- gsub("\\]", "\\\\]", filters)
    try <- unlist(stringi::stri_extract_all_regex(str = one.one, pattern = filters))
    if (is.na(try)) {
      try <- one.one
    }
    two <- paste0(try[!is.na(try)], collapse = ",")
  } else {
    two <- ifelse(grepl(",", one.one), one.one,
      gsub("(.{11})", "\\1,",
        one.one,
        perl = TRUE
      )
    )
  }
  ex <- paste0(ifelse(is.na(end_point), "", end_point), one, two)
  if (substr(ex, nchar(ex), nchar(ex)) == ",") {
    ex <- substr(ex, 1, nchar(ex) - 1)
  }

  # adds &filter= where needed, and : where needed
  middle <- ifelse(is.na(end_point_tentative), filter_item, paste0(end_point, filter_item))
  ex <- sub(
    paste0("(.*?)(\\&filter=|", middle, "|?filter=", ")"),
    paste0("\\1&filter=", middle), ex
  )
  ex <- sub(
    paste0("(.*?)", "(", filter_item, ")"),
    paste0("\\1", filter_item, ":"), ex
  )
  ex <- sub(
    paste0("(", filter_item, ".*?)", "(", filter_option, ")"),
    paste0("\\1", filter_option, ":"), ex
  )

  # special processing for "in" filter
  if (grepl("in", filter_option) & !(grepl("\\[", ex))) {
    ex <- paste0(sub("(.*?)(in:)", "\\1in:[", ex), "]")
  }

  return(ex)
}

#' @export
#' @title getMetadata(base_url, end_point, filters, fields, filters2 = NULL, pluck)
#' @description General utility to get metadata details from DATIM
#' @param end_point string - api endpoint for the metadata of interest
#' e.g. dataElements, organisationUnits
#' @param ... - the filters
#' @param fields - the fields
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param pluck - whether to add pluck option as documented by dhis2 api
#' developer guide
#' @param retry number of times to retry
#' @param expand dataframe to know how to expand result in case of duplicate filters
#' @return the metadata response in json format and flattened
#'

getMetadata <- function(end_point,
                        ..., fields = "name,id",
                        base_url = getOption("baseurl"),
                        pluck = F, retry = 1,
                        expand = NULL) {
  
  #non-standard evaluation for end_point
  end_point <- rlang::ensym(end_point)  
  
  # if no filters or fields are specified, just use endpoint as path
  if (!(missing(...)) | !(is.null(fields))) {
    end_point <- gsub("/", "", end_point)
  }

  # set up storage for multiple filter arguments
  filter_storage <- list()

  # process filter arguments
  if (!(missing(...))) {
    filter_check <- list(...)
    filters2 <- as.list(...)

    if (length(filter_check) > length(filters2)) {
      filters2 <- filter_check
    }

    for (i in 1:length(filters2))
    {
      if (i == 1) {
        ex2 <- processFilters(end_point = end_point, filters = filters2[[i]])
      } else {
        ex2 <- processFilters(end_point = NULL, filters = filters2[[i]])
      }

      ex2 <- sub(end_point, "", ex2)
      filter_storage[[i]] <- ex2
    }
    filter_storage <- unlist(filter_storage)
    filter_storage <- stringr::str_flatten(filter_storage)
  }

  # if the if loop doesnt get activated this will still create a variable for path
  ef <- ""

  # fields block
  if (!(is.null(fields))) {
    # flattens fields and adds ?fields= if needed
    ef <- stringr::str_flatten(unlist(sapply(fields, as.character)), ",")
    if (!(grepl("fields", ef))) {
      ef <- paste0("&fields=", ef)
    }
  }

  # if filter_storage is empty create placeholder for path string creation
  if (length(filter_storage) != 0) {
    ex <- filter_storage[[1]]
  } else {
    ex <- ""
  }

  # end point manipulation
  if (grepl(end_point, substr(ex, 1, nchar(end_point)))) {
    end_point <- ""
  }

  # create final path
  path <- paste0(
    end_point, ifelse(length(filter_storage) != 0, filter_storage, ""), ef,
    ifelse(pluck, "~pluck", "")
  )
  if (is.null(fields) & missing(...)) {
    path <- end_point
  }

  # pass path in api_get
  resp <- api_get(
    path = path, base_url = base_url, retry = retry, timeout = 60,
    api_version = NULL
  )

  # simplify data structure
  resp <- simplifyStructure(resp)

  # #add in duplicates if needed
  if (!(is.null(expand))) {
    resp <- duplicateResponse(resp, expand)
  }

  return(resp)
}

#' @export
#' @title metadataFilter(values, property, operator)
#' @description used to format filter strings for metadata calls
#' @param values the values in property:operator:value
#' @param property the property in property:operator:value
#' @param operator the operator in property:operator:value
#' @return property:operator:value
#' @usage
#'
#' metadataFilter(values, property, operator)
#'
#' property %deq% value
#'
#' property %d!eq% value
#'
#' property %dlike% value
#'
#' property %d!like% value
#'
#' property %din% values
#'
#' property %d!in% values

metadataFilter <- function(values, property, operator) {
  
  # check values is a vector only for in and !in operators 
  if (length(values) > 1 &&
      !(operator %in% c("in", "!in"))) {
    stop("A vector of values is only supported for in and !in operators")
  }
  
  if (is.null(values) &&
      !(operator %in% c("null", "!null", "empty"))) {
    stop("NULL values are only supported for null, !null and empty operators")
  }
  
  if (!is.null(values) &&
      operator %in% c("null", "!null", "empty")) {
    stop("NULL values required for null, !null and empty operators")
  }
  
  if (operator %in% c("in", "!in")) {
    return(paste0(
      property, ":", operator, ":[",
      paste0(values, collapse = ","),
      "]"
    ))
  } else {
    return(paste0(property, ":", operator, ":", values))
  }
}

#' @export
#' @rdname metadataFilter
"%din%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "in")
}

#' @export
#' @rdname metadataFilter
"%d!in%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!in")
}

#' @export
#' @rdname metadataFilter
"%dlike%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "like")
}

#' @export
#' @rdname metadataFilter
"%d!like%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!like")
}

#' @export
#' @rdname metadataFilter
"%deq%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "eq")
}

#' @export
#' @rdname metadataFilter
"%d!eq%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!eq")
}
