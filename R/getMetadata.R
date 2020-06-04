#' @title duplicateResponse(resp)
#' @description adds in duplicates to the response if they were in the filter
#' @param resp api response after simplification of data structure
#' @param expand a table with the number of times to duplicate each specific row
#' @return the same api reponse that entered but with added records
#'

duplicateResponse <- function(resp, expand) {

  # match rows of resp to rows of expand
  resp <- resp[match(expand$x, resp[, 1]), , drop = F]
  expand <- expand[expand$x %in% resp[, 1], ]
  bindlist <- list()

  # create the duplicates and store in a list
  for (i in 1:nrow(expand))
  {
    bindlist[[i]] <- rep(resp[i, ], expand[expand$x == resp[i, 1], "Freq"] - 1)
  }

  # add in the duplicates to the final response
  bind <- as.data.frame(c(do.call("rbind", bindlist)))
  colnames(bind) <- colnames(resp)
  resp <- rbind(resp, bind)

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
  
  #if the format comes in correct
  if(stringr::str_count(look, pattern = ":") == 2){
    filter_option <- stringr::str_extract(look, '(?<=:).*(?=:)')    
    filter_item <- stringr::str_extract(look, '^[^:]*')
    rest <- stringr::str_extract(look, '[^:]+$')
    end_point <- ifelse(is.na(end_point), "", end_point)
    end_point_tentative = ""
    
    # creates a basic filter path
    ex <- paste0(filter_item,
                 filter_option,
                 rest)
    } else{
  
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

  # removes whitespace
  ex <- gsub(" ", "", ex)

  return(ex)
}

#' @export
#' @title getMetadata(base_url, end_point, filters, fields, filters2 = NULL, pluck)
#' @description General utility to get metadata details from DATIM
#' @param end_point string - api endpoint for the metadata of interest
#' e.g. dataElements, organisationUnits
#' @param filters - the filters
#' @param fields - the fields
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param pluck - whether to add pluck option as documented by dhis2 api
#' developer guide
#' @param retry number of times to retry
#' @param expand dataframe to know how to expand result in case of duplicate filters
#' @return the metadata response in json format and flattened
#'

getMetadata <- function(end_point,
                        filters = NULL, fields = NULL,
                        base_url = getOption("baseurl"),
                        pluck = F, retry = 1,
                        expand = NULL) {

  # if no filters or fields are specified, just use endpoint as path
  if (!(is.null(filters)) | !(is.null(fields))) {
    end_point <- gsub("/", "", end_point)
  }

  # set up storage for multiple filter arguments
  filter_storage <- list()

  # process filter arguments
  if (!(is.null(filters))) {
    filters2 <- as.list(filters)
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
  if (is.null(fields) & is.null(filters)) {
    path <- end_point
  }

  # pass path in api_get
  resp <- api_get(
    path = path, base_url = base_url, retry = retry, timeout = 60,
    api_version = NULL
  )

  # simplify data structure
  resp <- simplifyStructure(resp)

  # add in duplicates if needed
  if (!(is.null(expand))) {
    resp <- duplicateResponse(resp, expand)
  }

  return(resp)
}

######################################################################################

#' @export
#' @title id_eq
#' @examples
#' base_url <- "https://play.dhis2.org/2.34/"
#' httr::GET(paste0(base_url, "api/me"),
#'           httr::authenticate("admin", "district"))
#'
#' api_filter <- id_eq("FTRrcoaog83")
#'
#' print(api_filter)
#'
#' getMetadata("dataElements", api_filter, base_url= base_url)
#'
id_eq <- function(value) {
  paste0("id:eq:", value)
}

#' @export
#' @title id_not_eq
#' @examples
#' base_url <- "https://play.dhis2.org/2.34/"
#' httr::GET(paste0(base_url, "api/me"),
#'           httr::authenticate("admin", "district"))
#'
#' api_filter <- id_not_eq("lxAQ7Zs9VYR")
#' print(api_filter)
#'
#' getMetadata("programs",
#'             api_filter,
#'             base_url= base_url)
#'
id_not_eq <- function(value) {
  paste0("id:!eq:", value)
}

#' @export
#' @title id_in
#' @examples
#' base_url <- "https://play.dhis2.org/2.34/"
#' httr::GET(paste0(base_url, "api/me"),
#'           httr::authenticate("admin", "district"))
#' ids_vctr <- c("lxAQ7Zs9VYR", "IpHINAT79UW")
#' ids_str <- "lxAQ7Zs9VYR,IpHINAT79UW"
#' api_filter <- id_in(ids_vctr)
#' print(api_filter)
#'
#' getMetadata("programs",
#'             api_filter,
#'             base_url= base_url)
#'
#' api_filter = id_in(ids_str)
#' print(api_filter)
#'
#' getMetadata("programs",
#'             api_filter,
#'             base_url= base_url)
#'
id_in <- function(values) {
  IN(values, "id")
}

#' @export
#' @title id_not_in
#' @examples
#' base_url <-  "https://play.dhis2.org/2.34/"
#' httr::GET(paste0(base_url, "api/me"),
#'           httr::authenticate("admin", "district"))
#' ids_vctr <- c("lxAQ7Zs9VYR", "IpHINAT79UW")
#' ids_str <- "lxAQ7Zs9VYR,IpHINAT79UW"
#' api_filter <-  id_not_in(ids_vctr)
#' print(api_filter)
#'
#' getMetadata("programs",
#'             api_filter,
#'             base_url= base_url)
#'
#' api_filter <-  id_not_in(ids_str)
#' print(api_filter)
#'
#' getMetadata("programs",
#'             api_filter,
#'             base_url= base_url)
#'
id_not_in <- function(values) {
  notIN(values, "id")
  }

#' @export
#' @title .id_eq
#' @examples
#' base_url = "https://play.dhis2.org/2.34/"
#' httr::GET(paste0(base_url, "api/me"),
#'           httr::authenticate("admin", "district"))
#'
#' api_filter = .id_eq(property_prefix = "organisationUnits",
#'                     value    = "bVZTNrnfn9G")
#'
#' print(api_filter)
#'
#' getMetadata("programs", api_filter, base_url= base_url)
#'
.id_eq <- function(value, property_prefix) {
  paste0(property_prefix, ".id:eq:", value)
}

#' @export
#' @title name_like
#' @examples
#' base_url <- "https://play.dhis2.org/2.34/"
#' httr::GET(paste0(base_url, "api/me"),
#'           httr::authenticate("admin", "district"))
#'
#' api_filter <- name_like("Antenatal")
#'
#' print(api_filter)
#'
#' getMetadata("programs", api_filter, base_url= base_url)
#'
name_like <- function(value) {
  paste0("name:like:", value)
}

#' @export
#' @title name_like
#' @examples
#' base_url <- "https://play.dhis2.org/2.34/"
#' httr::GET(paste0(base_url, "api/me"),
#'           httr::authenticate("admin", "district"))
#'
#' api_filter <- name_ilike("antenatal")
#'
#' print(api_filter)
#'
#' getMetadata("programs", api_filter, base_url= base_url)
#'
name_ilike <- function(value) {
  paste0("name:ilike:", value)
}

#' @export
#' @title IN
#' @examples
#' base_url <- "https://play.dhis2.org/2.34/"
#' httr::GET(paste0(base_url, "api/me"),
#'           httr::authenticate("admin", "district"))
#'
#' api_filter <- IN(c("lxAQ7Zs9VYR", "IpHINAT79UW"), "id")
#'
#' print(api_filter)
#'
#' getMetadata("programs", api_filter, base_url= base_url)
#'
IN <- function(values, property) {
  paste0(property, ":in:[",
         paste0(values, collapse = ","), 
         "]")
}

#' @export
#' @title notIN
#' @examples
#' base_url <- "https://play.dhis2.org/2.34/"
#' httr::GET(paste0(base_url, "api/me"),
#'           httr::authenticate("admin", "district"))
#'
#' api_filter <- notIN(c("lxAQ7Zs9VYR", "IpHINAT79UW"), "id")
#'
#' print(api_filter)
#'
#' getMetadata("programs", api_filter, base_url= base_url)
#'
notIN <- function(values, property) {
  paste0(property, ":!in:[",
         paste0(values, collapse = ","), 
         "]")
}



### This is what I showed in demo the other day



library(tidyverse)

groups <- c("RXL3lPSK8oG", #clinics
            "tDZVQ1WtwpA"  #hospitals
)
district <- "Bo"


# "standard" functions to help create :in: filters for metadata calls
# uses non-standard evaluation of the property so our code can have name and id
# instead of "name" and "in" when specifying filter property, examples below
IN <- function(values, property) {
  # ensym used to allow for non standard evaluation
  # property accepts a symbol or a string
  property <- rlang::as_string(rlang::ensym(property))
  paste0(property, ":in:[",
         paste0(values, collapse = ","), 
         "]")
}

IN(groups, id)
groups %>% IN(id)
IN(groups, "id")
groups %>% IN("id")

IN(groups, organisationUnitGroups.id)
groups %>% IN(organisationUnitGroups.id)
IN(groups, "organisationUnitGroups.id")
groups %>% IN("organisationUnitGroups.id")

# infix version of previous 
# allows for creating filters like 
# id %IN% uid_vctr
'%IN%' <- function(property, values){
  property <- rlang::ensym(property)
  IN(values, !!property)
} 

id %IN% groups
"id" %IN% groups

organisationUnitGroups.id %IN% groups
"organisationUnitGroups.id" %IN% groups

# helpers for common properties
# would be like idIN(uid_vctr) or uid_vctr %>% idIN
# these are nice because rstudio can auto code complete the function names
# super useful for use from the commandline
# the .id allows for reaching into collections in the json e.g.
# organisationUnit.id:in:[sadg,asfg]
# probably good for name, shortName, code, id, path ???

name_IN <- function(values, property_prefix) {
  property_prefix <- rlang::as_string(rlang::ensym(property_prefix))
  if (property_prefix == ""){
    property <- "name"
  } else{
    property <- paste0(property_prefix, ".name")
  }
  (!!property) %IN% values
}

id_IN <- function(values, property_prefix) {
  property_prefix <- rlang::as_string(rlang::ensym(property_prefix))
  if (property_prefix == ""){
    property <- "id"
  } else{
    property <- paste0(property_prefix, ".id")
  }
  (!!property) %IN% values
} 

id_IN(groups)

# as above for a different metadata filter operator
# we could easily have these for all possible operators
LIKE <- function(value, property) {
  property <- rlang::as_string(rlang::ensym(property))
  paste0(property, ":like:", value)
}

'%LIKE%' <- function(property, value) {
  property <- rlang::ensym(property)
  LIKE(value, !!property)
}

name_LIKE <- function(value, property_prefix) {
  property_prefix <- rlang::as_string(rlang::ensym(property_prefix))
  if (property_prefix == ""){
    property <- "name"
  } else{
    property <- paste0(property_prefix, ".name")
  }
  (!!property) %LIKE% value
}  

path_LIKE <- function(value, property_prefix) {
  property_prefix <- rlang::as_string(rlang::ensym(property_prefix))
  if (property_prefix == ""){
    property <- "path"
  } else{
    property <- paste0(property_prefix, ".path")
  }
  (!!property) %LIKE% value
}  


# examples
district  <-  "Bo"
LIKE(district, "name")
LIKE(district, name)
"name" %LIKE% district
name %LIKE% district
name_LIKE(district)
name_LIKE(district, organisationUnit)

# storing the propery in a variable first
var = "name"
(!!var) %LIKE% district

path %LIKE% district
path_LIKE(district)


groups <- c("RXL3lPSK8oG", #clinics
            "tDZVQ1WtwpA"  #hospitals
)

groups %>% id_IN()
groups %>% IN(id)
id %IN% groups

groups %>% id_IN(organisationUnitGroups)
groups %>% IN(organisationUnitGroups.id)
organisationUnitGroups.id %IN% groups

c(organisationUnitGroups.id %IN% groups,
  name %LIKE% district)

c(id_IN(groups, organisationUnit),
  name %LIKE% district)


name %LIKE% district
district %>% name_LIKE()
name_LIKE(district)
LIKE(district, name)

name %LIKE% "Bo"
"Bo" %>% name_LIKE()
name_LIKE("Bo")
LIKE("Bo", name)

organisationUnit.name %LIKE% district

district %>% LIKE(organisationUnit.name)


filters <- c(path %LIKE% "Bo", 
             organisationUnitGroups.id %IN% groups)
print(filters)
filters <- c(path_LIKE("Bo"),
             id_IN(groups, organisationUnitGroups))
print(filters)


# examples grabbing metadata with any number of filters built
# using the helpers
# end_point parameter supported by non-standard evaluation

getMetadata <- function(end_point, ..., fields = NULL){
  end_point <- rlang::ensym(end_point)
  filters = paste("", ..., sep = "&filter=")
  if (!is.null(fields)){
    fields = paste0("&fields=", paste0(fields, collapse = ","))}
  
  call <- paste0("https://play.dhis2.org/2.33/api/",
                 end_point, ".json",
                 "?paging=false",
                 filters,
                 fields)
  print(call)
  
  httr::content(httr::GET(call, httr::authenticate("admin","district")), as = "parsed")
}

filters <- c(path_LIKE("Bo"),
             id_IN(groups, organisationUnitGroups))
print(filters)
groups <- c("RXL3lPSK8oG", #clinics
            "tDZVQ1WtwpA"  #hospitals
)
getMetadata(organisationUnits,
            path_LIKE("Bo"),
            id_IN(groups, organisationUnitGroups))


getMetadata(organisationUnits,
            name %LIKE% "Bo", 
            organisationUnitGroups.id %IN% groups,
            fields = "name,id,path")

getMetadata(organisationUnits,
            name_LIKE("Bo"), 
            organisationUnitGroups.id %IN% groups,
            fields = "name,id,path")

getMetadata(organisationUnits,
            id %IN% groups)


# these were made for piping an mutating

#default takes ID and retuns name
getOrgUnits(id_vctr) #returns name vector
dplyr::mutate(data, name = getOrgUnits(ids)) # adds column with names

data %>% dplyr::mutate(name = getOrgUnits(ids)) # adds column with names

# but can always modfy defauls
getOrgUnits(id_vctr, fields = "name,id,path") # exta fields
getOrgUnits(id_vctr, oganisationUnitGroups.id, fields = "name,id,path") modified filter
getOrgUnits(name_vctr, name)
getOrgUnits(code_vctr, code)

id_vctr %>% getOrgUnits()
country_vctr %>% getPSNUs() # list of PSNUs for specified countries

