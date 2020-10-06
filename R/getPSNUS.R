#' @export
#' @title getPSNUs
#' @description wrapper to getOrgUnitGroups that retrieves PSNUs
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename rename id to psnu_id and name to psnu_name
#' @param retry the number of times to try the call
#' @importFrom dplyr %>%
#' @return nested data frame with PSNUs for provided uids

getPSNUs <- function(uids = NULL,
         fields = "id",
         rename = F, retry = 1) {

  to_subset <- fields
  fields <- paste0(fields, collapse = ",")
  fields <- paste0("organisationUnits[path," ,fields,"]")

  psnus <- getOrgUnitGroups("AVy8gJXym2D",
                          fields = fields, retry = retry)

  if(!(is.null(uids))){
    psnus <- psnus[unlist(sapply(uids, grep, psnus$path, USE.NAMES = F)), ]
    if(NROW(psnus) == 0){
      return(NULL)
    }
  }

  # only return if in fields
  psnus <- psnus[,to_subset, drop=F]


  # rename id to psnu_id and name to psnu_name
  if(rename){
    name_key <- c(id = "psnu_id", name = "psnu_name")
  names(psnus) <- name_key[names(psnus)]}


  return(psnus)
}


#' @export
#' @title getCommunities
#' @description wrapper to getOrgUnitGroups that retrieves PSNUs
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename rename id to psnu_id and name to psnu_name
#' @param retry the number of times to try the call
#' @importFrom dplyr %>%
#' @return nested data frame with Communities for provided uids

getCommunities <- function(uids = NULL,
         fields = "id",
         rename = F, retry = 1) {

  to_subset <- fields
  fields <- paste0(fields, collapse = ",")
  fields <- paste0("organisationUnits[path," ,fields,"]")

  communities <- getOrgUnitGroups("PvuaP6YALSA",
                          fields = fields, retry = retry)

  if(!(is.null(uids))){
    communities <- communities[unlist(sapply(uids, grep, communities$path, USE.NAMES = F)), ]
    if(NROW(communities) == 0){
      return(NULL)
    }
  }

  # only return if in fields
  communities <- communities[,to_subset]

  if(is.atomic(communities)){
    return(communities)
  }

  # rename id to community_id and name to community_name
  if(rename){
    name_key <- c(id = "community_id", name = "community_name")
  names(communities) <- name_key[names(communities)]}

  return(communities)
}


#' @export
#' @title getFacilities
#' @description wrapper to getOrgUnitGroups that retrieves PSNUs
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename rename id to psnu_id and name to psnu_name
#' @param retry the number of times to try the call
#' @importFrom dplyr %>%
#' @return nested data frame with Facilities for provided uids

getFacilities <- function(uids = NULL,
         fields = "id",
         rename = F, retry = 1) {

  to_subset <- fields
  fields <- paste0(fields, collapse = ",")
  fields <- paste0("organisationUnits[path," ,fields,"]")

  facilities <- getOrgUnitGroups("POHZmzofoVx",
                          fields = fields, retry = retry)

  if(!(is.null(uids))){
    facilities <- facilities[unlist(sapply(uids, grep, facilities$path, USE.NAMES = F)), ]
    if(NROW(facilities) == 0){
      return(NULL)
    }
  }

  # only return if in fields
  facilities <- facilities[,to_subset,drop=F]


  # rename id to facility_id and name to facility_name
  if(rename){
    name_key <- c(id = "facility_id", name = "facility_name")
  names(facilities) <- name_key[names(facilities)]}

  return(facilities)
}


#' @export
#' @title getMilitaryOrgUnits
#' @description wrapper to getOrgUnitGroups that retrieves PSNUs
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename rename id to psnu_id and name to psnu_name
#' @param retry the number of times to try the call
#' @importFrom dplyr %>%
#' @return nested data frame with MilitaryOrgUnits for provided uids

getMilitaryOrgUnits <- function(uids = NULL,
         fields = "id",
         rename = F, retry = 1) {

  to_subset <- fields
  fields <- paste0(fields, collapse = ",")
  fields <- paste0("organisationUnits[path," ,fields,"]")

  military_units <- getOrgUnitGroups("nwQbMeALRjL",
                          fields = fields, retry = retry)

  if(!(is.null(uids))){
    military_units <- military_units[unlist(sapply(uids, grep, military_units$path, USE.NAMES = F)), ]
    if(NROW(military_units) == 0){
      return(NULL)
    }
  }

  # only return if in fields
  military_units <- military_units[,to_subset, drop=F]

  # rename id to militaryOrg_id and name to militaryOrg_name
  if(rename){
    name_key <- c(id = "militaryOrg_id", name = "militaryOrg_name")
  names(military_units) <- name_key[names(military_units)]}

  return(military_units)
}


#' @export
#' @title getCountries
#' @description wrapper to getOrgUnitGroups that retrieves PSNUs
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename rename id to psnu_id and name to psnu_name
#' @param retry the number of times to try the call
#' @importFrom dplyr %>%
#' @return nested data frame with Countries for provided uids

getCountries <- function(uids = NULL,
         fields = "id",
         rename = F, retry = 1) {

  to_subset <- fields
  fields <- paste0(fields, collapse = ",")
  fields <- paste0("organisationUnits[path," ,fields,"]")

  countries <- getOrgUnitGroups("cNzfcPWEGSH",
                          fields = fields, retry = retry)

 if(!(is.null(uids))){
    countries <- countries[unlist(sapply(uids, grep, countries$path, USE.NAMES = F)), ]
    if(NROW(countries) == 0){
      return(NULL)
    }
  }

  # only return if in fields
  countries <- countries[,to_subset, drop=F]

  # rename id to country_id and name to country_name
  if(rename){
    name_key <- c(id = "country_id", name = "country_name")
  names(countries) <- name_key[names(countries)]}

  return(countries)
}
