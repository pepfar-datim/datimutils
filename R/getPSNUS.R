#' @export
#' @title getPSNUs
#' @description wrapper to getOrgUnitGroups that retrieves PSNUs
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename rename id to psnu_id and name to psnu_name
#' @param to_list make a list for every uid provided, will drop name from the uids provided
#' @param unnest make a long data frame for every uid provided
#' @param retry the number of times to try the call
#' @return nested data frame with PSNUs for provided ancestor_uids

getPSNUs <- function(uids = NULL,
         fields = "id",
         rename = F, to_list = F, unnest = F, retry = 1) {


  fields <- paste0("organisationUnits[name,id,ancestors[",paste0(fields, collapse = ","),"]")

  psnus <- getOrgUnitGroups("AVy8gJXym2D",
                          fields = fields, retry = retry)

  if(!(is.null(uids))){
psnus <- psnus[psnus$id %in% uids,]
  }

  # rename id to psnu_id and name to psnu_name)
  if(rename){
  psnus <- psnus %>% rename("psnu_id" = "id", "psnu_name" = "name" )}

  #turn dataframe to list for each uid provided
if(to_list){
psnus <- as.list(psnus$ancestors)
  if(rename){
  names(psnus) <- psnus$psnu_id
  } else{
    names(psnus) <- psnus$id
  }
return(psnus)
}


  if(unnest){
    psnus <- tidyr::unnest(psnus, cols = "ancestors")
    return(psnus)
  }


  return(psnus)

}


#' @export
#' @title getCommunities
#' @description wrapper to getOrgUnitGroups that retrieves PSNUs
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename rename id to psnu_id and name to psnu_name
#' @param to_list make a list for every uid provided, will drop name from the uids provided
#' @param unnest make a long data frame for every uid provided
#' @param retry the number of times to try the call
#' @return nested data frame with PSNUs for provided ancestor_uids

getCommunities <- function(uids = NULL,
         fields = "id",
         rename = F, to_list = F, unnest = F, retry = 1) {


  fields <- paste0("organisationUnits[name,id,ancestors[",paste0(fields, collapse = ","),"]")

  communities <- getOrgUnitGroups("AVy8gJXym2D",
                          fields = fields, retry = retry)

  if(!(is.null(uids))){
communities <- communities[communities$id %in% uids,]
  }

  # rename id to psnu_id and name to psnu_name)
  if(rename){
  communities <- communities %>% rename("community_id" = "id", "community_name" = "name" )}

  #turn dataframe to list for each uid provided
if(to_list){
communities <- as.list(communities$ancestors)
  if(rename){
  names(communities) <- communities$community_id
  } else{
    names(communities) <- communities$id
  }
return(communities)
}


  if(unnest){
    communities <- tidyr::unnest(communities, cols = "ancestors")
    return(communities)
  }


  return(communities)

}


#' @export
#' @title getFacilities
#' @description wrapper to getOrgUnitGroups that retrieves PSNUs
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename rename id to psnu_id and name to psnu_name
#' @param to_list make a list for every uid provided, will drop name from the uids provided
#' @param unnest make a long data frame for every uid provided
#' @param retry the number of times to try the call
#' @return nested data frame with PSNUs for provided ancestor_uids

getFacilities <- function(uids = NULL,
         fields = "id",
         rename = F, to_list = F, unnest = F, retry = 1) {


  fields <- paste0("organisationUnits[name,id,ancestors[",paste0(fields, collapse = ","),"]")

  facilities <- getOrgUnitGroups("AVy8gJXym2D",
                          fields = fields, retry = retry)

  if(!(is.null(uids))){
facilities <- facilities[facilities$id %in% uids,]
  }

  # rename id to psnu_id and name to psnu_name)
  if(rename){
  facilities <- facilities %>% rename("facility_id" = "id", "facility_name" = "name" )}

  #turn dataframe to list for each uid provided
if(to_list){
facilities <- as.list(facilities$ancestors)
  if(rename){
  names(facilities) <- facilities$psnu_id
  } else{
    names(facilities) <- facilities$id
  }
return(facilities)
}


  if(unnest){
    facilities <- tidyr::unnest(facilities, cols = "ancestors")
    return(facilities)
  }


  return(facilities)

}


#' @export
#' @title getMilitaryOrgUnits
#' @description wrapper to getOrgUnitGroups that retrieves PSNUs
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename rename id to psnu_id and name to psnu_name
#' @param to_list make a list for every uid provided, will drop name from the uids provided
#' @param unnest make a long data frame for every uid provided
#' @param retry the number of times to try the call
#' @return nested data frame with PSNUs for provided ancestor_uids

getMilitaryOrgUnits <- function(uids = NULL,
         fields = "id",
         rename = F, to_list = F, unnest = F, retry = 1) {


  fields <- paste0("organisationUnits[name,id,ancestors[",paste0(fields, collapse = ","),"]")

  miltary_units <- getOrgUnitGroups("AVy8gJXym2D",
                          fields = fields, retry = retry)

  if(!(is.null(uids))){
miltary_units <- miltary_units[miltary_units$id %in% uids,]
  }

  # rename id to psnu_id and name to psnu_name)
  if(rename){
  miltary_units <- miltary_units %>% rename("militaryOrg_id" = "id", "militaryOrg_name" = "name" )}

  #turn dataframe to list for each uid provided
if(to_list){
miltary_units <- as.list(miltary_units$ancestors)
  if(rename){
  names(miltary_units) <- miltary_units$militaryOrg_id
  } else{
    names(miltary_units) <- miltary_units$id
  }
return(miltary_units)
}


  if(unnest){
    miltary_units <- tidyr::unnest(miltary_units, cols = "ancestors")
    return(miltary_units)
  }


  return(miltary_units)

}


#' @export
#' @title getCountries
#' @description wrapper to getOrgUnitGroups that retrieves PSNUs
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename rename id to psnu_id and name to psnu_name
#' @param to_list make a list for every uid provided, will drop name from the uids provided
#' @param unnest make a long data frame for every uid provided
#' @param retry the number of times to try the call
#' @return nested data frame with PSNUs for provided ancestor_uids

getCountries <- function(uids = NULL,
         fields = "id",
         rename = F, to_list = F, unnest = F, retry = 1) {


  fields <- paste0("organisationUnits[name,id,ancestors[",paste0(fields, collapse = ","),"]")

  countries <- getOrgUnitGroups("AVy8gJXym2D",
                          fields = fields, retry = retry)

  if(!(is.null(uids))){
countries <- countries[countries$id %in% uids,]
  }

  if(rename){
  countries <- countries %>% rename("country_id" = "id", "country_name" = "name" )}

  #turn dataframe to list for each uid provided
if(to_list){
countries <- as.list(countries$ancestors)
  if(rename){
  names(countries) <- countries$country_id
  } else{
    names(countries) <- countries$id
  }
return(countries)
}


  if(unnest){
    countries <- tidyr::unnest(countries, cols = "ancestors")
    return(countries)
  }


  return(countries)

}
