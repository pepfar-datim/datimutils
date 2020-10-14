#' @title .getOrgUnitsByOrgUnitGroup
#' @description wrapper to getOrgUnitGroups that retrieves org unit groups
#' @param org_unit_group orgunitgroup to use
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename rename id to psnu_id and name to psnu_name
#' @param retry the number of times to try the call
#' @importFrom dplyr %>%
#' @return nested data frame with org unit group for provided uids

.getOrgUnitsByOrgUnitGroup <- function(
  org_unit_group,
  uids = NULL,
         fields = "id",
         rename = F, retry = 1) {

  if(org_unit_group == "AVy8gJXym2D"){
   name_key <- c(id = "psnu_id", name = "psnu_name", path = "path")
    column_name <- "psnus"
  } else if(org_unit_group == "PvuaP6YALSA"){
   name_key <- c(id = "community_id", name = "community_name", path = "path")
    column_name <- "communities"
  } else if(org_unit_group == "POHZmzofoVx"){
   name_key <- c(id = "facility_id", name = "facility_name", path = "path")
    column_name <- "facilities"
  } else if(org_unit_group == "nwQbMeALRjL"){
   name_key <- c(id = "military_id", name = "military_name", path = "path")
    column_name <- "militaries"
  } else if(org_unit_group == "cNzfcPWEGSH"){
   name_key <- c(id = "country_id", name = "country_name", path = "path")
    column_name <- "countries"
  }

  to_subset <- fields
  fields <- paste0(fields, collapse = ",")
  fields <- paste0("organisationUnits[path," ,fields,"]")

  oug_list <- getOrgUnitGroups(org_unit_group,
                          fields = fields, retry = retry)

  oug_df<- purrr::map(uids, ~dplyr::filter(oug_list,
                               stringr::str_detect(path, .x)) %>%
                                 dplyr::select(to_subset))

  if(rename){
  oug_df <- lapply(oug_df, function(x) {names(x) <- name_key[names(x)]; return(x)})
  }

  oug_df <- oug_df %>% dplyr::tibble()

  names(oug_df) <- column_name

  return(oug_df)
}

#' @export
#' @rdname dot-getOrgUnitsByOrgUnitGroup
getPSNUs <- function(uids = NULL,
         fields = "id",
         rename = F, retry = 1) {

  .getOrgUnitsByOrgUnitGroup(
  "AVy8gJXym2D",
  uids = uids,
         fields = fields,
         rename = rename, retry = retry)
}

#' @export
#' @rdname dot-getOrgUnitsByOrgUnitGroup

getCommunities <- function(uids = NULL,
         fields = "id",
         rename = F, retry = 1) {

   .getOrgUnitsByOrgUnitGroup(
  "PvuaP6YALSA",
  uids = uids,
         fields = fields,
         rename = rename, retry = retry)
}

#' @export
#' @rdname dot-getOrgUnitsByOrgUnitGroup

getFacilities <- function(uids = NULL,
         fields = "id",
         rename = F, retry = 1) {

   .getOrgUnitsByOrgUnitGroup(
  "POHZmzofoVx",
    uids = uids,
         fields = fields,
         rename = rename, retry = retry)
}

#' @export
#' @rdname dot-getOrgUnitsByOrgUnitGroup

getMilitaryOrgUnits <- function(uids = NULL,
         fields = "id",
         rename = F, retry = 1) {

   .getOrgUnitsByOrgUnitGroup(
  "nwQbMeALRjL",
    uids = uids,
         fields = fields,
         rename = rename, retry = retry)
}

#' @export
#' @rdname dot-getOrgUnitsByOrgUnitGroup

getCountries <- function(uids = NULL,
         fields = "id",
         rename = F, retry = 1) {

   .getOrgUnitsByOrgUnitGroup(
     "cNzfcPWEGSH",
      uids = uids,
         fields = fields,
         rename = rename, retry = retry)
}
