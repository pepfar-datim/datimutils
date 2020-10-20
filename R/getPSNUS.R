#' @title .renameFields
#' @description renames columns in dataframes within list
#' @param df orgunitgroup to use
#' @param name_key c(current_name1 = "rename1", current_name2 = "rename2")
#' @return list of dataframes with renamed columns

.renameFields <- function(df, name_key){
  df <- lapply(df, function(x) {names(x) <- name_key[names(x)]; return(x)})
  return(df)
}
#' @title .getOrgUnitsByOrgUnitGroup
#' @description wrapper to getOrgUnitGroups that retrieves org unit groups
#' @param org_unit_group orgunitgroup to use
#' @param uids vector of which to retrieve ancestors for
#' @param fields - the fields of information for those ancestors
#' @param rename c(current_name1 = "rename1", current_name2 = "rename2")
#' @param retry the number of times to try the call
#' @importFrom dplyr %>%
#' @return nested data frame with org unit group for provided uids

.getOrgUnitsByOrgUnitGroup <- function(
  org_unit_group,
  uids = NULL,
         fields = "id",
         rename = NULL, retry = 1) {

  to_subset <- fields
  fields <- paste0(fields, collapse = ",")
  fields <- paste0("organisationUnits[path," ,fields,"]")

  oug_list <- getOrgUnitGroups(org_unit_group,
                          fields = fields, retry = retry)

  oug_df<- purrr::map(uids, ~dplyr::filter(oug_list,
                               stringr::str_detect(path, .x)) %>%
                                 dplyr::select(to_subset))

  if(!(is.null(rename))){
  oug_df <- .renameFields(oug_df, rename)
  }

  oug_df <- oug_df %>% dplyr::tibble()

  return(oug_df)
}

#' @export
#' @rdname dot-getOrgUnitsByOrgUnitGroup
getPSNUs <- function(uids = NULL,
         fields = "id",
         rename = c(id = "psnu_id", name = "psnu_name", path = "path"),
                     retry = 1) {

  oug_df <- .getOrgUnitsByOrgUnitGroup(
  "AVy8gJXym2D",
  uids = uids,
         fields = fields,
         rename = rename, retry = retry)

   names(oug_df) <- "psnus"
    return(oug_df)
}

#' @export
#' @rdname dot-getOrgUnitsByOrgUnitGroup

getCommunities <- function(uids = NULL,
         fields = "id",
         rename = c(id = "community_id", name = "community_name", path = "path"),
                           retry = 1) {

   .getOrgUnitsByOrgUnitGroup(
  "PvuaP6YALSA",
  uids = uids,
         fields = fields,
         rename = rename, retry = retry)

  names(oug_df) <- "communities"
    return(oug_df)
}

#' @export
#' @rdname dot-getOrgUnitsByOrgUnitGroup

getFacilities <- function(uids = NULL,
         fields = "id",
         rename = c(id = "facility_id", name = "facility_name", path = "path"),
                          retry = 1) {

   .getOrgUnitsByOrgUnitGroup(
  "POHZmzofoVx",
    uids = uids,
         fields = fields,
         rename = rename, retry = retry)

  names(oug_df) <- "facilities"
    return(oug_df)
}

#' @export
#' @rdname dot-getOrgUnitsByOrgUnitGroup

getMilitaryOrgUnits <- function(uids = NULL,
         fields = "id",
         rename = c(id = "military_id", name = "military_name", path = "path"),
                                retry = 1) {

   .getOrgUnitsByOrgUnitGroup(
  "nwQbMeALRjL",
    uids = uids,
         fields = fields,
         rename = rename, retry = retry)

  names(oug_df) <- "militaries"
    return(oug_df)
}

#' @export
#' @rdname dot-getOrgUnitsByOrgUnitGroup

getCountries <- function(uids = NULL,
         fields = "id",
         rename = c(id = "country_id", name = "country_name", path = "path"),
                         retry = 1) {

   .getOrgUnitsByOrgUnitGroup(
     "cNzfcPWEGSH",
      uids = uids,
         fields = fields,
         rename = rename, retry = retry)

  names(oug_df) <- "countries"
    return(oug_df)
}
