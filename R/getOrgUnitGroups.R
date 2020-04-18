"api/organisationUnitGroups/cNzfcPWEGSH.csv?fields=organisationUnits[name,id,path]"
countries <-
  datapackr::api_call("organisationUnits") %>%
  datapackr::api_filter(field = "organisationUnitGroups.id",
                        operation = "eq",
                        match = "cNzfcPWEGSH") %>%
  datapackr::api_fields(fields = "id,name,level,ancestors[id,name]") %>%
  datapackr::api_get() %>%
  
  dplyr::filter(
    !name %in% 
      c("Antigua & Barbuda","Bahamas","Belize","China","Dominica","Grenada",
        "Saint Kitts & Nevis","Saint Lucia","Saint Vincent & the Grenadines",
        "Turkmenistan","Uzbekistan")) %>%
  dplyr::select(country_name = name, country_uid = id, dplyr::everything()) %>%
  
  
  countries <-
  datapackr::api_call("organisationUnits") %>%
  datapackr::api_filter("organisationUnitGroups.name:eq:Country") %>%
  datapackr::api_filter(paste0("children.id:in:[",site_uids,"]")) %>%
  datapackr::api_fields("id,name") %>%
  datapackr::api_get()

dplyr::if_else(include_mil, ",nwQbMeALRjL", ""),
"]")) %>%
  datapackr::api_fields("id,name,level,ancestors[id,name],organisationUnitGroups[id,name]") %>%
  datapackr::api_get() %>%
  dplyr::mutate(
    country_uid =
      stringr::str_extract(as.character(ancestors),
                           paste0(country_uids,collapse = "|")))


&filter=name:$ilike:_Military",
#"&filter=organisationUnitGroups.id:eq:nwQbMeALRjL", (New _Mil nodes not here...)
"&fields=name,id,level,ancestors[id,name]") 


getOrgUnitGroups(c("cNzfcPWEGSH", "nwQbMeALRjL"), fields = "name,id,organisationUnits[name,id,path]" )
- default is to use ids for the primary filter
- use in for the filter as eq is a subset of in
- Should we unnest somehow, if so probably have a parameter for unnest with a default of TRUE
- baseurl/api/organisationUnitGroups.json?paging=false&filter=id:in:[cNzfcPWEGSH,nwQbMeALRjL]&fields=name,id,organisationUnits[name,id,path]

getOrgUnitGroups(c("Country", "Military"), by = "name", fields = "name,id,organisationUnits[name,id,path]" )
- filtering on name instead of ID so we add by prameter
- baseurl/api/organisationUnitGroups.json?paging=false&filter=name:in:[Country,Military]&fields=name,id,organisationUnits[name,id,path]

getOrgUnitGroups(c("cNzfcPWEGSH", "nwQbMeALRjL"))
- default fields are name and ID
- baseurl/api/organisationUnitGroups.json?paging=false&filter=id:in:[cNzfcPWEGSH,nwQbMeALRjL]&fields=name,id]

getOrgUnitGroups2(.x = c("cNzfcPWEGSH", "nwQbMeALRjL"), .y = c("Military"), .x_by = "id", .y_by = "name")
- This one will have two filters, we don't seem to have a use case for more filters than that right now, and one can always further filter in their code
- use .x and .y as input parameters as in purrr:map2
- default fields are name and ID
- should we still assume .x_by has a default of id?
- baseurl/api/organisationUnitGroups.json?paging=false&filter=id:in:[cNzfcPWEGSH,nwQbMeALRjL]&filter=name:in:[Military]&fields=name,id







