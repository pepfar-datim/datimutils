getPSNUs <- function(ancestor_uids = NULL,
         ancestor_names = NULL,
         fields = "id",
         rename = TRUE) {



  #error if UID and names argument are both non null
  if(!(is.null(ancestor_names)) && !(is.null(ancestor_uids)))
  {stop("Can only specify ancestor_uids or ancestor_names, not both")}


  psnus <- getOrgUnitGroups(dplyr::coalesce(ancestor_uids, ancestor_names),
                     fields = fields)

  # rename id to psnu_id and name to psnu_name)
  if(rename){
  psnus <- psnus %>% rename(psnu_id = id, psnu_name = name )}
  
  
  return(psnus)

}
