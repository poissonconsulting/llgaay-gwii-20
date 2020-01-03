source("header.R")

dbs <- sbf_list_dbs(recursive = TRUE)

if(identical(dbs, file.path(sbf_get_main(), "dbs", paste0(project, ".sqlite")))) {
  stop("not yet implemented")
  # first it needs to archive existing one.... or not??
  # then copy to databases
  # then issue slack message
} else {
  if(length(dbs) == 1) {
    warning("database does not match project and/or is in subfolder")
  } else if(!length(dbs)) {
    warning("no databases")
  } else
    warning("more than one database")
}
