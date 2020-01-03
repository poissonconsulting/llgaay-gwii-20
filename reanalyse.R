source("header.R")

warning("need to add function to subfoldr to list objects but not get")
names <- c("analysis")

# for (name in names) {
#   objects <- sbf_load_objects_recursive(name)
#   for (i in seq_len(nrow(objects))) {
#     print(objects$sub[i])
#     object <- reanalyse(objects$objects[[i]], duration = dhours(2))
#     sbf_save_object(object, name, sub = objects$sub[i])
#   }
# }
