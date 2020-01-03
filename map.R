source("header.R")

sbf_set_sub("map")

sbf_save_png(file.path("~/Poisson/Spatial/", project, "map.png"),
  x_name = "map",
  caption = "A map")
