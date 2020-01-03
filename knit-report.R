source("header.R")

library(poisreport)
knit_report(paste0(project, ".Rmd"), ask = FALSE)

if (require(poisblogdown)) report_to_blogdown()

report_to_directory()
report_to_directory(dir = "~/Poisson/Clients - Transfer/Analysis Template")
