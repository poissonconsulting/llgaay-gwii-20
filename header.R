library(poispkgs)

stopifnot(packageVersion("poispkgs") >= "0.0.1.9017")

if(!"package:conflicted" %in% search()) {
  source("conflict-prefer.R")
}

if (getDoParWorkers() == 1) {
  message("registering 4 workers")
  registerDoParallel(4)
}

options(sbf.ask = FALSE)
options(tibble.print_max = 20)

palette(c(
  "black", "red", "blue", "green4", "brown", "slategray", "orchid",
  "lightskyblue", "aquamarine4", "orange3"
))

theme_set(theme_Poisson())

set_analysis_mode("report")

sbf_set_main("output")
sbf_reset_sub()

dtt_set_default_tz("Etc/GMT+8")

rm(list = ls())
graphics.off()

source("functions.R")

project <- basename(getwd())
project <- "analysis-template" # so runs with new working directory

Sys.setenv("SLACK_CHANNEL" = project)

dir <- paste0("~/Poisson/Data/", sub("-\\d\\d$", "", project))

year <- 2019
