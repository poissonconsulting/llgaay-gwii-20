source("header.R")
library(styler)

transformers <- tidyverse_style(strict = FALSE)

transformers$space$add_space_after_for_if_while <- NULL

style_dir(filetype = c(".R", ".Rmd"), transformers = transformers)
