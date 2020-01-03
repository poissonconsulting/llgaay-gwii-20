source("header.R")

files <- list.files(pattern = "^predict[-].*[.]R$")

for (file in files) {
  message("sourcing ", file)
  try(source(file))
}
