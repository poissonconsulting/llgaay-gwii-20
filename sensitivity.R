source("header.R")

files <- list.files(pattern = "^sensitivity[-].*[.]R$")

for (file in files) {
  message("sourcing ", file)
  try(source(file))
}
