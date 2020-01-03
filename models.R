source("header.R")

files <- list.files(pattern = "^models[-].*[.]R$")

for (file in files) {
  message("sourcing ", file)
  try(source(file))
}
