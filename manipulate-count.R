source("header.R")

sbf_set_sub("tidy")

count <- sbf_load_data("count")

species <- unique(count$Species)
for (spp in species) {
  print(sbf_set_sub("count", spp))

  data <- filter(count, Species == spp)

  sbf_save_data(data)

  gp <- ggplot(data = data, aes(x = Count)) +
    geom_histogram(binwidth = 1) +
    scale_x_continuous(name = "Count") +
    scale_y_continuous(name = "Frequency")

  sbf_open_window(3, 3)
  print(gp)

  sbf_save_plot(x_name = "data", report = FALSE)
}
