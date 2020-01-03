source("models-count.R")

species <- c("BT", "RB")

for (spp in species) {
  print(sbf_set_sub("count", spp))

  data <- sbf_load_data("data")

  analysis <- analyse(model, data = data)
  sbf_save_object(analysis)

  sbf_open_pdf("mcmc")
  plot(analysis)
  sbf_close_pdf()
}
