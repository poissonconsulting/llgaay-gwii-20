source("header.R")
source("models-rate.R")

sbf_set_sub("rate")
data <- sbf_load_data("data")

analysis <- analyse(model, data = data)
sbf_save_object(analysis)

sbf_open_pdf("mcmc")
plot(analysis)
sbf_close_pdf()
