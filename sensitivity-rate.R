source("header.R")

sbf_set_sub("rate")

analysis <- sbf_load_object("analysis")

sensitivity <- sd_priors_by(analysis, by = 10, glance = FALSE)

sbf_save_object(sensitivity)

analyses <- analyses(analysis, sensitivity)

glance <- glance(analyses, bound = TRUE)

glance %>%
  print()

sbf_save_table(glance, "sensitivity", caption = "Model sensitivity")

sbf_open_pdf("sensitivity")
plot(sensitivity)
sbf_close_pdf()
