source("header.R")

species <- c("BT", "RB")

for (spp in species) {
  sbf_set_sub("count", spp)

  analysis <- sbf_load_object("analysis")

  analysis2 <- sd_priors_by(analysis, by = 10)
  sbf_save_object(analysis2)

  analysis2 <- sbf_load_object("analysis2")

  sbf_open_pdf("mcmc2")
  plot(analysis2)
  sbf_close_pdf()

  glance2 <- glance(analysis2)
  coef2 <- coef(analysis2, include_constant = FALSE)

  glance2 %>% print()

  sbf_save_table(glance2, report = FALSE)
  sbf_save_table(coef2, report = FALSE)

  rhat2 <- rhat(analyses(analysis, analysis2), by = "term", as_df = TRUE, bound = TRUE, param_type = "fixed")

  rhat2 %>% print(n = nrow(.))

  sbf_save_table(rhat2, caption = "R-hat values indicating sensitivity of posterior distributions to the choice of priors.")
}
