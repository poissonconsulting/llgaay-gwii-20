source("header.R")

sbf_set_sub("count")

species <- c("BT", "RB")

for (spp in species) {
  print(sbf_set_sub("count", spp))

  analysis <- sbf_load_object("analysis")
  data <- data_set(analysis)

  glance <- glance(analysis)
  coef <- coef(analysis, include_constant = FALSE)

  glance %>% print()
  coef %>% print(n = nrow(.))

  sbf_save_table(glance, caption = "Model summary")
  sbf_save_table(coef, caption = "Model coefficients")

  count <- predict(analysis, new_data = new_data(data))

  gp <- ggplot(data = count, aes(x = Species, y = estimate)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    scale_y_continuous(name = "Count") +
    expand_limits(y = 0)

  sbf_open_window(3)
  print(gp)

  sbf_save_plot(x_name = "count", report = FALSE)
}

sbf_set_sub("count")

count <- sbf_load_plots_data_recursive("count") %>%
  unnest(plots_data)

gp <- ggplot(data = count, aes(x = Species, y = estimate)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_y_continuous(name = "Count") +
  expand_limits(y = 0)

sbf_open_window(3)
print(gp)

sbf_save_plot(x_name = "all", caption = "Predicted count (with 95% CIs).")
