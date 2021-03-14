source("header.R")

options(mb.parallel = FALSE)

sbf_set_sub("rate")

analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

glance <- glance(analysis)
coef <- coef(analysis, include_constant = FALSE, simplify = TRUE)

glance %>% print()
coef %>% print(n = nrow(.))

sbf_save_table(glance, caption = "Model convergence")
sbf_save_table(coef, caption = "Model coefficients")

total_deer <- data %>%
  group_by(Island) %>%
  summarise(Deer = sum(Deer), .groups = "keep") %>%
  ungroup()

popn <- filter(coef, str_detect(term, "bPopn")) %>%
  select(estimate, lower, upper) %>%
  bind_cols(total_deer) %>%
  select(Island, Deer, estimate, lower, upper) %>%
  print()

density <- data %>%
  mutate(Density = 0.3) %>%
  new_data(seq = c("Type", "DensityDependent"), 
           ref = list(Density = c(0.01, 0.3)), 
           obs_only = TRUE) %>%
  filter(Type != "Miscellaneous")

density %<>%
  predict(analysis, new_data = ., new_values = list(Density = .$Density), 
                   new_expr = 
"for(i in 1:nObs) {
    log(prediction[i]) <- bEfficiencyType[Type[i]] + DensityDependent[i] * bEfficiencyDensityType[Type[i]] * log(Density[i])
}")

gp <- ggplot(data = density, aes(x = Type, y = estimate)) +
  facet_grid(Density~., scales = "free_y") +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete("Hunting Method") +
  scale_y_continuous("Deer / Hourly Helicopter Team") +
  expand_limits(y = 0) +
  NULL

sbf_open_window(5,4)
sbf_print(gp)

sbf_save_plot(x_name = "density", caption = "The hunting rate by method")

gp <- ggplot(data = data, aes(x = Day, y = Deer)) +
  facet_wrap(~Type, scales = "free_y") +
  geom_point(aes(color = Island), alpha = 2/3,
             position = position_jitter(height = 0.1)) +
  expand_limits(y = 0) +
  scale_color_manual(values = c("black", "blue", "red")) +
  theme(legend.position = "bottom")

sbf_open_window()
sbf_print(gp)

gp <- gp + aes(x = Day, y = Deer / Hours)

sbf_open_window()
sbf_print(gp)

gp <- gp + aes(x = Day, y = Deer / (Hours * HourlyRate))

sbf_open_window()
sbf_print(gp)

