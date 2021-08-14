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
sbf_save_table(coef, caption = "Model terms (with 98% CIs)")

total_deer <- data %>%
  group_by(Island) %>%
  summarise(Removed = sum(Deer), .groups = "keep") %>%
  ungroup()

remaining <- mcmc_derive_data(analysis, new_data = "Island", term = "ePopn") %>%
  select(Island) %>%
  inner_join(total_deer, by = "Island") %>%
  mcmc_derive_data(expr = "new_par <- par - Removed")

popn <- coef(remaining, simplify = TRUE, conf_level = 0.98) %>%
  select(Island, Removed, estimate, lower, upper) %>%
  print()

sbf_save_table(popn, caption = "The total number of deer removed and the estimated number of remaining deer by island (with 98% CIs)")

cost <- mcmc_derive_data(analysis, new_data = "Method", term = "eCost") %>%
  select(Method)

indicator <- cost %>%
  filter(Method == "Indicator Dog") %>%
  slice(rep(1, 3)) %>%
  as.mcmcr() %>%
  as.mcmcarray()

bailing <- cost %>%
  filter(Method == "Bailing Dog") %>%
  slice(rep(1, 3)) %>%
  as.mcmcr() %>%
  as.mcmcarray()
  
remaining_indicator <- remaining 
remaining_indicator$mcmc %<>% combine_samples(indicator, fun = prod)
remaining_indicator %<>% 
  coef(simplify = TRUE, conf_level = 0.98) %>%
  select(Island, estimate, lower, upper)

remaining_bailing <- remaining 
remaining_bailing$mcmc %<>% combine_samples(bailing, fun = prod)
remaining_bailing %<>% 
  coef(simplify = TRUE, conf_level = 0.98) %>%
  select(Island, estimate, lower, upper)

efficiency <- data %>%
  mutate(Density = 0.3) %>%
  new_data(seq = c("Method", "DensityDependent"), 
           ref = list(Density = c(0.01, 0.3)), 
           obs_only = TRUE) %>%
  filter(Method != "Miscellaneous") %>%
  predict(analysis, new_data = ., new_values = list(Density = .$Density), 
          term = "eEfficiency") %>%
  mutate(DensityLevel = case_when(
    Density == 0.01 ~ "Low Density (0.01 ind/ha)",
    Density == 0.3 ~ "High Density (0.30 ind/ha)"))

gp <- ggplot(data = efficiency, aes(x = Method, y = estimate)) +
  facet_grid(DensityLevel~., scales = "free_y") +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete("Method") +
  scale_y_continuous("Efficiency (ind/heli.hr)") +
  expand_limits(y = 0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  NULL

sbf_open_window(3,4)
sbf_print(gp)

sbf_save_plot(x_name = "efficiency", caption = "The removal efficiency by method and density")

efficiency_data <- data %>%
  new_data(seq = c("Method", "DensityDependent"), 
           ref = list(Island = unique(.$Island),
                      Day = seq(min(.$Day), max(.$Day), by = 10)), 
           obs_only = TRUE) %>%
  predict(analysis, new_data = .)

gp <- ggplot(data = data) +
  aes(x = Day, y = Deer / (Hours * HourlyRate), color = Island) +
  facet_wrap(~Method, scales = "free_y") +
  geom_point(alpha = 2/3) +
  expand_limits(y = 0) +
  scale_x_date("Date") +
  scale_color_manual(values = c("black", "blue", "red")) +
  theme(legend.position = "bottom")

gp_efficiency <- gp + 
  ylab("Efficiency (ind/heli.hr)") + 
  geom_line(data = efficiency_data, aes(y = estimate))

sbf_open_window()
sbf_print(gp_efficiency)

sbf_save_plot(x_name = "efficiency_data", caption = "The removal efficiency by date, method and island with the estimated removal efficiency")

cost_data <- data %>%
  new_data(seq = c("Method", "DensityDependent"), 
           ref = list(Island = unique(.$Island),
                      Day = seq(min(.$Day), max(.$Day), by = 10)), 
           obs_only = TRUE) %>%
  predict(analysis, new_data = ., term = "eCost")

gp_cost <- gp + 
  aes(x = Day, y = (Hours * HourlyRate) / Deer) +
  geom_line(data = cost_data, aes(y = estimate)) +
  ylab("Cost (heli.hr/ind)")

sbf_open_window()
sbf_print(gp_cost)

sbf_save_plot(x_name = "cost_data", caption = "The cost by date, method and island with the estimated cost")

gp_deer_hours <- gp + 
  aes(x = Day, y = Deer / Hours) +
  ylab("Efficiency (ind/hr)")
  
sbf_open_window()
sbf_print(gp_deer_hours)

gp_deer <- gp + 
  aes(x = Day, y = Deer) +
  ylab("Rate (ind)")

sbf_open_window()
sbf_print(gp_deer)
