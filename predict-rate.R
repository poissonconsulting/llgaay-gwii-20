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
  summarise(Deer = sum(Deer), .groups = "keep") %>%
  ungroup()

popn <- filter(coef, str_detect(term, "bPopn")) %>%
  select(estimate, lower, upper) %>%
  bind_cols(total_deer) %>%
  select(Island, Removed = Deer, estimate, lower, upper) %>%
  mutate(across(c(estimate, lower, upper), function(x) x - Removed)) %>%
  print()

sbf_save_table(popn, caption = "The total number of deer removed and the estimated number of remaining deer by island (with 98% CIs)")

efficiency <- data %>%
  mutate(Density = 0.3) %>%
  new_data(seq = c("Method", "DensityDependent"), 
           ref = list(Density = c(0.01, 0.3)), 
           obs_only = TRUE) %>%
  filter(Method != "Miscellaneous") %>%
  predict(analysis, new_data = ., new_values = list(Density = .$Density), 
                   new_expr = 
"for(i in 1:nObs) {
    log(prediction[i]) <- bEfficiencyMethod[Method[i]] + DensityDependent[i] * log(Density[i])
}") %>%
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
