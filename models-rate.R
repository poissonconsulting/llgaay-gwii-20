source("header.R")

sbf_set_sub("rate")

description <- c(
  "`bY`" = "Intercept for `eY`",
  "`bX`" = "Effect of `X` on `bY`",
  "`sY`" = "SD of residual variation in `Y`",
  "`eY[i]`" = "Expected value of `y[i]`",
  "`Y[i]`" = "The `i`^th^ Y value"
)

description <- tibble(
  Parameter = names(description),
  Description = description
)

description %<>% arrange(Parameter)

sbf_save_table(description, caption = "Parameter descriptions.")

model <- model("model{
  sPopnDisperse ~ dnorm(0, 1^-2) T(0,)
  for(i in 1:nIsland) {
    ePopnDisperse[i] ~ dgamma(sPopnDisperse^-2, sPopnDisperse^-2)
    bPopn[i] ~ dpois(0.3 * Area[i] * ePopnDisperse[i])
  }
  for(i in 1:nIsland) {
    ePopn[i,1] <- bPopn[i]
    ePopn[i,2] <- ePopn[i,1] - DeerTotal[i,1]
    for(j in 2:nDay) {
      ePopn[i,j+1] <- ePopn[i,j] - DeerTotal[i,j]
    }
  }
}",
modify_data = function(data) {
  data$Day <- data$Day + 1L
  data$nDay <- max(data$Day)

  data$Area <- data[c("Area", "Island")] %>%
    as_tibble() %>%
    distinct() %>%
    arrange(Island) %>%
    use_series(Area)
  
  data$DeerTotal <- data[c("Island", "Day", "Deer")] %>%
    as_tibble() %>%
    group_by(Island, Day) %>%
    summarise(Deer = sum(Deer), .groups = "keep") %>%
    ungroup() %>%
    mutate(Day = factor(Day, levels = 1:data$nDay)) %>%
    complete(Island, Day, fill = list(Deer = 0L)) %>%
    pivot_wider(names_from = "Day", values_from = "Deer") %>%
    select(-Island) %>%
    as.matrix()
  
  data[c("Island", "Day", "Type", "Deer", "Hours")] %<>% 
    as_tibble() %>%
    filter(!Type %in%  c("Bailing Dog",
                         "Indicator Dog", 
                         "Line Push", "Opportunistic", "Walking")) %>%
    mutate(Type = droplevels(Type))
  data
},
gen_inits = function(data) {
  inits <- list()
  inits$bPopn1 <- apply(data$DeerTotal, MARGIN = 1, FUN = sum) + 1L
  inits
},
select_data = list(`Day-` = dtt_date(paste("2017-", c("04-21", "10-06"))),
                   Island = factor("Ramsay", c("Ramsay", "Murchison", "House")),
                   Area = c(32, 1700),
                   Deer = c(0L, 15L),
                   Type = factor("Helipcopter", c("Bailing Dog", "Bait Station", "Boat", 
                                     "Helicopter", "Indicator Dog", 
                                     "Line Push", "Opportunistic", "Walking")),
                   Hours = c(0.05, 14))
)

sbf_save_block(template(model), "template", caption = "Model description.")
