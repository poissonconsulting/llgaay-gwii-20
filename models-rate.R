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
    bPopn1[i] ~ dpois(0.3 * Area[i] * ePopnDisperse[i])
  }
  for(i in 1:nIsland) {
    bPopn[i,1] <- bPopn1[i]
    bPopn[i,2] <- bPopn[i,1] - DeerTotal[i,1]
    for(j in 2:nDay) {
      bPopn[i,j+1] <- bPopn[i,j] - DeerTotal[i,j]
    }
  }
}",
modify_data = function(data) {
  data$Day <- data$Day + 1L
  data$nDay <- max(data$Day)

  data$Area <- tibble(Area = data$Area, Island = data$Island) %>%
    distinct() %>%
    arrange(Island) %>%
    use_series(Area)
  
  data$DeerTotal <- tibble(Island = data$Island, Day = data$Day, Deer = data$Deer) %>%
    group_by(Island, Day) %>%
    summarise(Deer = sum(Deer), .groups = "keep") %>%
    ungroup() %>%
    mutate(Day = factor(Day, levels = 1:data$nDay)) %>%
    complete(Island, Day, fill = list(Deer = 0L)) %>%
    pivot_wider(names_from = "Day", values_from = "Deer") %>%
    select(-Island) %>%
    as.matrix()
  
  event <- tibble(Island = data$Island, Day = data$Day, Type = data$Type, Deer = data$Deer) %>%
    filter(!Type %in%  c("Bailing Dog",
                         "Indicator Dog", 
                         "Line Push", "Opportunistic", "Walking")) %>%
    mutate(Type = droplevels(Type)) %>%
    arrange(Island, Day)
  
  data$Island <- event$Island
  data$Day <- event$Day
  data$Type <- event$Type
  data$Deer <- event$Deer
  
  print(data)
  data
},
gen_inits = function(data) {
  inits <- list()
  inits$bPopn1 <- apply(data$DeerTotal, MARGIN = 1, FUN = sum) + 1L
  inits
},
random_effects = list(bPopn = c("Island", "Day")),
select_data = list(`Day-` = dtt_date(paste("2017-", c("04-21", "10-06"))),
                   Island = factor("Ramsay", c("Ramsay", "Murchison", "House")),
                   Area = c(32, 1700),
                   Deer = c(0L, 15L),
                   Type = factor("Helipcopter", c("Bailing Dog", "Bait Station", "Boat", 
                                     "Helicopter", "Indicator Dog", 
                                     "Line Push", "Opportunistic", "Walking")))
)

sbf_save_block(template(model), "template", caption = "Model description.")
