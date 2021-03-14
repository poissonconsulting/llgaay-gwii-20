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
  sPopnDisperse ~ dnorm(0, 2^-2) T(0,)
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
  bEfficiencyDensity ~ dnorm(1, 1^-2)
  sEfficiencyDensityType ~ dnorm(0, 2^-2) T(0,)
  for(i in 1:nType) {
    bEfficiencyType[i] ~ dnorm(0, 2^-2)
    bEfficiencyDensityType[i] ~ dnorm(0, sEfficiencyDensityType^-2)
  }
  
  sDeerDisperse ~ dnorm(0, 2^-2) T(0,)
  for(i in 1:nObs) {
    eDensity[i] <- bPopn[Island[i],Day[i]] / Area[Island[i]]
    eEffort[i] <- Hours[i] * HourlyRate[i]
    log(eEfficiency[i]) <- bEfficiencyType[Type[i]] + (bEfficiencyDensity + bEfficiencyDensityType[Type[i]]) * (log(eDensity[i]) - log(0.3))
    eDeer[i] <- eEffort[i] * eEfficiency[i] 
    eDeerDisperse[i] ~ dgamma(sDeerDisperse^-2, sDeerDisperse^-2)
    Deer[i] ~ dpois(eDeer[i] * eDeerDisperse[i])
  }
}",
new_expr = "
  for(i in 1:nObs) {
    eDensity[i] <- bPopn[Island[i],Day[i]] / Area[Island[i]]
    eEffort[i] <- Hours[i] * HourlyRate[i]
    log(eEfficiency[i]) <- bEfficiencyType[Type[i]] + (bEfficiencyDensity + bEfficiencyDensityType[Type[i]]) * (log(eDensity[i]) - log(0.3))
    eDeer[i] <- eEffort[i] * eEfficiency[i] 
    eDeerDisperse[i] ~ dgamma(sDeerDisperse^-2, sDeerDisperse^-2)
    Deer[i] ~ dpois(eDeer[i] * eDeerDisperse[i])
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
    filter(!Type %in%  c("Opportunistic")) %>%
    mutate(Type = droplevels(Type))
  data$nObs <- length(data$Deer)
  data
},
gen_inits = function(data) {
  inits <- list()
  inits$bPopn1 <- apply(data$DeerTotal, MARGIN = 1, FUN = sum) + 1L
  inits
},
random_effects = list(bPopn = "Day",
                      bEfficiencyDensityType = "Type"),
select_data = list(`Day-` = dtt_date(paste("2017-", c("04-21", "10-06"))),
                   Island = factor("Ramsay", c("Ramsay", "Murchison", "House")),
                   Area = c(32, 1700),
                   Deer = c(0L, 15L),
                   Type = factor("Helipcopter", c("Bailing Dog", "Bait Station", "Boat", 
                                     "Helicopter", "Indicator Dog", 
                                     "Line Push", "Opportunistic", "Walking")),
                   Hours = c(0.05, 14),
                   HourlyRate = c(0.05, 1.5)),
nthin = 100L
)

sbf_save_block(template(model), "template", caption = "Model description.")
