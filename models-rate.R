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
  for(i in 1:nIsland) {
    bDensity1[i] ~ dunif(0.01, 1.00)
    bPopn1[i] ~ dpois(Area[i] * bDensity1[i]) T(PopnMin[i],)
  }
  for(i in 1:nIsland) {
    bPopn[i,1] <- bPopn1[i]
    bDensity[i,1] <- bPopn[i,1] / Area[i]
    for(j in 2:nDay) {
      bPopn[i,j] <- bPopn[i,j-1] - DeerTotal[i,j-1]
      bDensity[i,j] <- bPopn[i,j] / Area[i]
    }
  }
  for(i in 1:nType) {
    bEfficiencyType[i] ~ dnorm(0, 5^-2)
  }
  
  sDeerDisperse ~ dnorm(0, 2^-2) T(0,)
  for(i in 1:nObs) {
    eDensity[i] <- bDensity[Island[i],Day[i]]
    eEffort[i] <- Hours[i] * HourlyRate[i]
    log(eEfficiency[i]) <- bEfficiencyType[Type[i]] + DensityDependent[i] * log(eDensity[i])
    eDeer[i] <- eEffort[i] * eEfficiency[i] 
    eDeerDisperse[i] ~ dgamma(sDeerDisperse^-2, sDeerDisperse^-2)
    Deer[i] ~ dpois(eDeer[i] * eDeerDisperse[i])
  }
}",
new_expr = "
  for(i in 1:nObs) {
    eDensity[i] <- bDensity[Island[i],Day[i]]
    eEffort[i] <- Hours[i] * HourlyRate[i]
    log(eEfficiency[i]) <- bEfficiencyType[Type[i]] + DensityDependent[i] * log(eDensity[i])
    eDeer[i] <- eEffort[i] * eEfficiency[i] 
    predict[i] <- eDeer[i]
    fit[i] <- predict[i]
    residual[i] <- res_gammma_pois(Deer[i], fit[i], sDeerDisperse)
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
  
  data$PopnMin <- data[c("Island", "Deer")] %>%
    as_tibble() %>%
    group_by(Island) %>%
    summarise(Deer = sum(Deer), .groups = "keep") %>%
    ungroup() %>%
    use_series("Deer")
  
  data
},
modify_new_data = function(data) {
  data$Day <- data$Day + 1L
  data$nDay <- max(data$Day)
  
  data
},
gen_inits = function(data) {
  inits <- list()
  inits$bPopn1 <- apply(data$DeerTotal, MARGIN = 1, FUN = sum) + 1L
  inits
},
random_effects = list(bPopn = "Day",
                      bDensity = "Day"),
select_data = list(`Day-` = dtt_date(paste("2017-", c("04-21", "10-06"))),
                   Island = factor(""),
                   Area = c(32, 1700),
                   Deer = c(0L, 15L),
                   Type = factor(""),
                   Hours = c(0.05, 14),
                   HourlyRate = c(0.05, 1.5),
                   DensityDependent = TRUE),
nthin = 100L
)

sbf_save_block(template(model), "template", caption = "Model description.")
