source("header.R")

sbf_set_sub("count")

description <- c(
  "`bCount`" = "Intercept for `log(eCount)`",
  "`sDispersion`" = "SD of `eDispersion`",
  "`eDispersion[i]`" = "Overdispersion of `i`^th^ count",
  "`eCount[i]`" = "Expected value of `i`^th^ count",
  "`Count[i]`" = "The `i`^th^ count"
)

description <- tibble(
  Parameter = names(description),
  Description = description
)

description %<>% arrange(Parameter)

sbf_save_table(description, caption = "Parameter descriptions.")

model <- model("model{
  bCount ~ dnorm(0, 5^-2)
  sDispersion ~ dnorm(0, 5^-2)

  for (i in 1:length(Count)) {
    log(eCount[i]) <- bCount
    eDispersion[i] ~ dgamma(exp(sDispersion)^-2, exp(sDispersion)^-2)
    Count[i] ~ dpois(eCount[i] * eDispersion[i])
  }
}",
  new_expr = "
for(i in 1:length(Count)) {
    prediction[i] <- exp(bCount)
    fit[i] <- prediction[i]
    residual[i] <- (Count[i] - fit[i]) / sqrt(fit[i] + (fit[i] * exp(sDispersion))^2)
}"
)

sbf_save_block(template(model), "template", caption = "Model description.")
