nrow(data)

names(data)

count(data, year)

table(data$year, data$marst)
count(data, year, marst)

income_by_educ <- data %>%
  filter(inctot != 9999999) %>%      # drop the "N/A" income code
  group_by(educ) %>%
  summarise(median_income = median(inctot))

income_by_educ 

summary(data$inctot)

adults     <- data %>% filter(age >= 18)

uninsured_by_state <- adults %>%
  group_by(statefip) %>%
  summarise(pct = 100 * mean(hcovany == 1))   # hcovany 1 = no coverage

print(uninsured_by_state, n = Inf)
