# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Intern Coding Lab")

# Packages
install.packages("tidyverse")
library(tidyverse)

# load the data
data <- read_csv("data/acs00015.csv")

##########################################################
nrow(data)

dim(data)

names(data)

head(data)

data$age

count(data, sex)

count(data, hinscaid)

count(data, statefip)

table(data$year, data$marst)

count(data, year, marst) %>% print(n = Inf)

summary(data$inctot)

mean(data$age)

median(data$inctot, na.rm = TRUE)

state_counts = count(data, statefip)

write_csv(state_counts, "Results/state_counts.csv")

count(data, sex)

data %>% count(sex)

adults = data %>% filter(age >= 18)

transit <- data %>% filter(tranwork %in% c(31, 32, 33, 34, 35, 36, 37))

income_by_educ = data %>%
    filter(inctot != 9999999) %>%
    group_by(educ) %>%
    summarise(median_income = median(inctot))

income_by_educ

uninsured_by_state <- adults %>%
  group_by(statefip) %>%
  summarise(pct = 100 * mean(hcovany == 1))   # hcovany 1 = no coverage

uninsured_by_state %>% arrange(desc(pct))

data %>%
  filter(year == 2024, age >= 18, hinscaid == 2) %>%   # hinscaid 2 = Medicaid/CHIP
  summarise(n = n())

medicaid_by_year <- data %>%
  filter(age >= 18, hinscaid == 2) %>%
  group_by(year) %>%
  summarise(n = n())

print(medicaid_by_year, n = Inf)

medicaid_by_year <- data %>%
  filter(age >= 18, hinscaid == 2) %>%
  group_by(year) %>%
  summarise(n = n())

ggplot(medicaid_by_year, aes(x = year, y = n)) +
  geom_line() + geom_point()

ggplot(medicaid_by_year, aes(x = factor(year), y = n)) +
  geom_col()

medicaid_rates <- data %>%
  group_by(year) %>%
  summarise(
    n = n(),                                     # survey responses (unweighted)
    pop = sum(perwt),                            # weighted population estimate
    pct = 100 * weighted.mean(hinscaid == 2, perwt),   # weighted % on Medicaid -- 2 = Medicaid/CHIP
    .groups = "drop")

medicaid_rates

count(data, year)

medicaid_wt <- data %>%
  filter(age >= 18, hinscaid == 2) %>%
  group_by(year) %>%
  summarise(population = sum(perwt))

ggplot(medicaid_wt, aes(x = as.numeric(year), y = population)) +
  geom_line(linewidth = 1.2, color = "#3043B4") +
  geom_point(size = 2, color = "#3043B4") +
  scale_x_continuous(breaks = seq(2016, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"),
                     limits = c(NA, 44000000),
                     expand = expansion(mult = c(0.08, 0))) +
  labs(
    title = "Adults on Medicaid, 2016-2024",
    subtitle = "Weighted population estimate (ACS)",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/medicaid_adults.png", width = 8, height = 5)


medicaid_grp <- data %>%
  filter(hinscaid == 2) %>%
  group_by(year, group = if_else(age < 19, "Kids", "Adults")) %>%
  summarise(n = n(), .groups = "drop")

ggplot(medicaid_grp, aes(x = as.numeric(year), y = n, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Adults" = "#3043B4",
    "Kids"   = "#C97703")) +
  scale_x_continuous(breaks = seq(2016, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::comma, limits = c(2000, 6000), expand = c(0, 0)) +
  labs(
    title = "On Medicaid: Adults vs. Kids (2016-2024)",
    subtitle = "ACS sample counts",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/medicaid_adults_vs_kids.png", width = 8, height = 5)

uninsured <- data %>%
  filter(age >= 18) %>%
  group_by(year) %>%
  summarise(pct = 100 * mean(hcovany == 1))   # hcovany 1 = no coverage

ggplot(uninsured, aes(year, pct)) +
  geom_line(linewidth = 1, color = "#3043B4") + geom_point() +
  labs(x = NULL, y = "% uninsured")

kids <- data %>%
  filter(age < 19, statefip %in% c(48, 6, 36)) %>%   # 48 TX, 6 CA, 36 NY
  group_by(year, statefip) %>%
  summarise(pct = 100 * mean(hinscaid == 2), .groups = "drop")   # 2 = Medicaid/CHIP

ggplot(kids, aes(year, pct, color = factor(statefip))) +
  geom_line(linewidth = 1) + geom_point() +
  labs(x = NULL, y = "% of children on Medicaid", color = "State")
