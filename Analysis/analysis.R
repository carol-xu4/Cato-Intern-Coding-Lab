# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Intern Coding Lab")

# Packages
install.packages("tidyverse")
library(tidyverse)

# load the data
data = read_csv("data/acs00015.csv")

##########################################################
nrow(data)
dim(data)
names(data)
head(data)
count(data, statefip)

table(data$year, data$marst)

count(data, year, marst)

summary(data$inctot)   # min, 1st quartile, median, mean, 3rd quartile, max

mean(data$age)
median(data$inctot, na.rm = TRUE)    # the middle income

state_counts <- count(data, statefip)
print(state_counts)

write_csv(state_counts, "Results/state_counts.csv")

income_by_educ <- data %>%
  filter(inctot != 9999999) %>%      # drop the "N/A" income code
  group_by(educ) %>%
  summarise(median_income = median(inctot))

print(income_by_educ)

# share of adults uninsured, by state
uninsured_by_state <- adults %>%
  group_by(statefip) %>%
  summarise(pct = 100 * mean(hcovany == 1))   # hcovany 1 = no coverage

# to rank the result, add arrange() to sort high-to-low
uninsured_by_state %>% arrange(desc(pct)) %>% print(n = Inf)

# In 2024: how many adults were on Medicaid?
data %>%
  filter(year == 2024, age >= 18, hinscaid == 2) %>%   # hinscaid 2 = Medicaid/CHIP
  summarise(n = n())

# For every year: group_by(year) first, then count within each
medicaid_by_year <- data %>%
  filter(age >= 18, hinscaid == 2) %>%
  group_by(year) %>%
  summarise(n = n())

print(medicaid_by_year, n = Inf)

# a real example: build a summary table from our data first
medicaid_by_year <- data %>%
  filter(age >= 18, hinscaid == 2) %>%
  group_by(year) %>%
  summarise(n = n())

# lines -- a trend over time
ggplot(medicaid_by_year, aes(x = year, y = n)) +
  geom_line() + geom_point()

# bars -- the same numbers as columns
ggplot(medicaid_by_year, aes(x = factor(year), y = n)) +
  geom_col()

# weighted this time: sum perwt for a population estimate
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

# one row per year per group
medicaid_grp <- data %>%
  filter(hinscaid == 2) %>%
  group_by(year, group = if_else(age < 19, "Kids", "Adults")) %>%
  summarise(n = n(), .groups = "drop")

print(medicaid_grp)

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
