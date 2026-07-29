if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Intern Coding Lab")

data <- read_csv("data/acs00015.csv")

## HEALTH -------------------------------------------------------------------------------------------  
# uninsured, over time
uninsured <- data %>%
  filter(age >= 18) %>%
  group_by(year) %>%
  summarise(pct = 100 * mean(hcovany == 1))   # hcovany 1 = no coverage

ggplot(uninsured, aes(year, pct)) +
  geom_line(linewidth = 1, color = "#3043B4") + geom_point() +
  labs(x = NULL, y = "% uninsured")

uninsured <- data %>%
  filter(age >= 18) %>%
  group_by(year) %>%
  summarise(pct = 100 * weighted.mean(hcovany == 1, perwt), .groups = "drop")   # hcovany 1 = no coverage

ggplot(uninsured, aes(x = as.numeric(year), y = pct)) +
  geom_line(linewidth = 1.2, color = "#3043B4") +
  geom_point(size = 2, color = "#3043B4") +
  scale_x_continuous(breaks = seq(2016, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0.02, 0)) +
  labs(
    title = "Uninsured Rate Among Adults, 2016-2024",
    subtitle = "Weighted share of adults 18+ with no health insurance coverage",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot", plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/health1.png", width = 8, height = 6)

# Kids on Medicaid/CHIP
kids <- data %>%
  filter(age < 19, statefip %in% c(48, 6, 36)) %>%   # 48 TX, 6 CA, 36 NY
  group_by(year, statefip) %>%
  summarise(pct = 100 * mean(hinscaid == 2), .groups = "drop")   # 2 = Medicaid/CHIP

ggplot(kids, aes(year, pct, color = factor(statefip))) +
  geom_line(linewidth = 1) + geom_point() +
  labs(x = NULL, y = "% of children on Medicaid", color = "State")

kids <- data %>%
  filter(age < 19, statefip %in% c(48, 6, 36)) %>%   # 48 TX, 6 CA, 36 NY
  group_by(year, statefip) %>%
  summarise(pct = 100 * weighted.mean(hinscaid == 2, perwt), .groups = "drop") %>%  # 2 = Medicaid/CHIP
  mutate(state = recode(statefip, `6` = "California", `36` = "New York", `48` = "Texas"))

ggplot(kids, aes(x = as.numeric(year), y = pct, color = state)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "California" = "#3043B4",
    "New York"   = "#C97703",
    "Texas"      = "#1B7A4B")) +
  scale_x_continuous(breaks = seq(2016, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0.02, 0)) +
  labs(
    title = "Children on Medicaid: California, New York, and Texas (2016-2024)",
    subtitle = "Share of children under 19 covered by Medicaid or CHIP (weighted)",
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
    axis.text.x = element_text(size = 10, color = "gray40"),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/health2.png", width = 8, height = 6)

#  Medicaid by employment
data %>%
  filter(year == 2024, age >= 18, age <= 64) %>%
  group_by(empstat) %>%          # 1 employed, 2 unemployed, 3 not in labor force
  summarise(pct_medicaid = 100 * mean(hinscaid == 2))

emp_medicaid <- data %>%
  filter(year == 2024, age >= 18, age <= 64) %>%
  group_by(empstat) %>%          # 1 employed, 2 unemployed, 3 not in labor force
  summarise(pct_medicaid = 100 * weighted.mean(hinscaid == 2, perwt), .groups = "drop") %>%
  mutate(status = recode(empstat,
    `1` = "Employed", `2` = "Unemployed", `3` = "Not in labor force"))

ggplot(emp_medicaid, aes(x = reorder(status, pct_medicaid), y = pct_medicaid)) +
  geom_col(fill = "#3043B4") +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Medicaid Coverage by Employment Status (2024)",
    subtitle = "Share of working-age adults (18-64) covered by Medicaid or CHIP, weighted",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/health3.png", width = 8, height = 6)

##  EDUCATION ---------------------------------------------------------------------------------------
# Income by education
inc <- data %>%
  filter(inctot != 9999999) %>%   # 9999999 = N/A
  group_by(educ) %>%
  summarise(median_income = median(inctot))

ggplot(inc, aes(factor(educ), median_income)) +
  geom_col(fill = "#0D0E51") +
  labs(x = "Education level (educ code)", y = "Median personal income")

educ_lk <- tibble::tribble(
  ~educ, ~label,
  0,  "None / preschool",
  1,  "Grade 1-4",
  2,  "Grade 5-8",
  3,  "Grade 9",
  4,  "Grade 10",
  5,  "Grade 11",
  6,  "Grade 12 / HS",
  7,  "1 yr college",
  8,  "2 yrs college",
  9,  "3 yrs college",
  10, "Bachelor's",
  11, "5+ yrs college"
)

inc <- data %>%
  filter(year == 2024, inctot != 9999999, age >= 18, age <= 64) %>%   # 2024, working-age adults; 9999999 = N/A
  group_by(educ) %>%
  summarise(median_income = matrixStats::weightedMedian(inctot, perwt), .groups = "drop") %>%
  left_join(educ_lk, by = "educ") %>%
  mutate(label = factor(label, levels = educ_lk$label))   # keep education in order

ggplot(inc, aes(x = label, y = median_income)) +
  geom_col(fill = "#0D0E51") +
  scale_y_continuous(labels = scales::label_dollar(),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Median Personal Income by Education (2024)",
    subtitle = "Working-age adults (18-64); weighted median total personal income",
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
    axis.text.x = element_text(size = 9, color = "gray40", angle = 40, hjust = 1),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/education1.png", width = 8, height = 6)

# Degrees by sex
data %>%
  filter(year == 2024, age >= 25) %>%
  group_by(sex) %>%              # 1 male, 2 female
  summarise(pct_ba = 100 * mean(educ >= 10))   # educ >= 10 ~ bachelor's+

degrees <- data %>%
  filter(year == 2024, age >= 25) %>%
  group_by(sex) %>%              # 1 male, 2 female
  summarise(pct_ba = 100 * weighted.mean(educ >= 10, perwt), .groups = "drop") %>%  # educ >= 10 ~ bachelor's+
  mutate(sex = recode(sex, `1` = "Men", `2` = "Women"))

ggplot(degrees, aes(x = sex, y = pct_ba, fill = sex)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c("Men" = "skyblue", "Women" = "pink"), guide = "none") +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Bachelor's Degree or Higher, by Sex (2024)",
    subtitle = "Share of adults 25+ with a bachelor's degree or more, weighted",
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

ggsave("Results/education2.png", width = 8, height = 6)

# Lawyers with econ degree
data %>%
  filter(age >= 25, age <= 34, occ2010 == 2100, degfieldd == 5501) %>%   # 2100 = lawyers, judges
  summarise(n = n())

# DEGFIELD field-of-degree labels (common codes; extend as needed)

lawyer_fields <- data %>%
  filter(year == 2024, age >= 25, age <= 34, occ2010 == 2100) %>%   # 2100 = lawyers, judges
  group_by(degfieldd) %>%
  summarise(population = sum(perwt), .groups = "drop") %>%           # weighted estimate of people
  slice_max(population, n = 10) %>%                                  # top 10 fields
  left_join(degfield_lk, by = "degfield") %>%
  mutate(field = coalesce(field, paste0("Field ", degfieldd)),
         is_econ = degfieldd == 5501)                                  # 54 = economics

ggplot(lawyer_fields, aes(x = reorder(field, population), y = population, fill = is_econ)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = "#C97703", `FALSE` = "#0D0E51"), guide = "none") +
  scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Field of Degree Among Young Lawyers (2024)",
    subtitle = "Lawyers and judges aged 25-34, by field of bachelor's degree (weighted); economics in orange",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 9, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/education3.png", width = 8, height = 6)

## GENERAL ECONOMICS --------------------------------------------------------------------------------
# wages over time
wages <- data %>%
  filter(incwage > 0, incwage != 999999) %>%
  group_by(year) %>%
  summarise(median_wage = matrixStats::weightedMedian(incwage, perwt), .groups = "drop")

ggplot(wages, aes(x = as.numeric(year), y = median_wage)) +
  geom_line(linewidth = 1.2, color = "#1B7A4B") +
  geom_point(size = 2, color = "#1B7A4B") +
  scale_x_continuous(breaks = seq(2016, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::label_dollar(), expand = c(0.02, 0)) +
  labs(
    title = "Median Wage Income, 2016-2024",
    subtitle = "Weighted median wage and salary income among earners",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot", plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/econ1.png", width = 8, height = 6)

# employment over time
emp <- data %>%
  filter(age >= 18, age <= 64) %>%
  group_by(year) %>%
  summarise(pct_employed = 100 * weighted.mean(empstat == 1, perwt), .groups = "drop")  # 1 = employed

ggplot(emp, aes(x = as.numeric(year), y = pct_employed)) +
  geom_line(linewidth = 1.2, color = "#1B7A4B") +
  geom_point(size = 2, color = "#1B7A4B") +
  scale_x_continuous(breaks = seq(2016, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0.02, 0)) +
  labs(
    title = "Employment Rate, 2016-2024",
    subtitle = "Share of working-age adults (18-64) employed, weighted",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot", plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/econ2.png", width = 8, height = 6)

# work hours by sex
hours <- data %>%
  filter(year == 2024, uhrswork > 0) %>%
  group_by(sex) %>%
  summarise(avg_hours = weighted.mean(uhrswork, perwt), .groups = "drop") %>%
  mutate(sex = recode(sex, `1` = "Men", `2` = "Women"))

ggplot(hours, aes(x = sex, y = avg_hours, fill = sex)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c("Men" = "#1B7A4B", "Women" = "pink"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Average Weekly Hours Worked, by Sex (2024)",
    subtitle = "Weighted mean usual hours per week among those who work",
    x = NULL, y = "Hours per week",
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    axis.title.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot", plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/econ3.png", width = 8, height = 6)

# wages by occupation
occ_wages <- data %>%
  filter(year == 2024, incwage > 0, incwage != 999999) %>%
  group_by(occ2010) %>%
  summarise(median_wage = matrixStats::weightedMedian(incwage, perwt),
            n = n(), .groups = "drop") %>%
  filter(n >= 100)                     # drop tiny, noisy occupation cells


occ_wages <- occ_wages %>%
  mutate(occ_label = as.character(occ2010))   

ends <- bind_rows(
  slice_max(occ_wages, median_wage, n = 15) %>% mutate(grp = "Highest-paid"),
  slice_min(occ_wages, median_wage, n = 15) %>% mutate(grp = "Lowest-paid"))

ggplot(ends, aes(x = reorder(occ_label, median_wage), y = median_wage, fill = grp)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Highest-paid" = "#1B7A4B", "Lowest-paid" = "#ec36a7"),
                    guide = "none") +
  facet_wrap(~ grp, scales = "free_y", ncol = 1) +
  scale_y_continuous(labels = scales::label_dollar(), expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Highest- and Lowest-Paid Occupations (2024)",
    subtitle = "Weighted median wage income; occupations with 100+ sample workers",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    strip.text = element_text(size = 11, face = "bold", hjust = 0, color = "gray30"),
    panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.text = element_text(size = 8, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot", plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/econ4.png", width = 8, height = 6)

# longest work weeks
occ_hours <- data %>%
  filter(year == 2024, uhrswork > 0) %>%
  group_by(occ2010) %>%
  summarise(avg_hours = weighted.mean(uhrswork, perwt),
            n = n(), .groups = "drop") %>%
  filter(n >= 100) %>%
  mutate(occ_label = as.character(occ2010)) %>%   
  slice_max(avg_hours, n = 15)

ggplot(occ_hours, aes(x = reorder(occ_label, avg_hours), y = avg_hours)) +
  geom_col(fill = "#1B7A4B") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Occupations with the Longest Work Weeks (2024)",
    subtitle = "Weighted mean usual hours per week; occupations with 100+ sample workers",
    x = NULL, y = "Hours per week",
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.text = element_text(size = 8, color = "gray40"),
    axis.title.x = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot", plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/econ5.png", width = 8, height = 6)

# family income by state
state_lk <- tibble::tribble(
  ~statefip, ~abb,
   1,"AL",  2,"AK",  4,"AZ",  5,"AR",  6,"CA",  8,"CO",  9,"CT", 10,"DE", 11,"DC",
  12,"FL", 13,"GA", 15,"HI", 16,"ID", 17,"IL", 18,"IN", 19,"IA", 20,"KS", 21,"KY",
  22,"LA", 23,"ME", 24,"MD", 25,"MA", 26,"MI", 27,"MN", 28,"MS", 29,"MO", 30,"MT",
  31,"NE", 32,"NV", 33,"NH", 34,"NJ", 35,"NM", 36,"NY", 37,"NC", 38,"ND", 39,"OH",
  40,"OK", 41,"OR", 42,"PA", 44,"RI", 45,"SC", 46,"SD", 47,"TN", 48,"TX", 49,"UT",
  50,"VT", 51,"VA", 53,"WA", 54,"WV", 55,"WI", 56,"WY", 72,"PR"
)

fam <- data %>%
  filter(year == 2024, ftotinc != 9999999) %>%
  group_by(statefip) %>%
  summarise(median_family_income = matrixStats::weightedMedian(ftotinc, perwt), .groups = "drop") %>%
  left_join(state_lk, by = "statefip")

ggplot(fam, aes(x = reorder(abb, median_family_income), y = median_family_income)) +
  geom_col(fill = "#25b490") +
  coord_flip() +
  scale_y_continuous(labels = scales::label_dollar(), expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Median Family Income by State (2024)",
    subtitle = "Weighted median total family income",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.text = element_text(size = 7, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot", plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/econ6.png", width = 8, height = 6)

# poverty: 1 vs 2 parent
fam_kids <- data %>%
  filter(year == 2024, ftotinc != 9999999) %>%
  group_by(nchild) %>%
  summarise(median_family_income = matrixStats::weightedMedian(ftotinc, perwt), .groups = "drop") %>%
  filter(nchild <= 5) %>%
  mutate(kids = factor(nchild, labels = c("0","1","2","3","4","5+")))

ggplot(fam_kids, aes(x = kids, y = median_family_income)) +
  geom_col(fill = "#1B7A4B") +
  scale_y_continuous(labels = scales::label_dollar(), expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Median Family Income by Number of Children (2024)",
    subtitle = "Weighted median total family income",
    x = "Children in the family", y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    axis.title.x = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot", plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/econ7.png", width = 8, height = 6)

## IMMIGRATION --------------------------------------------------------------------------------------
# foreign-born over time
trend <- data %>%
  group_by(year) %>%
  summarise(pct = 100 * mean(bpl >= 100))   # bpl >= 100 = foreign-born

ggplot(trend, aes(year, pct)) +
  geom_line(linewidth = 1, color = "#B23A48") + geom_point() +
  labs(x = NULL, y = "% foreign-born")

fb <- data %>%
  group_by(year) %>%
  summarise(pct = 100 * weighted.mean(bpl >= 100, perwt), .groups = "drop")   # bpl >= 100 = foreign-born

ggplot(fb, aes(x = as.numeric(year), y = pct)) +
  geom_line(linewidth = 1.2, color = "#B23A48") +
  geom_point(size = 2, color = "#B23A48") +
  scale_x_continuous(breaks = seq(2016, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0.02, 0)) +
  labs(
    title = "Foreign-Born Share of the Population, 2016-2024",
    subtitle = "Weighted share of people born outside the U.S.",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot", plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/immigration1.png", width = 8, height = 6)

# immigrant arrival & income
data %>%
  filter(year == 2024, citizen == 3, yrimmig > 0, inctot != 9999999) %>%   # 3 = not a citizen
  group_by(arrived = if_else(yrimmig <= 1999, "1999 or earlier", "2000 or later")) %>%
  summarise(median_income = median(inctot))

arrival <- data %>%
  filter(year == 2024, citizen == 3, yrimmig > 0, inctot != 9999999) %>%   # 3 = not a citizen
  group_by(arrived = if_else(yrimmig <= 1999, "1999 or earlier", "2000 or later")) %>%
  summarise(median_income = matrixStats::weightedMedian(inctot, perwt), .groups = "drop")

ggplot(arrival, aes(x = arrived, y = median_income, fill = arrived)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c("1999 or earlier" = "#3043B4", "2000 or later" = "#C97703"),
                    guide = "none") +
  scale_y_continuous(labels = scales::label_dollar(), expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Noncitizen Income by Time of Arrival (2024)",
    subtitle = "Weighted median personal income among noncitizens, by arrival cohort",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot", plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/immigration2.png", width = 8, height = 6)

# naturalized over time
nat <- data %>%
  filter(bpl >= 100) %>%
  group_by(year) %>%
  summarise(pct = 100 * mean(citizen == 2))   # 2 = naturalized

ggplot(nat, aes(year, pct)) +
  geom_line(linewidth = 1, color = "#B23A48") + geom_point() +
  labs(x = NULL, y = "% naturalized")

nat <- data %>%
  filter(bpl >= 100) %>%
  group_by(year) %>%
  summarise(pct = 100 * weighted.mean(citizen == 2, perwt), .groups = "drop")   # 2 = naturalized

ggplot(nat, aes(x = as.numeric(year), y = pct)) +
  geom_line(linewidth = 1.2, color = "#B23A48") +
  geom_point(size = 2, color = "#B23A48") +
  scale_x_continuous(breaks = seq(2016, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0.02, 0)) +
  labs(
    title = "Naturalized Share of the Foreign-Born, 2016-2024",
    subtitle = "Weighted share of foreign-born residents who are naturalized citizens",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot", plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/immigration3.png", width = 8, height = 6)

## TECHNOLOGY ---------------------------------------------------------------------------------------
# smartphones by state
phones <- data %>%
  filter(year == 2024, cismrtphn != 0) %>%
  group_by(statefip) %>%
  summarise(pct = 100 * mean(cismrtphn == 1))   # 1 = yes

ggplot(phones, aes(reorder(statefip, pct), pct)) +
  geom_col(fill = "#6D3FA3") + coord_flip() +
  labs(x = NULL, y = "% with a smartphone")

phones <- data %>%
  filter(year == 2024, cismrtphn != 0) %>%
  distinct(serial, .keep_all = TRUE) %>%       # one observation per household
  group_by(statefip) %>%
  summarise(pct = 100 * weighted.mean(cismrtphn == 1, hhwt),
            .groups = "drop") %>%              # 1 = yes
  left_join(state_lk, by = "statefip")

ggplot(phones, aes(x = reorder(abb, pct), y = pct)) +
  geom_col(fill = "#6D3FA3") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Households with a Smartphone, by State (2024)",
    subtitle = "Weighted share of households with a smartphone",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 7, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/tech1.png", width = 8, height = 6)

# smartphones over time
sp <- data %>%
  filter(cismrtphn != 0) %>%
  group_by(year) %>%
  summarise(pct = 100 * mean(cismrtphn == 1))

ggplot(sp, aes(year, pct)) +
  geom_line(linewidth = 1, color = "#6D3FA3") + geom_point() +
  labs(x = NULL, y = "% with a smartphone")

sp <- data %>%
  filter(cismrtphn != 0) %>%
  distinct(year, serial, .keep_all = TRUE) %>%   # one observation per household per year
  group_by(year) %>%
  summarise(pct = 100 * weighted.mean(cismrtphn == 1, hhwt),
            .groups = "drop")

ggplot(sp, aes(x = as.numeric(year), y = pct)) +
  geom_line(linewidth = 1.2, color = "#6D3FA3") +
  geom_point(size = 2, color = "#6D3FA3") +
  scale_x_continuous(
    breaks = seq(2016, 2024, by = 2),
    expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = c(0.02, 0)) +
  labs(
    title = "Smartphone Ownership, 2016-2024",
    subtitle = "Weighted share of households with a smartphone",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
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

ggsave("Results/tech2.png", width = 8, height = 6)

# smartphones by education
data %>%
  filter(year == 2024, cismrtphn != 0) %>%
  group_by(educ) %>%
  summarise(pct = 100 * mean(cismrtphn == 1))

phone_educ <- data %>%
  filter(year == 2024, age >= 25, cismrtphn != 0) %>%
  group_by(educ) %>%
  summarise(pct = 100 * weighted.mean(cismrtphn == 1, perwt),
            .groups = "drop") %>%
  left_join(educ_lk, by = "educ") %>%
  mutate(label = factor(label, levels = educ_lk$label))

ggplot(phone_educ, aes(x = label, y = pct)) +
  geom_col(fill = "#6D3FA3") +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Smartphone Access by Education (2024)",
    subtitle = "Share of adults 25+ living in a household with a smartphone, weighted",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 9, color = "gray40", angle = 40, hjust = 1),
    axis.text.y = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/tech3.png", width = 8, height = 6)

## ENERGY / ENVIRONMENT -----------------------------------------------------------------------------
# commute modes
commute <- data %>%
  filter(year == 2024, tranwork > 0) %>%
  group_by(tranwork) %>%
  summarise(n = n())

ggplot(commute, aes(reorder(tranwork, n), n)) +
  geom_col(fill = "#0E7C86") + coord_flip() +
  labs(x = "Mode (tranwork code)", y = "Workers (sample)")

commute <- data %>%
  filter(year == 2024, tranwork > 0) %>%
  mutate(mode = case_when(
    tranwork >= 10 & tranwork <= 19 ~ "Car, truck, or van",
    tranwork == 20                 ~ "Motorcycle",
    tranwork >= 30 & tranwork <= 39 ~ "Public transportation",
    tranwork == 40                 ~ "Walked",
    tranwork == 50                 ~ "Bicycle",
    tranwork == 80                 ~ "Worked from home",
    TRUE                           ~ "Other")) %>%
  group_by(mode) %>%
  summarise(workers = sum(perwt), .groups = "drop")

ggplot(commute, aes(x = reorder(mode, workers), y = workers)) +
  geom_col(fill = "#0E7C86") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_comma(),
    expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "How Americans Got to Work in 2024",
    subtitle = "Estimated number of workers by usual mode of transportation",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/energy1.png", width = 8, height = 6)

# electricity bills by state
elec <- data %>%
  filter(year == 2024, costelec > 0, costelec < 9990) %>%
  group_by(statefip) %>%
  summarise(avg_bill = mean(costelec))

ggplot(elec, aes(reorder(statefip, avg_bill), avg_bill)) +
  geom_col(fill = "#0E7C86") + coord_flip() +
  labs(x = NULL, y = "Avg annual electricity cost ($)")

elec <- data %>%
  filter(year == 2024, costelec > 0, costelec < 9990) %>%
  distinct(serial, .keep_all = TRUE) %>%       # one observation per household
  group_by(statefip) %>%
  summarise(avg_bill = weighted.mean(costelec, hhwt),
            .groups = "drop") %>%
  left_join(state_lk, by = "statefip")

ggplot(elec, aes(x = reorder(abb, avg_bill), y = avg_bill)) +
  geom_col(fill = "#0E7C86") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_dollar(),
    expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Average Annual Electricity Cost by State (2024)",
    subtitle = "Weighted mean among households that reported paying for electricity",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 7, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/energy2.png", width = 8, height = 6)

# work from home over time
wfh <- data %>%
  filter(tranwork > 0) %>%
  group_by(year) %>%
  summarise(pct_wfh = 100 * mean(tranwork == 80))   # 80 = worked at home

ggplot(wfh, aes(year, pct_wfh)) +
  geom_line(linewidth = 1, color = "#0E7C86") + geom_point() +
  labs(x = NULL, y = "% working from home")

wfh <- data %>%
  filter(tranwork > 0) %>%
  group_by(year) %>%
  summarise(
    pct_wfh = 100 * weighted.mean(tranwork == 80, perwt),
    .groups = "drop")   # 80 = worked at home

ggplot(wfh, aes(x = as.numeric(year), y = pct_wfh)) +
  geom_line(linewidth = 1.2, color = "#0E7C86") +
  geom_point(size = 2, color = "#0E7C86") +
  scale_x_continuous(
    breaks = seq(2016, 2024, by = 2),
    expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = c(0.02, 0), limits = c(0, 20)) +
  labs(
    title = "Working from Home, 2016-2024",
    subtitle = "Weighted share of workers who usually worked from home",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
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

ggsave("Results/energy3.png", width = 8, height = 6)

## BONUS --------------------------------------------------------------------------------------------
# earliest to work
data %>%
  filter(departs > 0) %>%
  group_by(occ2010) %>%
  summarise(avg_departs = mean(departs)) %>%
  arrange(avg_departs)

early <- data %>%
  filter(year == 2024, departs > 0) %>%
  mutate(
    depart_minutes = (departs %/% 100) * 60 + departs %% 100) %>%  # HHMM to minutes
  group_by(occ2010) %>%
  summarise(
    avg_minutes = weighted.mean(depart_minutes, perwt),
    n = n(),
    .groups = "drop") %>%
  filter(n >= 100) %>%
  slice_min(avg_minutes, n = 15, with_ties = FALSE) %>%
  mutate(
    occ_label = as.character(occ2010),
    avg_time = sprintf(
      "%d:%02d a.m.",
      floor(avg_minutes / 60),
      round(avg_minutes %% 60)))

ggplot(early, aes(x = reorder(occ_label, -avg_minutes), y = avg_minutes)) +
  geom_col(fill = "#C97703") +
  coord_flip() +
  geom_text(
    aes(label = avg_time),
    hjust = -0.1, size = 3, color = "gray30") +
  scale_y_continuous(
    breaks = seq(240, 600, by = 60),
    labels = function(x) sprintf("%d:00", x / 60),
    limits = c(0, max(early$avg_minutes) * 1.12),
    expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Occupations That Leave for Work Earliest (2024)",
    subtitle = "Weighted mean departure time; occupations with 100+ sample workers",
    x = NULL, y = "Average departure time",
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 8, color = "gray40"),
    axis.title.x = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/bonus1.png", width = 8, height = 6)

# night-shift states
night <- data %>%
  filter(departs > 0) %>%
  group_by(statefip) %>%
  summarise(pct = 100 * mean(departs < 500))   # before 5:00 a.m.

ggplot(night, aes(reorder(statefip, pct), pct)) +
  geom_col(fill = "#C97703") + coord_flip() +
  labs(x = NULL, y = "% leaving before 5 a.m.")

night <- data %>%
  filter(year == 2024, departs > 0) %>%
  group_by(statefip) %>%
  summarise(
    pct = 100 * weighted.mean(departs < 500, perwt),
    .groups = "drop") %>%                       # before 5:00 a.m.
  left_join(state_lk, by = "statefip")

ggplot(night, aes(x = reorder(abb, pct), y = pct)) +
  geom_col(fill = "#C97703") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Workers Leaving Home Before 5 a.m., by State (2024)",
    subtitle = "Weighted share among workers who reported a departure time",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 7, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/bonus2.png", width = 8, height = 6)

# does WFH pay?
data %>%
  filter(incwage > 0, incwage != 999999) %>%
  group_by(wfh = tranwork == 80) %>%
  summarise(median_wage = median(incwage))

wfh_pay <- data %>%
  filter(
    year == 2024,
    tranwork > 0,
    incwage > 0,
    incwage != 999999) %>%
  group_by(work_location = if_else(
    tranwork == 80, "Worked from home", "Commuted")) %>%
  summarise(
    median_wage = matrixStats::weightedMedian(incwage, perwt),
    .groups = "drop")

ggplot(wfh_pay, aes(x = work_location, y = median_wage, fill = work_location)) +
  geom_col(width = 0.65) +
  scale_fill_manual(
    values = c("Commuted" = "#0E7C86", "Worked from home" = "#C97703"),
    guide = "none") +
  scale_y_continuous(
    labels = scales::label_dollar(),
    expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Wage Income of Remote Workers and Commuters (2024)",
    subtitle = "Weighted median wage and salary income among workers with positive earnings",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
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

ggsave("Results/bonus3.png", width = 8, height = 6)

# overqualified paycheck
data %>%
  filter(year == 2024, educ < 10, incwage > 0, incwage != 999999) %>%
  group_by(occ2010) %>%
  summarise(median_wage = median(incwage)) %>%
  arrange(desc(median_wage))

no_ba_pay <- data %>%
  filter(
    year == 2024,
    age >= 25,
    educ < 10,
    incwage > 0,
    incwage != 999999) %>%
  group_by(occ2010) %>%
  summarise(
    median_wage = matrixStats::weightedMedian(incwage, perwt),
    n = n(),
    .groups = "drop") %>%
  filter(n >= 100) %>%
  slice_max(median_wage, n = 15, with_ties = FALSE) %>%
  mutate(occ_label = as.character(occ2010))

ggplot(no_ba_pay, aes(x = reorder(occ_label, median_wage), y = median_wage)) +
  geom_col(fill = "#C97703") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_dollar(),
    expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Highest-Paid Occupations for Workers Without a Bachelor's Degree (2024)",
    subtitle = "Weighted median wage income; occupations with 100+ sample workers",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 8, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/bonus4.png", width = 8, height = 6)

# hours by marital status
data %>%
  filter(uhrswork > 0) %>%
  group_by(marst) %>%
  summarise(avg_hours = mean(uhrswork))

marital_hours <- data %>%
  filter(year == 2024, uhrswork > 0) %>%
  mutate(marital_status = case_when(
    marst %in% c(1, 2) ~ "Married",
    marst %in% c(3, 4) ~ "Separated or divorced",
    marst == 5         ~ "Widowed",
    marst == 6         ~ "Never married",
    TRUE               ~ "Other")) %>%
  group_by(marital_status) %>%
  summarise(
    avg_hours = weighted.mean(uhrswork, perwt),
    .groups = "drop")

ggplot(
  marital_hours,
  aes(x = reorder(marital_status, avg_hours), y = avg_hours)) +
  geom_col(fill = "#C97703") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Average Weekly Hours Worked by Marital Status (2024)",
    subtitle = "Weighted mean usual hours per week among workers",
    x = NULL, y = "Hours per week",
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10, color = "gray40"),
    axis.title.x = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/bonus5.png", width = 8, height = 6)

# rare & transit commutes
# rare modes: 20 motorcycle, 39 ferry, 50 bicycle
data %>%
  filter(tranwork %in% c(20, 39, 50)) %>%
  group_by(tranwork) %>%
  summarise(n = n())

# occupations that use public transit most (bus/rail codes 31-37)
data %>%
  filter(tranwork %in% c(31, 32, 33, 34, 35, 36, 37)) %>%
  group_by(occ2010) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

rare_commutes <- data %>%
  filter(year == 2024, tranwork %in% c(20, 39, 50)) %>%
  mutate(mode = recode(
    tranwork,
    `20` = "Motorcycle",
    `39` = "Ferry",
    `50` = "Bicycle")) %>%
  group_by(mode) %>%
  summarise(commuters = sum(perwt), .groups = "drop")

ggplot(
  rare_commutes,
  aes(x = reorder(mode, commuters), y = commuters)) +
  geom_col(fill = "#C97703") +
  scale_y_continuous(
    labels = scales::label_comma(),
    expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Motorcycle, Ferry, and Bicycle Commuters (2024)",
    subtitle = "Weighted estimate of workers by commute mode",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
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

ggsave("Results/bonus6a.png", width = 8, height = 6)

transit_occ <- data %>%
  filter(year == 2024, tranwork > 0) %>%
  group_by(occ2010) %>%
  summarise(
    pct_transit = 100 * weighted.mean(
      tranwork %in% c(31, 32, 33, 34, 35, 36, 37), perwt),
    n = n(),
    .groups = "drop") %>%
  filter(n >= 100) %>%
  slice_max(pct_transit, n = 15, with_ties = FALSE) %>%
  mutate(occ_label = as.character(occ2010))

ggplot(
  transit_occ,
  aes(x = reorder(occ_label, pct_transit), y = pct_transit)) +
  geom_col(fill = "#0E7C86") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Occupations with the Highest Public-Transit Use (2024)",
    subtitle = "Weighted share commuting by bus or rail; occupations with 100+ sample workers",
    x = NULL, y = NULL,
    caption = "Source: ACS PUMS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0,
                                 margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 8, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("Results/bonus6b.png", width = 8, height = 6)
