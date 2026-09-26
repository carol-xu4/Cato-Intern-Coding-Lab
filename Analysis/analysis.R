# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Intern Coding Lab")

# Packages
install.packages("tidyverse")
library(tidyverse)

# load the data
data = read_csv("data/acs00015.csv")

nrow(data)

dim(data)

names(data)

count(data, sex)

count(data, hinscaid)

# How many people are enrolled in Medicaid?
data %>%
    filter(hinscaid == 2) %>%
    summarise(n = n())

# How many adults were enrolled in Medicaid in 2024?
data %>%
  filter(year == 2024, age >= 18, hinscaid == 2) %>%   # hinscaid 2 = Medicaid/CHIP
  summarise(n = n())

# How did the number of adults enrolled in Medicaid change over time?
# For every year: group_by(year) first, then count within each
medicaid_by_year <- data %>%
  filter(age >= 18, hinscaid == 2) %>%
  group_by(year) %>%
  summarise(n = n())

print(medicaid_by_year, n = Inf)

write_csv(medicaid_by_year, "results/medicaid_year.csv")
