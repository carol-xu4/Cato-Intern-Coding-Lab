# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Intern Coding Lab")

# Packages
install.packages("tidyverse")

library(tidyverse)

# load the data
data = read_csv("data/acs00015.csv")

# How did the number of adults enrolled in Medicaid change over time?
# For every year: group_by(year) first, then count within each
medicaid_sex_year <- data %>%
  filter(age >= 18, hinscaid == 2) %>%
  group_by(year, sex) %>%
  summarise(n = n())

print(medicaid_sex_year, n = Inf)
