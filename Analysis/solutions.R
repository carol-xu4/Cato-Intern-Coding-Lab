if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Intern Coding Lab")

data <- read_csv("data/acs00014.csv")

## HEALTH -------------------------------------------------------------------------------------------  
# uninsured, by state
uninsured <- data %>%
  filter(year == 2024, age >= 18) %>%
  group_by(statefip) %>%
  summarise(pct = 100 * mean(hcovany == 1))   # hcovany 1 = no coverage

ggplot(uninsured, aes(reorder(statefip, pct), pct)) +
  geom_col(fill = "#3043B4") + coord_flip() +
  labs(x = NULL, y = "% uninsured")

# Kids on Medicaid/CHIP


#  Medicaid by employment


##  EDUCATION ---------------------------------------------------------------------------------------

## GENERAL ECONOMICS --------------------------------------------------------------------------------

## IMMIGRATION --------------------------------------------------------------------------------------

## TECHNOLOGY ---------------------------------------------------------------------------------------

## ENERGY / ENVIRONMENT -----------------------------------------------------------------------------

## BONUS --------------------------------------------------------------------------------------------