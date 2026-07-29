## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Intern Coding Lab")

# ACS data -----------------------------------------------------------------
ddi_acs = read_ipums_ddi("data/usa_00015.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>%
    rename_with(tolower)

acs = acs %>%
  select(
    year, serial, statefip, gq, sex, age, perwt, hhwt,
    costelec, cismrtphn, pernum, marst, bpl, citizen, yrimmig,
    hcovany, hinscaid, educ, empstat, occ2010, uhrswork, 
    inctot, ftotinc, incwage, tranwork, departs, arrives,
    poverty, ind, incinvst, classwkr, classwkrd, cinethh,
    cpi99, degfield, degfieldd, nchild)

# adjust income variables for inflation (to 2024 dollars) & remove NA
acs = acs %>%
  mutate(
    inctot   = ifelse(inctot %in% c(9999999, 9999998), NA, inctot),
    ftotinc  = ifelse(ftotinc == 9999999, NA, ftotinc),
    poverty  = ifelse(poverty == 0, NA, poverty))

# 2024 factor from CPI99 = 0.531
table(acs$year, acs$cpi99)

cpi_2024 = acs$cpi99[acs$year == 2024][1]

acs = acs %>%
  mutate(
    ftotinc2024  = ftotinc  * cpi99 / cpi_2024,
    inctot2024   = inctot   * cpi99 / cpi_2024)

write_csv(acs, "data/acs00015.csv")
