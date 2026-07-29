## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Intern Coding Lab")

# ACS data -----------------------------------------------------------------
ddi_acs = read_ipums_ddi("data/usa_00014.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>%
    rename_with(tolower)

acs = acs %>%
  select(
    year, serial, statefip, gq, sex, age, perwt,
    costelec, cismrtphn, pernum, marst, bpl, citizen, yrimmig,
    hcovany, hinscaid, educ, empstat, occ2010, uhrswork, 
    inctot, ftotinc, incwage, tranwork, departs, arrives,
    poverty, ind, incinvst, classwkr, classwkrd, cinethh)

write_csv(acs, "data/acs00014.csv")
