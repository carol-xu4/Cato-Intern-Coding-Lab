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
