# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Intern Coding Lab")

# Packages
install.packages("tidyverse")
library(tidyverse)

# load the data
data <- read_csv("data/acs00015.csv")

##########################################################

table(data$year, data$marst)

count(data, year, marst) %>% print(n = Inf)

count(data, hinscaid)

summary(data$inctot)

mean(data$age)
median(data$age)
median(data$inctot)
