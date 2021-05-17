# Packages - skal installeres først, se herunder ----
# ADMINISTRATOR-RETTIGHEDER: Tools -> Install packages -> Vælg "tidyverse" package -> Vælg C-drev i "Install to library"
library(tidyverse)

# Indlæsning af data, 3 eksempler ----
# 1. Indlæser data fra mappe
data <- read.csv("~/R/PROgrez/data/repeatedpulse.csv", sep=",")

# 2. Indlæser csv-fil direkte fra hjemmeside-URL
data <- read.csv(url("https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/RepeatedPulse.csv"))

# 3. Indlæser csv-fil fra hjemmeside med readr:: (en del af tidyverse package)
data <- readr::read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/RepeatedPulse.csv")


# Plots ----
data %>% 
  ggplot(aes(Time, Pulse, group = Day, color = Day)) +
  geom_line() +
  geom_point() +
  # stat_summary(fun = mean) +
  theme_bw()

data %>% 
  ggplot(aes(Time, Pulse, group = 1)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun_data = mean_cl_normal, geom = "ribbon", alpha = 0.1) + 
  theme_bw()

# geom_errorbar??

