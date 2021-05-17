library(tidyverse)
library(tidytuesdayR)

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2020-11-03')
tuesdata <- tidytuesdayR::tt_load(2020, week = 45)

ikea.csv <- tuesdata$ikea.csv


library(tidyverse)
# Or read in the data manually
ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

# Creating variabel with price in USD by multiplying with 0.27 
# (exchange rate Saudi Riyals to US dollars November 3 2020. 
# Though it should be notet prices are from 4/20/2020)
ikea$price_usd <- ikea$price * 0.27


# Plot - Avg. price in each category ----
ikea %>%  
  ggplot(aes(reorder(category, desc(category)), price_usd))+
  geom_bar(stat = "summary", fun.y = "mean", fill="gold", color="navyblue", size=1)+
  labs(x = "",
       y = "Avg. item price, USD",
       caption = "@runemandersen",
       title = "Average IKEA item price by category",
       subtitle = "#TidyTuesday - Week 45") +
  theme_bw()+
  coord_flip()

# Saving plot
ggsave('tidytuesday45.png', dpi=1000)
