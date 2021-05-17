# This weeks web-address:
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-11-10/readme.md

# load packages
library(tidyverse)
library(gganimate)
theme_set(theme_bw())

# read the data manually
mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')

# Creating plot
plot <-   ggplot(mobile, aes(year,mobile_subs, group=entity)) +
        geom_line(data = filter(mobile, entity!="Denmark"),color = "gray", alpha = 0.7) +
        geom_line(data = filter(mobile, entity=="Denmark"),color = "#C60C30", size=1) +
        labs(x = "Year",
             y = "Mobile subscriptions per 100 people",
             title = "Mobile subscriptions over time, Denmark vs. The World",
             subtitle = "#TidyTuesday - Week 46",
             caption = "Twitter: @runemandersen") +
        theme_bw() +
        annotate("text",x=1995, y=50, label = "Denmark", size = 4 , color="#C60C30")


# Animating plot
anim <-  plot + transition_reveal(year)

animate(anim, width = 800, height = 800, res = 150)

anim_save("wk46b.gif")
  
