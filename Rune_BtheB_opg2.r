# Indlæser Packages - skal installeres først, se herunder ----
# 1. Åbn RStudio med ADMINISTRATOR-RETTIGHEDER: 
# 2. Vælg i topmenu: Tools -> Install packages -> Vælg package -> Vælg C-drev i "Install to library"
library(tidyverse)
library(reshape2)
library(Hmisc)
library(gganimate)

# Indlæser csv-fil direkte fra hjemmeside-URL
data <- read.csv(url("https://vincentarelbundock.github.io/Rdatasets/csv/HSAUR/BtheB.csv"))

# KONVERTERER TIL 'LONG'-FORMAT
# Starter med at lave id ('X') til factor
data$X <- factor(data$X)

# Bruger reshape2 package til at lave til long-format
data_long <- melt(data, id.vars=c("X", "treatment", "drug", "length"),
                  variable.name="time",
                  value.name="bdi")

# Rename the factor levels of the variable ("time") column
levels(data_long$time)[levels(data_long$time)=="bdi.pre"] <- "Pre"
levels(data_long$time)[levels(data_long$time)=="bdi.2m"] <- "2m"
levels(data_long$time)[levels(data_long$time)=="bdi.4m"] <- "4m"
levels(data_long$time)[levels(data_long$time)=="bdi.6m"] <- "6m"
levels(data_long$time)[levels(data_long$time)=="bdi.8m"] <- "8m"

# Putter labels på treatment
data_long$treatment <- factor(data_long$treatment,
                       levels = c("BtheB", "TAU"),
                       labels = c("Beat the Blues", "Treatment as usual"))


# Laver plot ----
data_long %>% 
  ggplot(aes(time, bdi, group = treatment, color = treatment)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position = position_dodge(0.2)) +
  stat_summary(fun = mean, geom = "point", size = 4, position = position_dodge(0.2)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", size = 0.5, width = 0.15, position = position_dodge(0.2)) +
  labs(x = "Time of measurement",
       y = "Beck Depression Inventory",
       title = "Workshop 2: Repeated Measurements",
       subtitle = "Gennemsnitlig BDI over tid",
       caption = "Rune | PROgrez") +
  theme_bw() +
  theme(legend.justification=c(1.1,1.1),
        legend.position=c(1,1)) +
  scale_color_manual(values=c("darkslateblue", "goldenrod2"), name = "Treatment") +
  scale_y_continuous(breaks=0:20*2)



# Gemmer plot
ggsave('Rune_BtheB_R.png', 
       width = 28.0,
       height = 19.05,
       units = "cm",
       dpi=600)




# med shapes for sjov ----
data_long %>% 
  ggplot(aes(time, bdi, group = treatment, color = treatment, shape=treatment)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position = position_dodge(0.2)) +
  stat_summary(fun = mean, geom = "point", size = 4, position = position_dodge(0.2)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", size = 0.5, width = 0.15, position = position_dodge(0.2)) +
  labs(x = "Time of measurement",
       y = "Beck Depression Inventory",
       title = "Workshop 2: Repeated Measurements",
       subtitle = "BDI over tid",
       caption = "Rune | PROgrez") +
  theme_bw() +
  theme(legend.justification=c(1.1,1.1),
        legend.position=c(1,1)) +
  scale_color_manual(values=c("darkslateblue", "goldenrod2"), name = "Treatment") +
  scale_shape_manual(values=c(17, 15), name = "Treatment") +
  scale_y_continuous(breaks=0:20*2)

  