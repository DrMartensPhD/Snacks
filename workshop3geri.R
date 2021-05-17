# Indlæser og klargør data ----

# Indlæser packages
library(tidyverse) # til alt lave figurer m.m.
library(Hmisc) #bruges kun til at lave errorbars - den del er deaktiveret i nuværende kode herunder


# Indløser data
geri <- read.csv("~/R/PROgrez/data/geri.csv", sep = ",")
head(geri)

# Omdøber "Intervention" i 'gruppe' til "Balanzo" i stedet
geri$gruppe <- factor(geri$gruppe,
                       levels = c("Intervention", "Kontrol"),
                       labels = c("Balanzo", "Kontrol"))


# PLOT ----
ggplot(geri, aes(gruppe, tandemchange, fill = gruppe, color = gruppe)) +
  stat_summary(fun = mean, geom="col", size = 1.2, width = 0.6) +
  # stat_summary(fun.data = mean_cl_normal, geom = "errorbar", color = "black", size = 0.8, width = 0.05) +
  stat_summary(aes(label=round(..y.., digits = 1)), fun=mean, geom="text", size=5, vjust = -0.4, color = "black") +
  labs(x = "",
       y = "Forbedring i balance, Tandem-test",
       title = "Træning med Balanzo forbedrer balancen blandt ældre indlagte",
       subtitle = "",
       caption = "") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(face = "bold", size = 16),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12)) +
  scale_fill_manual(labels = c("i","k"), values = c("deepskyblue", "coral"), name = "legend") + 
  scale_color_manual(labels = c("i","k"), values = c("dodgerblue3", "firebrick"), name = "legend") +
  scale_y_continuous(breaks=0:10*1)

# Gemmer plot
ggsave("workshop3.jpg", 
       width = 8.62,
       height = 7.27,
       units = "in",
       dpi=600)




