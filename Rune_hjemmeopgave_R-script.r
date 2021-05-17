# Packages - skal installeres først, se herunder ----
# ADMINISTRATOR-RETTIGHEDER: Tools -> Install packages -> Vælg "tidyverse" package -> Vælg C-drev i "Install to library"
library(tidyverse)

# Klargør data ----
# Indlæser data fra mappe: hjemmeopgave.txt
data <- read.csv("~/R/PROgrez/data/hjemmeopgave.txt", sep="")

# Laver 'Rygning'-variabel, som bare er 'ryger'-variabel med labels
data$Rygning <- factor(data$ryger,
                     levels = c("nej", "ja"),
                     labels = c("Ikke-rygere", "Rygere"))


# Scatterplot med regressionslinjer ----
data %>% 
  ggplot(aes(uge, vaegt, group = Rygning, color = Rygning, fill = Rygning)) +
  geom_smooth(method=lm) +
  geom_point() +
  labs(x = "Gestationsalder i uger",
       y = "Barnets vægt i gram",
       title = "Workshop 1: Scatterplot (med regressionslinjer)",
       subtitle = "Fødselsvægt og gestastionsalder ved fødslen",
       caption = "Rune | PROgrez") +
  theme_bw() +
  scale_color_manual(values=c("dodgerblue3", "firebrick")) +
  scale_fill_manual(values=c("deepskyblue", "coral"))


# Gemmer plot
ggsave('Rune_scatterplot_R.png', 
       width = 20.0,
       height = 14.0,
       units = "cm",
       dpi=600)
