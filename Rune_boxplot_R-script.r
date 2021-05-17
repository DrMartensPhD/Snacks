# Indlæser Packages - skal installeres først, se herunder ----
# 1. Åbn RStudio med ADMINISTRATOR-RETTIGHEDER: 
# 2. Vælg i topmenu: Tools -> Install packages -> Vælg package -> Vælg C-drev i "Install to library"
library(tidyverse)
library(gganimate) #SKAL INSTALLERE PACKAGE "gifski" for at gganimate fungere, oplevede jeg

# Klargør data ----
# Indlæser data fra mappe: hjemmeopgave.txt
data <- read.csv("~/R/PROgrez/data/hjemmeopgave.txt", sep="")

# Laver 'Rygning'-variabel, som bare er 'ryger'-variabel med labels
data$Rygning <- factor(data$ryger,
                     levels = c("nej", "ja"),
                     labels = c("Ikke-rygere", "Rygere"))

# Finder median-alder til at stille opgave
summary(data$alder) # Median alder 27,02 år

# Laver uge til factor/kategorisk variabel (kun til bruge, hvis x=uge)
data$uge <- as.factor(data$uge)

# Laver ny variabel ("cat") til dikotomiseret alder(bruges i panel-boxplot)
data$alder_gr <- ifelse(data$alder >27,
                        c("o. 27"),
                        c("u. 27"))

# Ændrer rækkefølge i alder_gr variabel
data$alder_gr <- factor(data$alder_gr, levels=c("u. 27", "o. 27"), ordered=TRUE)


# Laver ny variabel ("cat") til (bruges til 4-box per uge)
data$cat <- ifelse(data$alder >27 & data$ryger == "ja", "Ryger, over 27", 
                   ifelse(data$alder <=27 & data$ryger == "ja", "Ryger, under 27",
                          ifelse(data$alder >27 & data$ryger == "nej", "Ikke-ryger, over 27","Ikke-ryger, under 27"))                   )

# tjek af cat variabel
table(data$cat,data$alder_gr)
table(data$cat,data$ryger) # Alt stemmer!


# Ændrer rækkefølge i cat variabel
data$cat <- factor(data$cat, levels=c("Ryger, under 27", "Ikke-ryger, under 27", "Ryger, over 27", "Ikke-ryger, over 27"), ordered=TRUE)


# Laver funktion til at sætte n ind i plot med stat_summary/geom="text"
n_fun <- function(x){
  return(data.frame(y = 5500,
                    label = length(x)))
}

# Laver plot ----
# Boxplot ALDER
ggplot(data, aes(x=uge, y=vaegt, fill = alder_gr)) +
  geom_boxplot(width = 0.8, outlier.size = 2) +
  stat_summary(fun.data = n_fun, geom = "text",
               aes(group=alder_gr),
               hjust = 0.5, position = position_dodge(0.8)) + 
  labs(x = "Gestationsalder i uger",
     y = "Barnets vægt i gram",
     title = "Workshop 2: Boxplot",
     subtitle = "Fødselsvægt og gestastionsalder fordelt på aldersgruppe",
     caption = "Rune | PROgrez") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="Moders alder", values=c("deepskyblue", "dodgerblue3")) +
  scale_y_continuous(breaks=0:30*250)

# Tjek at n passer
table(data$alder_gr,data$uge) # Det gør det!

# Gemmer plot
ggsave('Rune_boxplot_R_alder.png', 
       width = 27.0,
       height = 19.05,
       units = "cm",
       dpi=600)

# Boxplot ALDER&RYGNING
ggplot(data, aes(x=uge, y=vaegt, fill = cat)) +
  geom_boxplot(width = 0.8, outlier.size = 2) +
  stat_summary(fun.data = n_fun, geom = "text",
               aes(group=cat),
               hjust = 0.5, position = position_dodge(0.8)) +  
  labs(x = "Gestationsalder i uger",
       y = "Barnets vægt i gram",
       title = "Workshop 2: Boxplot",
       subtitle = "Fødselsvægt og gestastionsalder fordelt på aldersgruppe og rygestatus",
       caption = "Rune | PROgrez") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("coral", "deepskyblue", "firebrick","dodgerblue3")) +
  scale_y_continuous(breaks=0:30*250)

# Tjek at n passer
table(data$cat,data$uge)

# Gemmer plot
ggsave('Rune_boxplot_R_alder-rygning.png', 
       width = 27.0,
       height = 19.05,
       units = "cm",
       dpi=600)


# Boxplot ALDER&RYGNING i paneler
ggplot(data, aes(x=alder_gr, y=vaegt, fill = Rygning )) +
  geom_boxplot(width = 0.5) +
  labs(x = "Født af kvinde over/under 27 år",
       y = "Barnets vægt i gram",
       title = "Workshop 2: Boxplot",
       subtitle = "Fødselsvægt fordelt på aldersgruppe og rygestatus for børn født i gestationsuge 37-42",
       caption = "Rune | PROgrez") +
  facet_wrap(~uge)  +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("deepskyblue", "coral")) +  scale_y_continuous(breaks=0:40*400)


# Gemmer plot
ggsave('Rune_boxplot_R_paneler.png', 
       width = 27.0,
       height = 19.05,
       units = "cm",
       dpi=600)


# Anitmate----
p <- # Boxplot ALDER&RYGNING
  ggplot(data, aes(x=uge, y=vaegt, fill = cat)) +
  geom_boxplot(width = 0.8, outlier.size = 2) +
  stat_summary(fun.data = n_fun, geom = "text",
               aes(group=cat),
               hjust = 0.5, position = position_dodge(0.8)) +  
  labs(x = "Gestationsalder i uger",
       y = "Barnets vægt i gram",
       title = "Workshop 2: Boxplot",
       subtitle = "Fødselsvægt og gestastionsalder fordelt på aldersgruppe og rygestatus",
       caption = "Rune | PROgrez") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name="", values=c("coral", "deepskyblue", "firebrick","dodgerblue3")) +
  scale_y_continuous(breaks=0:30*250)

anim <- p +
  transition_states(alder_gr,
                    transition_length = 1,
                    state_length = 3) +
  enter_fade() +
  exit_disappear(early = TRUE)


animate(anim, width = 1000, height = 1000, res = 150)

anim_save("Rune_boxplot_gif.gif")

