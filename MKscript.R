# PACKAGES ----
# Kun første gang: Installér Tidyverse-package
install.packages("tidyverse")
install.packages("readxl")
install.packages("reshape2")

# Indlæs Tidyverse-package
library(tidyverse)
library(readxl)
library(reshape2)


# FIGUR 2 ----

# Indlæser data manuelt
fig2 <- data.frame(
  "level" = c("Inactive", "Insufficiently active", "Active", "Highly active"), 
  "n" = c(44, 234, 108, 185), 
  "who" = c("Not meeting", "Not meeting", "Meeting", "Meeting"),
    )
fig2$level <- factor(fig2$level, levels = c("Inactive", "Insufficiently active", "Active", "Highly active"))

# Danner plot SORT/HVID
ggplot(fig2,aes(level, n, fill = who)) +
  geom_col() +
  geom_label(aes(label = n), fill = "white", vjust = 1.5) +
  labs(x = "",
       y = "") +
  theme_classic() +
  theme(legend.justification=c(1.1,1.1),
        legend.position=c(1,1)) +
  scale_fill_manual(values=c("grey19", "grey59"), name = "Guideline recommendations") + 
  scale_y_continuous(breaks=0:20*25)

# Gemmer figur
ggsave("fig2_sort-hvid.jpg", 
       width = 15,
       height = 15,
       units = "cm",
       dpi=600)

# Danner plot FARVER
ggplot(fig2,aes(level, n, fill = level)) +
  geom_col() +
  geom_label(aes(label = n), fill = "white", vjust = 1.5) +
  labs(x = "",
       y = "") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_fill_manual(labels = c("Not meeting", "Not meeting", "Meeting", "Meeting"), values = c("firebrick", "coral", "dodgerblue2", "navy"), name = "Guideline recommendations") + 
  scale_y_continuous(breaks=0:20*25)

# Gemmer figur
ggsave("fig2_farver.jpg", 
       width = 18,
       height = 18,
       units = "cm",
       dpi=600)


# FIGUR 3----
   
# Indlæser data til figur 3 fra fil med klippet tabel
tabel <- read_excel("~/R/MK-studie/tabel.xlsx")

# Laver data om til long-format
fig3 <- melt(tabel, id.vars=c("vigorous"),
                  variable.name="moderate",
                  value.name="n")

# Ændrer rækkefølge på vigorous levels
fig3$vigorous <- factor(fig3$vigorous, levels=c("0", "<30", "30-59", "60-120", ">120"), ordered=TRUE)

 

# Laver ny variabel ("level") til vores 4 levels
fig3 <- mutate(fig3, level = ifelse(vigorous %in% "0" & moderate %in% c("0","<30"), "Inactive", 
                                     ifelse(vigorous %in% c("60-120") & moderate %in% c("151-300"),"Highly active",
                                            ifelse(moderate %in% c(">300"),"Highly active",
                                                   ifelse(vigorous %in% c(">120") & moderate %in% c("30-60","61-90","91-150","151-300"),"Highly active",
                                                          ifelse(vigorous %in% c(">120") & moderate %in% c("0","<30"),"Active",
                                                                 ifelse(vigorous %in% c("60-120") & moderate %in% c("30-60","61-90","91-150"),"Active",
                                                                        ifelse(vigorous %in% c("30-59") & moderate %in% c("91-150","151-300"),"Active",
                                                                               ifelse(vigorous %in% c("0", "<30") & moderate %in% c("151-300"),"Active",
                                                                                      "Insufficiently active")))))))))
# Ændrer rækkefølge af level variabel
fig3$level <- factor(fig3$level, levels=c("Inactive", "Insufficiently active", "Active", "Highly active"), ordered=TRUE)


# KOMMANDO TIL AT LAVE WHO-VARIABEL
fig3 <- mutate(fig3, who = ifelse(level %in% c("Inactive", "Insufficiently active"), "Not meeting", "Meeting"))
  

# Laver n om til ekstra rows
fig3uncount <- fig3 %>%
  uncount(n)

# Tjekker at antal passer
table(fig3uncount$level)    # Det gør de!


# FIGUR 3----

# Fig3-JITTER ----
ggplot(fig3uncount, aes(moderate, vigorous, color = level)) +
  geom_jitter(position = position_jitter(width = .25, height = .25), size = 3.5, alpha = 0.6) +
  labs(x = "Moderate-intensity physical activity, minutes/week",
       y = "Vigorous-intensity physical activity, minutes/week") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_color_manual(values=c("firebrick", "coral", "dodgerblue2", "navy"), name = "")


# Gemmer figur
ggsave("fig3.jpg", 
       width = 14,
       height = 14,
       units = "cm",
       dpi=600)


# Figur 3 med SHAPES
ggplot(fig3uncount, aes(moderate, vigorous, color = level, shape = level)) +
  geom_jitter(position = position_jitter(width = .25, height = .25), size = 4, alpha = 0.6) +
  labs(x = "Moderate-intensity physical activity, minutes/week",
       y = "Vigorous-intensity physical activity, minutes/week") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("grey59", "grey59", "grey19", "grey19"), name = "") +
  scale_shape_manual(values = c(15, 19, 17, 18), name = "")


# Gemmer figur
ggsave("fig3_shapes.jpg", 
       width = 14,
       height = 14,
       units = "cm",
       dpi=600)





# Figur 3 som kugler ----
ggplot(fig3, aes(moderate, vigorous, color = level, size = n)) +
  geom_count() +
  labs(x = "Moderate-intensity physical activity, minutes/week",
       y = "Vigorous-intensity physical activity, minutes/week") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_color_manual(values=c("firebrick", "tomato", "dodgerblue3", "navy"), name = "") +
  scale_size_continuous(breaks = NULL, name = "", range = c(6,24)) +
  geom_text(aes(label = n), color = "#ffffff", size = 4, fontface = 2) +
  guides(colour = guide_legend(override.aes = list(size = 6)))


ggsave("fig3_balls_farver.jpg", 
       width = 18,
       height = 18,
       units = "cm",
       dpi=600)

# Som sort/hvid 4 kat
ggplot(fig3, aes(moderate, vigorous, color = level, size = n)) +
  geom_count() +
  labs(x = "Moderate-intensity physical activity, minutes/week",
       y = "Vigorous-intensity physical activity, minutes/week") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("grey65", "grey45", "grey25", "grey5"), name = "") +
  scale_size_continuous(breaks = NULL, name = "", range = c(6,24)) +
  geom_text(aes(label = n), color = "#ffffff", size = 4, fontface = 2) +
  guides(colour = guide_legend(override.aes = list(size = 6)))


# Som sort/hvid 4 kat SHAPES
# ggplot(fig3, aes(moderate, vigorous, color = who, size = n, shape = level)) +
#   geom_count() +
#   labs(x = "Moderate-intensity physical activity, minutes/week",
#        y = "Vigorous-intensity physical activity, minutes/week") +
#   theme_classic() +
#   theme(legend.position = "top") +
#   scale_color_manual(values = c("grey19", "grey59"), name = "") +
#   scale_size_continuous(breaks = NULL, name = "", range = c(6,24)) +
#   geom_text(aes(label = n), color = "#ffffff", size = 4, fontface = 2) +
#   scale_shape_manual(values = c(15, 19, 17, 18), name = "") +
#   guides(color = guide_legend(override.aes = list(size = 6)))


ggsave("fig3_balls4_sort-hvid.jpg", 
       width = 18,
       height = 18,
       units = "cm",
       dpi=600)


# Som sort/hvid, 2 kat
ggplot(fig3, aes(moderate, vigorous, color = who, size = n)) +
  geom_count() +
  labs(x = "Moderate-intensity physical activity, minutes/week",
       y = "Vigorous-intensity physical activity, minutes/week") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("grey19", "grey59"), name = "Guideline recommendations") +
  scale_size_continuous(breaks = NULL, name = "", range = c(6,24)) +
  geom_text(aes(label = n), color = "#ffffff", size = 4, fontface = 2) +
  guides(colour = guide_legend(override.aes = list(size = 6)))


ggsave("fig3_balls2_sort-hvid.jpg", 
       width = 18,
       height = 18,
       units = "cm",
       dpi=600)
