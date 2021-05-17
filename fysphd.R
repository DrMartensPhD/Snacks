library(tidyverse)
library(readr)
library(stringr)
library(rvest)
library(gganimate)
library(magick)
library(viridis)

# KILDE: https://www.fysio.dk/fafo/ph.d.-oversigt/fysioterapeuter-med-afsluttet-phd
# KILDE2: https://www.statistikbanken.dk/10137


# Tekstfelt for hver afsluttet ph.d. hentes ud som "body"
body <- read_html("https://www.fysio.dk/fafo/ph.d.-oversigt/fysioterapeuter-med-afsluttet-phd") %>% 
    html_nodes(".panel-body") %>% # The relevant tag - FUNDET MED GOOGLE CHROME Developer Tools, Ctrl + Shift + I derefter Inspect (Ctrl + Shift + C)
  html_text() %>%
  str_trim %>%   # Trim additional white space
  unlist()   # Convert the list into a vector

# Navn på fysioterapeuter med afsluttet ph.d. hives ud som "name"
name <- read_html("https://www.fysio.dk/fafo/ph.d.-oversigt/fysioterapeuter-med-afsluttet-phd") %>% 
  html_nodes(".panel-heading") %>%
  html_text() %>% 
  str_trim %>% 
  unlist()

# Kombinerer navn og tekst i et datafram "fysphd"
fysphd <- as.data.frame(cbind(
 name,
 body
))


# Hiver årstal ud af text og laver til ny variabel
fysphd$year <- ifelse(grepl("^.*Afsluttet.*?([0-9]+).*", fysphd$body), # først tjekkes om der er et tal efter "Afsluttet"
       gsub("^.*Afsluttet.*?([0-9]+).*", "\\1", fysphd$body), # Det første tal efter "Afsluttet" hentes ud
       "NA") # Tilfælde uden tal (første linje) erstattes med NA

# Angiver year manuelt for de 2 tilfælde, der mangler
# Peter Stubbs ph.d. er fra 2011:
# https://profiles.uts.edu.au/Peter.Stubbs
# Inge ris ph.d. er fra 2016:
# https://www.fysio.dk/fafo/afhandlinger/phd
fysphd$year <- replace(fysphd$year, 62,'2016')
fysphd$year <- replace(fysphd$year, 117,'2011')

# Laver ny dataframe med optælling per år og cumulativ (Cum) optælling
newdf <- fysphd %>% group_by(year) %>%
  summarise(Count=n()) %>% ungroup() %>%
  mutate(Cum=cumsum(Count))

# Laver year som numerisk
newdf$year = as.numeric(as.character(newdf$year))

# Plot med points
p <- ggplot(newdf, aes(year, Cum, group = 1)) +
  geom_line(size = 1, color = "#C41130") +
  geom_point(size = 2) +
  labs(x = "År",
       y = "Kumuleret antal ph.d.'er",
       title = "Fysioterapeuter i Danmark med en ph.d.",
       subtitle ="Kilde: https://www.fysio.dk/fafo/ph.d.-oversigt/fysioterapeuter-med-afsluttet-phd") +
  theme_bw()

p

# Saving plot
ggsave('fysphd_plot.png', 
       width = 16,
       height = 12,
       units = "cm",
       dpi=1200)



# Animate hhv. med og uden årstal som subtitle, der tikker op
anim <- p + transition_reveal(year)
# anim <- p + transition_reveal(year) + labs(title = "Fysioterapeuter i Danmark med en ph.d.", subtitle = "År: {as.integer(frame_along)}")

animate(anim, width = 1000, height = 750, res = 150, fps = 15, end_pause = 15)

anim_save("fysphd_anim.gif")


# ALLE PHD EFTER HOVEDOMRÅDE 1996-2019 ----
dst <- read.csv("~/R/Snacks/data/dst.csv", sep=";", header = FALSE)
names(dst) <- c("Hovedområde", "year", "Count")
dst$Cum <- ave(dst$Count, dst$Hovedområde, FUN=cumsum)


r <- ggplot(dst, aes(year, Cum, group = Hovedområde, colour = stringr::str_wrap(Hovedområde,14))) +
  geom_line(size = 0.8) +
  geom_point(size = 2, show.legend = FALSE) +
  labs(x = "År",
       y = "Kumuleret antal nye ph.d.'er",
       title = "Tildelte ph.d.-grader i Danmark 1996-2019",
       subtitle ="Kilde: https://www.statistikbanken.dk/10137") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_x_continuous(limits=c(1995,2026)) +
  # scale_color_viridis(discrete = TRUE, option = "A") # ALTERNATIV: 
  scale_color_brewer(palette = "Dark2", name = "Hovedområde") +
  geom_label(aes(label = Hovedområde), hjust = -0.05, size = 3.5, alpha = 0.6, show.legend = FALSE)

r


# Animate 
anim2 <- r + transition_reveal(year)

animate(anim2, width = 1000, height = 1000, res = 150, fps = 15, end_pause = 20)

anim_save("dst_anim.gif")

# KOMBINERE DE TO MED MAGICK(fik ikke til at fungere) ---- 
# a_gif <- animate(anim, width = 1200, height = 900, res = 150, end_pause = 10)
# b_gif <- animate(anim2, width = 1200, height = 900, res = 150, end_pause = 10)
# 
# a_mgif <- image_read(a_gif)
# b_mgif <- image_read(b_gif)
# 
# new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
# for(i in 2:100){
#   combined <- image_append(c(a_mgif[i], b_mgif[i]))
#   new_gif <- c(new_gif, combined)
# }
# 
# new_gif