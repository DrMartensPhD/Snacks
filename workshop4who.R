# Indlæser packages
library(tidyverse)
library(readxl)
library(reshape2)
library(scales)


# Klargøring af datasæt til workshop 4 ----
# Indlæser data til figur 3 fra fil med klippet tabel
who <- read_excel("~/R/PROgrez/data/wk4WHO.xlsx")

# Laver data om til long-format
wholong <- melt(who, id.vars=c("Cause", "Type"),
             variable.name="Region",
             value.name="Deaths")


wholong$Deaths <- wholong$Deaths*1000


wholong$Population <- ifelse(wholong$Region == "African Region", 1019920205,
                             ifelse(wholong$Region == "Region of the Americas", 992028000,
                                    ifelse(wholong$Region == "South-East Asia Region", 1947631000,
                                           ifelse(wholong$Region == "European Region", 916166000,
                                                  ifelse(wholong$Region == "Eastern Mediterranean Region", 664335000,
                                                         1889788000)))))




# Gemmer data som .csv
# [,] betyder at vi medtager alle rækker (før komma) og kolonner (efter komma). Alternativt "[,c("var1", "var2")]"
write.csv(wholong[,],"~/R/PROgrez/data/wholong.csv", row.names = FALSE)

# Indlæser data ----

wholong <- read.csv("~/R/PROgrez/data/wholong.csv", sep = ",")
head(wholong)


# PLOT 1/P ----
p <- ggplot(wholong, aes(stringr::str_wrap(Region, 12), reorder(Cause, desc(Cause)), color = stringr::str_wrap(Type,20), size = Deaths)) +
        geom_count() +
        labs(x = "",
             y = "",
             title = "Deaths by cause in WHO regions in 2016",
             subtitle = "Bubble size denotes number of deaths") +
        theme_classic() +
        theme(plot.title = element_text(size = 12),
              plot.subtitle = element_text(size = 10)) +
        scale_size_continuous(
                breaks = c(1000000,2000000,3000000,4000000,5000000), 
                labels=c("1,000,000", "2,000,000", "3,000,000", "4,000,000", "5,000,000"), 
                name = "Deaths in 2016", 
                range = c(1,10)) +
        scale_color_brewer(palette="Dark2", name = "Type") +
        scale_y_discrete(expand = expansion(mult = c(0.03, .05))) +
        guides(colour = guide_legend(override.aes = list(size = 7)))
p

# Gemmer plot
ggsave('p.jpg', 
       width = 24.0,
       height = 16.0,
       units = "cm",
       dpi=300)

# PLOT 2/Q ----
# laver ny variabel med dødsfald/population
wholong$deathsperpop <- wholong$Deaths/wholong$Population

# Selve plottet
q <- ggplot(wholong, aes(deathsperpop, reorder(Cause, desc(Cause)), 
                         color = reorder(stringr::str_wrap(Region, 15),Population),  
                         size = Deaths)) +
        geom_count(alpha = 0.6) +
        labs(x = "",
             y = "",
             title = "Percentage mortality in WHO regions for different causes of death",
             subtitle = "Deaths/Population * 100%") +
        theme_classic() +
        theme(plot.title = element_text(size = 12),
              plot.subtitle = element_text(size = 10)) +
        scale_size_continuous(
                breaks = c(1000000,2000000,3000000,4000000,5000000), 
                labels=c("1,000,000", "2,000,000", "3,000,000", "4,000,000", "5,000,000"), 
                name = "Deaths in 2016", 
                range = c(1,10)) +
        scale_color_brewer(palette="Dark2", name = "Region")+
        guides(color = guide_legend(override.aes = list(size = 8))) +
        scale_y_discrete(expand = expansion(mult = c(0.03, .05))) +
        scale_x_continuous(labels = percent)
q

# Gemmer plot
ggsave('q.jpg', 
       width = 28,
       height = 18,
       units = "cm",
       dpi=300)


# PLOT 3/R ----
r <- ggplot(wholong, aes(Deaths, reorder(Cause, desc(Cause)), fill=Region)) +
        geom_col(position = "fill") + 
        labs(x = "",
             y = "",             
             title = "Distribution among WHO regions for different causes of death",
             subtitle = "Percentage of deaths occuring in each region") +
        theme_bw() +
        theme(plot.title = element_text(size = 10),
              plot.subtitle = element_text(size = 8)) +
        scale_fill_brewer(palette="Dark2", name = "WHO Region") +
        scale_x_continuous(labels = percent)
r

# Gemmer plot
ggsave('r.jpg', 
       width = 22,
       height = 12,
       units = "cm",
       dpi=300)

# PLOT 4/S ----
# Finder de 8 mest dødelige sygdomme
agg <- aggregate(Deaths ~ Cause, wholong, sum)
agg[order(agg$Deaths, decreasing = TRUE),] # Top 5 skyld i >3 mio. dødsfald

# Laver variabel hvor dem, der ikke er top 5, hedder "Other"
wholong <- mutate(wholong, Causeother = ifelse(Cause %in% c("Cardiovascular diseases",
                                                            "Malignant neoplasms",
                                                            "Infectious and parasitic diseases",
                                                            "Respiratory diseases",
                                                            "Unintentional injuries"), wholong$Cause, "Other"))

# Ændrer rækkefølgen, så "Other" er til sidst
wholong$Causeother <- factor(wholong$Causeother, levels = c("Cardiovascular diseases",
                                                            "Infectious and parasitic diseases",
                                                            "Malignant neoplasms",
                                                            "Respiratory diseases",
                                                            "Unintentional injuries",
                                                            "Other"), ordered=TRUE)



s <- ggplot(wholong, aes(stringr::str_wrap(Region, 15), Deaths, fill=Causeother)) +
        geom_col(position = "fill") + 
        labs(x = "",
             y = "",             
             title = "Distribution of top 5 global causes of deaths in WHO regions",
             subtitle = "Causes of >3 million deaths in 2016 worldwide") +
        theme_bw() +
        theme(plot.title = element_text(size = 10),
              plot.subtitle = element_text(size = 8)) +
        scale_fill_brewer(palette="Dark2", name = "Cause of death") +
        scale_y_continuous(labels = percent)
s

# Gemmer plot
ggsave('s.jpg', 
       width = 22,
       height = 12,
       units = "cm",
       dpi=300)

# PLOT 5/T ----
t <- ggplot(wholong, aes(stringr::str_wrap(Region, 12), Deaths, fill=Causeother)) +
        geom_col(position = "Dodge") + 
        labs(x = "",
             y = "",             
             title = "Mortality for top 5 global causes of deaths in WHO regions",
             subtitle = "Causes of >3 million deaths in 2016 worldwide") +
        theme_bw() +
        theme(plot.title = element_text(size = 10),
              plot.subtitle = element_text(size = 8)) +
        scale_fill_brewer(palette="Dark2", name = "Cause of death") +
        scale_y_continuous(labels = comma, breaks = 0:12*500000)
t

# Gemmer plot
ggsave('t.jpg', 
       width = 22,
       height = 12,
       units = "cm",
       dpi=300)

