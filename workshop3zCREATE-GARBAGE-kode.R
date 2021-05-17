# Indlæser og klargør data ----

# Indlæser packages
library(haven) # til at indlæse .dta fil
library(tidyverse) # til alt lave figurer m.m.

# Import af .dta fil (ret sti til, kræver 'haven' package):
geri <- read_dta("~/R/PROgrez/data/Geriatric(ny).dta")

# Kigger på data
view(geri)
head(geri)
summary(geri)

# Omkoder Kn og udskrevdest1:
# Laver køn (Kn - 0=men, 1=women) og
# udskrivelsesdestination (udskrdest1 - 1=eget hjem, 0=udksrevet til ikke-eget hjem) 
geri$Kn <- factor(geri$Kn,
                    levels = c(0,1),
                    labels = c("Mand", "Kvinde"))

geri$udskrdest1 <- factor(geri$udskrdest1,
                            levels= c(0,1),
                            labels= c("Udskrevet til eget hjem", "Anden udskrivelse"))


# Laver random gruppe-variabel
geri$gruppe <- sample(2, size = nrow(geri), replace = TRUE)
geri$gruppe <- factor(geri$gruppe,
                  levels = c(1,2),
                  labels = c("Intervention", "Kontrol"))

# Tjekker om der er forskel mellem grupper på tandemchange m.fl.
t.test(tandemchange~gruppe, data=geri)
chisq.test(table(geri$gruppe, geri$udskrdest1))
t.test(Indlvarighed~gruppe, data=geri)


# Gemmer data som .csv
# "[,c()]" specificierer hvilke kolonner gemmes (det før , er rækker, hvor vi ønsker alle)
write.csv(geri[,c("Alder", "Kn", "gruppe", "Indlvarighed", "udskrdest1", "Tandemstart", "Tandemslut", "tandemchange")],"~/R/PROgrez/data/geri.csv", row.names = FALSE)

# Tjekker gemte data
test <- read.csv("~/R/PROgrez/data/geri.csv", sep = ",")
head(test)




# ØVRIG KODE FRA UGE 3 ----

# PLOT2 ----
geri$Balance <- ifelse(geri$tandemchange > 0, "Forbedring", "Ingen forbedring")


geri %>% 
  filter(!(Balance %in% NA)) %>% 
  ggplot(aes(x=factor(1), fill=Balance)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 1) + 
  coord_polar("y", start = 0, direction = -1) + 
  theme_void() +
  facet_wrap(~ gruppe2) +   
  theme(legend.position = "bottom")


table(geri$Balance,geri$gruppe2)





# PRØVER TABLEONE PACKAGE TIL AT LAVE TABEL1
dput(names(geri))
vars <- c("Alder", "Kn", "Indlvarighed", "udskrdest1", "Tandemstart","Tandemslut", "tandemchange")

tableone <- CreateTableOne(vars = vars, strata=c("gruppe"), data=geri, test=F)
tableone

print(tableone, quote = T, noSpaces = T) # quote sætter "" om, noSpaces fjerner alignment med mellemrum (fungerer fint uden)
