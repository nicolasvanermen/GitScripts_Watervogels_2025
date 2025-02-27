library(tidyverse)
library(INBOtheme)
# theme_set(theme_inbo())
conflicted::conflict_prefer("filter", "dplyr")

### functies opvragen
source("./GitScripts/Functies_WV_grafieken.R")

#################
# 1 DATA INLEZEN
#################
WV_DB <- read_csv("./Data/WV_DB_2025-01-23.csv")
Gebieden_ZS <- read_csv("./Data/Gebieden2 herwerking Wim.csv")
Gebieden_WVDB <- read_csv("./Data/FctLocationGroup_info - herwerking NV.csv")

### check het voorkomen van gebiedsnamen in beide tabellen:
Gebieden_WVDB %>% count(LocationWVNaam) %>% summary()
Gebieden_WVDB %>% count(LocationGroupNaam)

Gebieden_WVDB_rev <- Gebieden_WVDB %>% filter(!LocationGroupNaam %in% c("Linkeroever","Rechteroever"))
Gebieden_WVDB_rev %>% count(LocationWVNaam) %>% summary()

Gebieden_ZS %>% count(Gebied) %>% summary()

Gebieden_ZS %>% filter(!Gebied %in% Gebieden_WVDB_rev$LocationWVNaam) %>% pull(Gebied) %>% sort()
Gebieden_WVDB_rev %>% filter(!LocationWVNaam %in% Gebieden_ZS$Gebied) %>% pull(LocationWVNaam) %>% sort()

### een gelijkaardige bewerking kan ook via anti_join, nu tussen gebiedstabellen en WV_DB:
Gebieden_ZS %>% anti_join(WV_DB, by = c("Gebied")) %>% 
  select(Gebied) %>% pull()

Gebieden_WVDB_rev %>% anti_join(WV_DB, by = c("LocationWVNaam" = "Gebied")) %>% 
  select(LocationWVNaam) %>% pull()

###########################
# 2 Wrangle Prosperpolder
###########################

Prosper_Z <- read_csv("./Data/Prosperpolder Zuid 2018-2024.csv")
Prosper_N <- read_csv("./Data/Prosperpolder Noord 2013-2024.csv")

Prosper_Z %>% distinct(family)

Prosper_Z <- Prosper_Z %>% 
  rename(Gebied = location,
         NedNaam = `species name`,
         Teldatum = date,
         Aantal = number,
         Groep = family) %>%
  mutate(Telseizoen_num = year(Teldatum),
         Telling = as.character(month(Teldatum)),
         Telling = case_match(Telling, 
                              "10" ~ "Oktober",
                              "11" ~ "November",
                              "12" ~ "December",
                              "1" ~ "Januari",
                              "2" ~ "Februari",
                              "3" ~ "Maart", .default = NA),
         Groep = case_match(Groep, 
                              "Meeuwen, Sterns en Schaarbekken (Laridae)" ~ "Meeuwen en Sternen", .default = Groep),
         LocationGroupNaam = "Sigma-Wetland") %>% 
  select(Gebied, LocationGroupNaam, Teldatum, Telseizoen_num, Telling, NedNaam, Aantal, Groep)

Prosper_N <- Prosper_N %>% 
  rename(Gebied = location,
         NedNaam = `species name`,
         Teldatum = date,
         Aantal = number,
         Groep = family) %>%
  mutate(Telseizoen_num = year(Teldatum),
         Telling = as.character(month(Teldatum)),
         Telling = case_match(Telling, 
                              "10" ~ "Oktober",
                              "11" ~ "November",
                              "12" ~ "December",
                              "1" ~ "Januari",
                              "2" ~ "Februari",
                              "3" ~ "Maart", .default = NA),
         Groep = case_match(Groep, 
                            "Meeuwen, Sterns en Schaarbekken (Laridae)" ~ "Meeuwen en Sternen", .default = Groep),
         LocationGroupNaam = "Sigma-Estuarien") %>% 
  select(Gebied, LocationGroupNaam, Teldatum, Telseizoen_num, Telling, NedNaam, Aantal, Groep)

Prosper_Z %>% glimpse()
Prosper_N %>% glimpse()

Prosper_Z %>% distinct(Groep)

Prosper <- rbind(Prosper_N, Prosper_Z)
# write_csv(Prosper, "./Data/Prosper_wrangled.csv")

#########################
# 3 Wrangle gebiedsfile
#########################
Gebieden_ZS <- left_join(Gebieden_ZS, Gebieden_WVDB_rev %>% select(LocationWVNaam, LocationGroupNaam),
                         by = c("Gebied" = "LocationWVNaam"))

### checks:
Gebieden_ZS %>% distinct(LocationGroupNaam)
Gebieden_ZS %>% filter(Sigma == 1) %>% distinct(LocationGroupNaam)
Gebieden_ZS %>% mutate(test = str_detect(LocationGroupNaam, "Sigma")) %>% filter(test == TRUE) %>% distinct(Sigma)

Prosper %>% group_by(Gebied, LocationGroupNaam) %>% summarise()

Gebieden_ZS_wrangled <- bind_rows(Gebieden_ZS, Prosper %>% group_by(Gebied, LocationGroupNaam) %>% summarise())
Gebieden_ZS_wrangled %>% count(Gebied) %>% summary()
# write_csv(Gebieden_ZS, "./Data/Gebieden_wrangled.csv")

# #testjes rond count / n / length
# Gebieden_ZS %>% count(Cluster)
# Gebieden_ZS %>% group_by(Cluster) %>% summarise(n = n())
# Gebieden_ZS %>% summarise(n = n(), .by = Cluster)
# #wat ook werkt maar het allerminst elegant is:
# Gebieden_ZS %>% group_by(Cluster) %>% summarise(n = length(Cluster))


#########################
# 4 Wrangle WV_DB
#########################
WV_DB %>% filter(Gebied == "Prosperpolder DOEL") %>% count(Gebied, Telseizoen)
WV_DB <- WV_DB %>% mutate(Telseizoen_num = as.numeric(str_sub(Telseizoen, 1, 4)))

cbind(WV_DB %>% filter(Gebied == "Prosperpolder DOEL", Telseizoen_num > 2012) %>% 
  group_by(NedNaam) %>% 
  summarise(Sum = sum(Aantal)) %>% arrange(desc(Sum)) %>% head(n = 20),
Prosper %>% filter(NedNaam != "Canadese Gans onbekend") %>% 
  group_by(NedNaam) %>% 
  summarise(Sum = sum(Aantal)) %>% arrange(desc(Sum))%>% head(n = 20))
### toch wel wat verschil...

WV_DB_select <- WV_DB %>% filter(!(Gebied == "Prosperpolder DOEL" & Telseizoen_num > 2012))
WV_DB_Prosper <- bind_rows(WV_DB_select, 
                           Prosper %>%
                             select(!LocationGroupNaam))
WV_DB_Prosper %>% glimpse()
WV_DB_Prosper %>% filter(Gebied == "Doel - Prosperpolder Noord") %>% glimpse()
# write_csv(WV_DB_Prosper, "./Data/WV_DB_Prosper_wrangled.csv")

#########################
# 5 Create graphs
#########################

Tellingen_ZS <- WV_DB_Prosper %>% 
  right_join(Gebieden_ZS_wrangled, by = c("Gebied" = "Gebied")) %>% 
  filter(Groep != "Meeuwen en Sternen",
         Telseizoen_num > 1998 & Telseizoen_num < 2024)

Tot_Telling_Sigma <- Tellingen_ZS %>% 
  filter(LocationGroupNaam %in% c("Sigma-Estuarien", "Sigma-Wetland")) %>%
  group_by(Telseizoen_num, LocationGroupNaam) %>% 
  summarise(aantal = sum(Aantal)/6)

b1c.Totaal_Sigma <- ggplot() + 
  geom_point(data = Tot_Telling_Sigma, aes(x = Telseizoen_num, y = aantal, colour = LocationGroupNaam), size = 0.5) + 
  geom_smooth(data = Tot_Telling_Sigma, aes(x = Telseizoen_num, y = aantal, colour = LocationGroupNaam, fill = LocationGroupNaam), linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 15000)) + 
  scale_colour_discrete(name = NULL) + 
  scale_fill_discrete(name = NULL) + 
  labs(title = "Sigmagebieden: wintergemiddelden (incl. steltlopers)", y = "Aantal watervogels", x = element_blank()) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.85)) + 
  f_graph_theme() 

b1c.Totaal_Sigma
f_save_graph(b1c.Totaal_Sigma)

### Prosperpolder DOEL omzetten naar Sigma - estuarien ipv wetland
Tellingen_ZS %>% filter(Gebied == "Prosperpolder DOEL") %>% distinct(LocationGroupNaam)

Tellingen_ZS_alt <- Tellingen_ZS %>% 
  mutate(LocationGroupNaam = case_when(Gebied == "Prosperpolder DOEL" ~ "Sigma-Estuarien", .default = LocationGroupNaam))
Tellingen_ZS_alt %>% filter(Gebied == "Prosperpolder DOEL") %>% distinct(LocationGroupNaam)

Tellingen_ZS_alt %>% count(LocationGroupNaam)
Tellingen_ZS %>% count(LocationGroupNaam)

Tot_Telling_Sigma_alt <- Tellingen_ZS_alt %>% 
  filter(LocationGroupNaam %in% c("Sigma-Estuarien", "Sigma-Wetland")) %>%
  group_by(Telseizoen_num, LocationGroupNaam) %>% 
  summarise(aantal = sum(Aantal)/6)

b1d.Totaal_Sigma <- ggplot() + 
  geom_point(data = Tot_Telling_Sigma_alt, aes(x = Telseizoen_num, y = aantal, colour = LocationGroupNaam), size = 0.5) + 
  geom_smooth(data = Tot_Telling_Sigma_alt, aes(x = Telseizoen_num, y = aantal, colour = LocationGroupNaam, fill = LocationGroupNaam), linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 15000)) + 
  scale_colour_discrete(name = NULL) + 
  scale_fill_discrete(name = NULL) + 
  labs(title = "Sigmagebieden: wintergemiddelden (incl. steltlopers)", y = "Aantal watervogels", x = element_blank()) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.85)) + 
  f_graph_theme() 

b1d.Totaal_Sigma
f_save_graph(b1d.Totaal_Sigma)
