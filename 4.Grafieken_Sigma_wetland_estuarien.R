library(tidyverse)
library(INBOtheme)
library(DBI)
# theme_set(theme_inbo())
conflicted::conflict_prefer("filter", "dplyr")

### functies opvragen
source("./GitScripts/Functies_WV_grafieken.R")

#################
# 1 DATA INLEZEN
#################
WV_DB <- read_csv("./Data/WV_DB_2025-01-23.csv")
WV_DB <- WV_DB %>% rename(LocationWVNaam = Gebied) 

con <- inbodb::connect_inbo_dbase("W0004_00_Waterbirds")

Gebieden_ZS <- dbGetQuery(con,
"select LocationGroupCode
, LocationGroupNaam
, LocationGroupTypeCode
, LocationGroupTypeNaam
, LocationWVCode
, LocationWVNaam
, StartDate
, EndDate
from [dbo].[FactLocationGroup]
where 1 = 1
AND IsDeleted = 0
AND LocationGroupCode in ('ZS','ZR','ZSVAL','SEST','WETL','NOH')"
)

###########################
# 2 Wrangle Prosperpolder
###########################

Prosper_Z <- read_csv("./Data/Prosperpolder Zuid 2018-2024.csv")
Prosper_N <- read_csv("./Data/Prosperpolder Noord 2013-2024.csv")

Prosper_Z %>% distinct(family)

Prosper_Z <- Prosper_Z %>% 
  rename(LocationWVNaam = location,
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
  select(LocationWVNaam, LocationGroupNaam, Teldatum, Telseizoen_num, Telling, NedNaam, Aantal, Groep)

Prosper_N <- Prosper_N %>% 
  rename(LocationWVNaam = location,
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
  select(LocationWVNaam, LocationGroupNaam, Teldatum, Telseizoen_num, Telling, NedNaam, Aantal, Groep)

Prosper_Z %>% glimpse()
Prosper_N %>% glimpse()

Prosper_Z %>% distinct(Groep)

Prosper <- rbind(Prosper_N, Prosper_Z)
# write_csv(Prosper, "./Data/Prosper_wrangled.csv")

#########################
# 3 Wrangle gebiedsfile
#########################

Prosper %>% group_by(LocationWVNaam, LocationGroupNaam) %>% summarise()

Gebieden_ZS_wrangled <- bind_rows(Gebieden_ZS, Prosper %>% group_by(LocationWVNaam, LocationGroupNaam) %>% summarise())
Gebieden_ZS_wrangled %>% count(LocationWVNaam) %>% summary()
# write_csv(Gebieden_ZS, "./Data/Gebieden_wrangled.csv")

#########################
# 4 Wrangle WV_DB
#########################

WV_DB %>% filter(LocationWVNaam == "Prosperpolder DOEL") %>% count(LocationWVNaam, Telseizoen)
WV_DB <- WV_DB %>% mutate(Telseizoen_num = as.numeric(str_sub(Telseizoen, 1, 4)))

cbind(WV_DB %>% filter(LocationWVNaam == "Prosperpolder DOEL", Telseizoen_num > 2012) %>% 
  group_by(NedNaam) %>% 
  summarise(Sum = sum(Aantal)) %>% arrange(desc(Sum)) %>% head(n = 20),
Prosper %>% filter(NedNaam != "Canadese Gans onbekend") %>% 
  group_by(NedNaam) %>% 
  summarise(Sum = sum(Aantal)) %>% arrange(desc(Sum))%>% head(n = 20))
### toch wel wat verschil...

WV_DB_select <- WV_DB %>% 
  filter(!(LocationWVNaam == "Prosperpolder DOEL" & Telseizoen_num > 2012)) 
WV_DB_Prosper <- bind_rows(WV_DB_select, 
                           Prosper %>%
                             select(!LocationGroupNaam))
WV_DB_Prosper %>% glimpse()
WV_DB_Prosper %>% filter(LocationWVNaam == "Doel - Prosperpolder Noord") %>% glimpse()
# write_csv(WV_DB_Prosper, "./Data/WV_DB_Prosper_wrangled.csv")

#########################
# 5 Create graphs
#########################

Tellingen_ZS <- WV_DB_Prosper %>% 
  right_join(Gebieden_ZS_wrangled, by = c("LocationWVNaam" = "LocationWVNaam")) %>% 
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
Tellingen_ZS %>% filter(LocationWVNaam == "Prosperpolder DOEL") %>% distinct(LocationGroupNaam)

Tellingen_ZS_alt <- Tellingen_ZS %>% 
  mutate(LocationGroupNaam = case_when(LocationWVNaam == "Prosperpolder DOEL" ~ "Sigma-Estuarien", .default = LocationGroupNaam))
Tellingen_ZS_alt %>% filter(LocationWVNaam == "Prosperpolder DOEL") %>% distinct(LocationGroupNaam)

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
