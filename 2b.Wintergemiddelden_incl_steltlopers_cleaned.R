library(tidyverse)
library(INBOtheme)
# theme_set(theme_inbo(base_size = 12, transparent = FALSE))
conflicted::conflict_prefer("filter", "dplyr")

### functies opvragen
source("./Scripts/Functies watervogel grafieken.R")

##################
# 1 DATA INLEZEN
##################

WV_DB <- read.csv("./Data/WV_DB_2025-01-23.csv")
Gebieden_ZS <- read_csv("./Data/Gebieden2 herwerking Wim.csv")
### dit is de aangepaste gebieden-file doorgemaild door Wim op 31-01-2025

### check:
Gebieden_ZS %>% anti_join(WV_DB, by = c("Gebied")) %>% select(Gebied) %>% pull()
### geen mismatches: alle gebieden zitten ook in de WV_DB

### selecteren van de tellingen in de relevante telgebieden
Tellingen_ZS <- WV_DB %>% right_join(Gebieden_ZS, by = c("Gebied" = "Gebied"))

### selecteren periode 1999/00 -> 2023/24; meeuwen worden niet betrokken in de analyse
Tellingen_ZS <- Tellingen_ZS %>% 
  mutate(Telseizoen_num = as.numeric(str_sub(Telseizoen, 1, 4)),
         Telseizoen_chr = str_sub(Telseizoen, 1, 4)) %>% 
  filter(Groep != "Meeuwen en Sternen",
         Telseizoen_num > 1998 & Telseizoen_num < 2024)

Tellingen_ZS <-  Tellingen_ZS %>% 
  mutate(Gebiedsklasse = ifelse(Sigma == 1, "Sigmagebied", 
                                ifelse(Vallei == 1, "Valleigebied",
                                       if_else(Estuarien == 1, "Estuarium",
                                               if_else(NOHaven == 1, "Haven", NA)))))

### zelfde actie voor volledige WV_DB:
WV_DB <- WV_DB %>% 
  mutate(Telseizoen_num = as.numeric(str_sub(Telseizoen, 1, 4)),
         Telseizoen_chr = str_sub(Telseizoen, 1, 4)) %>% 
  filter(Groep != "Meeuwen en Sternen",
         Telseizoen_num > 1998 & Telseizoen_num < 2024)

Tellingen_ZS %>% 
  group_by(ProjectCode) %>% summarise(n())

WV_DB %>% 
  group_by(ProjectCode) %>% summarise(n())

WV_DB %>% filter(Gebied == "Poldercomplex OOSTKERKE") %>% 
  distinct(NedNaam) %>% 
  filter(str_detect(NedNaam, "gans")|str_detect(NedNaam, "Gans"))
### Geen inheemse ganzen! Dit betekent dat de Analysesetkey selectie "F.Analysesetkey in (2, 3, 4)" in 1 beweging
### de juiste soorten voor de juiste gebieden selecteert; de onderstaande filter op ProjectCode lijkt mij daarom verkeerd!!
### -> nagevraagd bij Wim
WV_DB <- WV_DB %>% filter(ProjectCode %in% c("MIDMA","ZSCH"))

### check:
nrow(WV_DB) == 764930
nrow(Tellingen_ZS) == 198217

#########################################################
# 2 Aantallen in Vlaanderen, Zeeschelde & Sigmagebieden
#########################################################
Tot_Telling_Vl <- WV_DB %>% 
  # filter(Groepscode != "S") %>% 
  group_by(Telseizoen_num, Telling) %>% 
  summarise(aantal = sum(Aantal))

Tot_Telling_ZS <- Tellingen_ZS %>% 
  # filter(Groepscode != "S") %>%   
  group_by(Telseizoen_num, Telling) %>% 
  summarise(aantal = sum(Aantal))

Tot_Telling_Sigma <- Tellingen_ZS %>% 
  filter(Gebiedsklasse == "Sigmagebied") %>%  
         # Groepscode != "S") %>%  
  group_by(Telseizoen_num, Telling) %>% 
  summarise(aantal = sum(Aantal))

Tot_Telling_summary_temp <- Tot_Telling_Sigma %>% 
  full_join(Tot_Telling_ZS, by = c("Telseizoen_num", "Telling")) %>% 
  full_join(Tot_Telling_Vl, by = c("Telseizoen_num", "Telling")) %>% 
  rename(aantal_Sigma = aantal.x,
         aantal_ZS = aantal.y,
         aantal_Vl = aantal) %>% 
  mutate(Aandeel_Sigma_ZS = aantal_Sigma / aantal_ZS,
         Aandeel_Sigma_Vl = aantal_Sigma / aantal_Vl,
         Aandeel_ZS_Vl = aantal_ZS / aantal_Vl)
Tot_Telling_summary_temp

Tot_Telling_summary <- Tot_Telling_summary_temp %>%
  group_by(Telseizoen_num) %>% 
  summarise(max_Sigma = max(aantal_Sigma), 
            mean_Sigma = mean(aantal_Sigma),
            max_ZS = max(aantal_ZS),
            mean_ZS = mean(aantal_ZS),
            max_Vl = max(aantal_Vl),
            mean_Vl = mean(aantal_Vl),
            mean_Aandeel_Sigma_ZS = mean(Aandeel_Sigma_ZS),
            mean_Aandeel_Sigma_Vl = mean(Aandeel_Sigma_Vl),
            mean_Aandeel_ZS_Vl = mean(Aandeel_ZS_Vl)) 
Tot_Telling_summary

#####################################
# 3a Grafieken: wintergemiddelden  
#####################################
ggplot(Tot_Telling_summary_temp, aes(x = Telseizoen_num, y = aantal_Sigma))  +
  geom_point() + geom_smooth() + 
  f_labs_totaal("Sigmagebieden (getelde aantallen)") 

ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = max_Sigma)) +
  geom_point() + geom_smooth() +
  f_labs_totaal("Sigmagebieden (wintermaxima)") 

b1b.Totaal_Sigma <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_Sigma)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) +
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 20000)) + 
  f_labs_totaal("Sigmagebieden: wintergemiddelden (incl. steltlopers)") + 
  theme_bw() +
  f_graph_theme()

b1b.Totaal_Sigma
f_save_graph(b1b.Totaal_Sigma)

b2b.Totaal_ZS <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_ZS)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 125000)) + 
  f_labs_totaal("Zeeschelde-vallei: wintergemiddelden (incl. steltlopers)") + 
  theme_bw() +
  f_graph_theme()

b2b.Totaal_ZS
f_save_graph(b2b.Totaal_ZS)

b3b.Totaal_VL <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_Vl)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 320000)) + 
  f_labs_totaal("Vlaanderen: wintergemiddelden (incl. steltlopers)") + 
  theme_bw() +
  f_graph_theme()

b3b.Totaal_VL
f_save_graph(b3b.Totaal_VL)


Tot_Telling_Sigma_ZS <- Tot_Telling_summary %>% pivot_longer(cols = c(2:10)) %>% filter(name %in% c("mean_Sigma","mean_ZS"))

b4b.Totaal_Sigma_ZS <- ggplot() + 
  geom_point(data = Tot_Telling_Sigma_ZS, aes(x = Telseizoen_num, y = value, colour = name), size = 0.5) + 
  geom_smooth(data = Tot_Telling_Sigma_ZS, aes(x = Telseizoen_num, y = value, colour = name, fill = name), linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 125000)) + 
  scale_colour_manual(values = c("darkgreen","orangered3"), labels = c("Sigma", "Zeeschelde-vallei"), name = NULL) +
  scale_fill_manual(values = c("darkgreen","orangered3"), labels = c("Sigma", "Zeeschelde-vallei"), name = NULL) +
  labs(title = "Sigmagebieden & Zeeschelde-vallei: wintergemiddelden\n(incl. steltlopers)", y = "Aantal watervogels", x = element_blank()) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.85)) + 
  f_graph_theme() 

b4b.Totaal_Sigma_ZS
f_save_graph(b4b.Totaal_Sigma_ZS)

Tot_Telling_ZS_VL <- Tot_Telling_summary %>% pivot_longer(cols = c(2:10)) %>% filter(name %in% c("mean_ZS","mean_Vl"))

b5b.Totaal_ZS_VL <- ggplot() + 
  geom_point(data = Tot_Telling_ZS_VL, aes(x = Telseizoen_num, y = value, colour = name), size = 0.5) + 
  geom_smooth(data = Tot_Telling_ZS_VL, aes(x = Telseizoen_num, y = value, colour = name, fill = name), linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number, limits = c(0, 320000)) + 
  scale_colour_manual(values = c("yellow4","orangered3"), labels = c("Vlaanderen", "Zeeschelde-vallei"), name = NULL) +
  scale_fill_manual(values = c("yellow4","orangered3"), labels = c("Vlaanderen", "Zeeschelde-vallei"), name = NULL) +
  labs(title = "Zeeschelde-vallei & Vlaanderen: wintergemiddelden\n(incl. steltlopers)", y = "Aantal watervogels", x = element_blank()) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.85)) + 
  f_graph_theme() 

b5b.Totaal_ZS_VL
f_save_graph(b5b.Totaal_ZS_VL)

################################################################################
# 3b Grafieken: Aandeel watervogels in Sigmagebieden tov Zeeschelde/Vlaanderen 
################################################################################
c1b.Aandeel_Sigma_ZS <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_Aandeel_Sigma_ZS)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
  scale_y_continuous(limits = c(0, 0.4), labels = scales::percent_format(scale = 100)) +
  f_labs_aandeel("Percentage watervogels in Sigmagebieden tov Zeeschelde-vallei\n(incl. steltlopers)") + 
  theme_bw() +
  f_graph_theme()

c1b.Aandeel_Sigma_ZS
f_save_graph(c1b.Aandeel_Sigma_ZS)

c2b.Aandeel_ZS_VL <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_Aandeel_ZS_Vl)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent_format(scale = 100)) +
  f_labs_aandeel("Percentage watervogels in de Zeeschelde-vallei tov Vlaanderen\n(incl. steltlopers)") + 
  theme_bw() +
  f_graph_theme()

c2b.Aandeel_ZS_VL
f_save_graph(c2b.Aandeel_ZS_VL)


c3b.Aandeel_Sigma_VL <- ggplot(Tot_Telling_summary, aes(x = Telseizoen_num, y = mean_Aandeel_Sigma_Vl)) + 
  geom_point(size = 0.5) + geom_smooth(linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) + 
  scale_y_continuous(limits = c(0, 0.1), labels = scales::percent_format(scale = 100)) +
  f_labs_aandeel("Percentage watervogels in Sigmagebieden tov Vlaanderen\n(incl. steltlopers)") + 
  theme_bw() +
  f_graph_theme()

c3b.Aandeel_Sigma_VL
f_save_graph(c3b.Aandeel_Sigma_VL)

#####################################################
# 4 Grafiek: wintergemiddelden per gebiedsklasse
#####################################################
Tot_Telling_Gebiedsklassen_temp <- Tellingen_ZS %>% 
  # filter(Groepscode != "S") %>%  
  group_by(Gebiedsklasse, Telseizoen_num, Telling) %>% 
  summarise(aantal = sum(Aantal)) 

Tot_Telling_Gebiedsklassen <- Tot_Telling_Gebiedsklassen_temp %>%
  group_by(Gebiedsklasse, Telseizoen_num) %>% 
  summarise(Wintermean = mean(aantal)) 
Tot_Telling_Gebiedsklassen

b6b.Totaal_ZS_Gebiedsklassen <- ggplot() + 
  geom_point(data = Tot_Telling_Gebiedsklassen, aes(x = Telseizoen_num, y = Wintermean, col = Gebiedsklasse), size = 0.5) + 
  geom_smooth(data = Tot_Telling_Gebiedsklassen, aes(x = Telseizoen_num, y = Wintermean, col = Gebiedsklasse, fill = Gebiedsklasse), linewidth = 0.25, alpha = 0.3) + 
  scale_x_continuous(breaks = seq(from = 1999, to = 2023, by = 2)) +
  scale_y_continuous(labels = scales::number) + 
  scale_colour_discrete(name = NULL) + 
  scale_fill_discrete(name = NULL) + 
  f_labs_totaal("Zeeschelde-vallei: wintergemiddelden\n(incl. steltlopers)") + 
  theme_bw() +
  theme(legend.position = c(0.8, 0.8)) +
  f_graph_theme()

b6b.Totaal_ZS_Gebiedsklassen
f_save_graph(b6b.Totaal_ZS_Gebiedsklassen)
